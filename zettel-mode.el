;;; zettel-mode.el --- A Zettelkasten-style note-taking helper     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Wojciech Siewierski

;; Author: Wojciech Siewierski
;; Keywords: outlines, org-mode, convenience
;; Version: 0.9
;; Package-Requires: ((emacs "26.1") (org "9.3") (deft "0.8"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; An Emacs mode for Zettelkasten-style note-taking.
;; Heavily inspired by org-roam.

;;; Code:

(require 'cl-lib)
(require 'deft)
(require 'subr-x)
(require 'org)

(defgroup zettel-mode nil
  "A mode for Zettelkasten-style note-taking."
  :group 'outlines)

(defcustom zettel-link-text-prefix "§ "
  "A prefix for titles of links between notes."
  :type 'string)

(defcustom zettel-sidebar-max-depth 0
  "Maximum depth of the references lists in the sidebar."
  :type 'integer)


(defun zettel--get-backrefs (target-file)
  "Get the links to other deft-managed files referencing TARGET-FILE."
  (sort
   (mapcan
    (lambda (file)
      (with-current-buffer (find-file-noselect file)
        (let ((link (org-element-map (org-element-parse-buffer) 'link
                      ;; Find the first link to target-file.
                      (lambda (link)
                        (equal target-file
                               (org-element-property :path link)))
                      nil t)))
          (when link
            ;; If this file links to the target-file, return its name
            ;; and title.  `org-export-get-environment' cannot be
            ;; called in the lambda above as it doesn't operate in the
            ;; context of the whole file, that's why we do it here.
            (let ((org-title (car (plist-get
                                   (org-export-get-environment)
                                   :title))))
              (list (cons file
                          org-title)))))))
    (deft-find-all-files-no-prefix))
   (lambda (x y) (string< (car x)
                          (car y)))))

(defun zettel--get-refs (target-file)
  "Get the links to other deft-managed files referenced from TARGET-FILE."
  (with-current-buffer (find-file-noselect target-file)
    (sort
     (mapcar
      (lambda (file)
        (cons file
              (with-current-buffer (find-file-noselect file)
                (car (plist-get
                      (org-export-get-environment)
                      :title)))))
      (cl-intersection
       (deft-find-all-files-no-prefix)
       (delete-dups
        (org-element-map (org-element-parse-buffer) 'link
          (lambda (link)
            (let ((path (org-element-property :path link))
                  (type (org-element-property :type link)))
              (when (equal type "file")
                path)))))
       :test #'equal))
     (lambda (x y) (string< (cdr x)
                            (cdr y))))))

(defun zettel--get-external-refs (target-file)
  "Get the external links referenced from TARGET-FILE.

Return all the links other than the ones to the other
deft-managed files."
  (with-current-buffer (find-file-noselect target-file)
    (mapcar
     (lambda (link)
       (cons (plist-get link :raw)
             (plist-get link :text)))
     (sort
      (cl-set-difference
       (delete-dups
        (org-element-map (org-element-parse-buffer) 'link
          (lambda (link)
            (let ((path (org-element-property :path link))
                  (type (org-element-property :type link))
                  (raw (org-element-property :raw-link link))
                  (text (org-element-contents link)))
              (list :raw raw
                    :text (if text
                              (substring-no-properties
                               (car text))
                            raw)
                    :path (when (equal type "file")
                            path))))))
       (deft-find-all-files-no-prefix)
       :test (lambda (a b) (equal (plist-get a :path)
                                  b)))
      (lambda (a b)
        (string< (plist-get a :text)
                 (plist-get b :text)))))))

(defun zettel--insert-refs-using (ref-function target-file depth &optional listed)
  "Insert a sidebar section.

Insert the `org-mode' links found using REF-FUNCTION on
TARGET-FILE recursively until `zettel-sidebar-max-depth' is
reached.  DEPTH is used to track the current recursion level,
initially 0.  LISTED holds the files already listed in a given
subtree to avoid duplicates and cycles."
  (dolist (file-data
           (cl-nset-difference (funcall ref-function target-file)
                               listed
                               :test (lambda (x y) (equal (car x) y))))
    (insert (make-string (* 2 depth)
                         ?\ )
            "- ")
    (let ((link (concat "file:" (car file-data)))
          (title (file-name-base (cdr file-data))))
      (org-insert-link nil link title))
    (insert "\n")
    (when (< depth zettel-sidebar-max-depth)
      (zettel--insert-refs-using ref-function
                                 (car file-data)
                                 (1+ depth)
                                 (cons target-file listed)))))


(defun zettel--insert-backrefs (target-file)
  (insert "* Backrefs\n\n")
  (zettel--insert-refs-using #'zettel--get-backrefs target-file 0))

(defun zettel--insert-refs (target-file)
  (insert "\n* References\n\n")
  (zettel--insert-refs-using #'zettel--get-refs target-file 0))

(defun zettel--insert-external-refs (target-file)
  (when-let ((links (zettel--get-external-refs target-file)))
    (insert "\n* External\n\n")
    (dolist (link (zettel--get-external-refs target-file))
      (insert "- ")
      (let ((link (car link))
            (title (cdr link)))
        (org-insert-link nil link title))
      (insert "\n"))))


(defvar zettel-sidebar-buffer "*zettel-sidebar*")

(defun zettel-sidebar (&optional depth)
  "Show or refresh the sidebar with the lists of references.

DEPTH overrides `zettel-sidebar-max-depth' temporarily."
  (interactive "P")
  (let* ((zettel-sidebar-max-depth (or depth
                                       zettel-sidebar-max-depth))
         (target-file (file-name-nondirectory (buffer-file-name)))
         (buffer (get-buffer-create zettel-sidebar-buffer)))
    (display-buffer-in-side-window buffer
                                   '((side . right)))
    (with-current-buffer buffer
      (read-only-mode 1)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (zettel-sidebar-mode)
        (zettel--insert-backrefs target-file)
        (zettel--insert-refs target-file)
        (zettel--insert-external-refs target-file)
        (goto-char (point-min))))))

(defvar zettel--last-buffer nil
  "The last buffer the sidebar was generated for.

Used to detect the change of buffer.")

(defun zettel-update-hook ()
  (unless (and (eq zettel--last-buffer (current-buffer))
               (get-buffer-window zettel-sidebar-buffer))
    (zettel-sidebar)
    (setq zettel--last-buffer (current-buffer))))


(defun zettel--unique-name (name)
  "Add a unique ID to NAME."
  (concat (format-time-string "%Y%m%d%H%M%S_")
          name))

(defun zettel-insert-note (text title file-name)
  "Insert a link to another org file, possibly creating a new file.

If FILE-NAME doesn't exist, create it and use TITLE as its
title (\"#+TITLE\").  Afterwards the file is displayed in the
other window.  Regardless of the file's previous existence a link
to FILE-NAME with TEXT as its description is left at the
original point.

In interactive uses, if the region is active, use the selected
text.  Otherwise interactively ask for a file to link to."
  (interactive
   (if (region-active-p)
       (let* ((text (buffer-substring-no-properties
                     (point) (mark)))
              (title (file-name-sans-extension
                      (completing-read "File title: "
                                       (deft-find-all-files-no-prefix)
                                       nil nil
                                       text)))
              (file-name (deft-absolute-filename title)))
         (list text title file-name))
     (let* ((title (file-name-sans-extension
                    (completing-read "File title: "
                                     (deft-find-all-files-no-prefix))))
            (file-name (deft-absolute-filename title))
            (text (read-from-minibuffer "Description: ")))
       (list text title file-name))))
  (let* ((file-name (if (file-exists-p file-name)
                        file-name
                      (deft-absolute-filename
                        (zettel--unique-name
                         (file-name-nondirectory
                          (file-name-sans-extension
                           file-name))))))
         (link (concat "file:" file-name)))
    (org-insert-link nil
                     link
                     (concat zettel-link-text-prefix
                             text))
    (unless (file-exists-p file-name)
      (find-file-other-window file-name)
      (with-current-buffer (get-file-buffer file-name)
        (insert "#+TITLE: " title "\n\n")
        (goto-char (point-max)))
      (deft-cache-update-file file-name)
      (deft-refresh-filter))
    (zettel-sidebar)))

(defun zettel-insert-link (&optional arg)
  "Call `zettel-insert-note' if region is active, otherwise call `org-insert-link'.

With a prefix ARG always call `org-insert-link'"
  (interactive "P")
  (if (and (region-active-p)
           (not arg))
      (call-interactively #'zettel-insert-note)
    (let ((current-prefix-arg nil))
      (call-interactively #'org-insert-link))))


(defvar zettel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap org-insert-link] #'zettel-insert-link)
    (define-key map (kbd "M-n") #'org-next-link)
    (define-key map (kbd "M-p") #'org-previous-link)
    map))

(define-derived-mode zettel-mode org-mode "Zettel"
  "A mode for Zettelkasten-style note-taking based on `org-mode'.

Provides a quicker to use version of `org-insert-link' and
a sidebar outlining the file's relationship with other files."
  (add-hook 'post-command-hook #'zettel-update-hook nil t))

(add-to-list 'auto-mode-alist '("/\\.deft/.*\\.org\\'" . zettel-mode))


(defvar zettel-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'org-next-link)
    (define-key map (kbd "p") #'org-previous-link)
    (define-key map (kbd "g") (lambda (arg)
                                (interactive "P")
                                (with-current-buffer zettel--last-buffer
                                  (zettel-sidebar arg))))
    map))

(define-derived-mode zettel-sidebar-mode org-mode "Zettel-sidebar"
  "A specialized mode for the `zettel-mode' sidebar with lists of references."
  (setq-local org-return-follows-link t)
  (setq-local org-cycle-include-plain-lists 'integrate))


(provide 'zettel-mode)
;;; zettel-mode.el ends here
