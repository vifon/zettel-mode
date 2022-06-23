;;; zettel-mode.el --- A Zettelkasten-style note-taking helper     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Wojciech Siewierski

;; Author: Wojciech Siewierski
;; Keywords: outlines, org-mode, convenience
;; Version: 1.0
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

(defcustom zettel-sidebar-update-delay 0.5
  "The time in seconds to wait before updating the sidebar on current buffer change."
  :type 'float)

(defcustom zettel-slug-format "%Y%m%d%H%M%S_"
  "The prefix used for the filenames of created files."
  :type 'string)


(defun zettel-get-files ()
  (directory-files default-directory nil "\\.org\\'"))

(defun zettel-absolute-filename (name)
  (concat (file-name-as-directory default-directory)
          (downcase
           (replace-regexp-in-string "[[:space:]/]+" "-" name))
          ".org"))

(defun zettel-get-backrefs (target-file)
  "Get the links to other files referencing TARGET-FILE."
  (sort
   (mapcan
    (lambda (file)
      (with-current-buffer (find-file-noselect file)
        (when-let ((link (org-element-map
                          (org-element-parse-buffer) 'link
                          ;; Find the first link to target-file.
                          (lambda (link)
                            (equal target-file
                                   (org-element-property :path link)))
                          nil t)))
          ;; If this file links to the target-file, return its name
          ;; and title.  `zettel-get-org-title' cannot be called in
          ;; the lambda above as `org-element-map' doesn't operate in
          ;; the context of the whole file, that's why we do it here.
          (let ((org-title (zettel-get-org-title)))
            (list (list file
                        org-title))))))
    (zettel-get-files))
   (lambda (x y) (string< (car x)
                          (car y)))))

(defun zettel-get-refs (target-file)
  "Get the links to other files referenced from TARGET-FILE."
  (with-current-buffer (find-file-noselect target-file)
    (sort
     (mapcar
      (lambda (file)
        (list file (zettel-get-org-title file)))
      (cl-intersection
       (zettel-get-files)
       (delete-dups
        (org-element-map (org-element-parse-buffer) 'link
          (lambda (link)
            (let ((path (org-element-property :path link))
                  (type (org-element-property :type link)))
              (when (equal type "file")
                path)))))
       :test #'equal))
     (lambda (x y) (string< (cadr x)
                            (cadr y))))))

(defun zettel-get-external-refs (target-file)
  "Get the external links referenced from TARGET-FILE.

Return all the links other than the ones to the other files."
  (with-current-buffer (find-file-noselect target-file)
    (mapcar
     (lambda (link)
       (list (plist-get link :raw)
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
       (zettel-get-files)
       :test (lambda (a b) (equal (plist-get a :path)
                                  b)))
      (lambda (a b)
        (string< (plist-get a :text)
                 (plist-get b :text)))))))

(defun zettel-get-org-title (&optional file)
  "Get the `org-mode' title of FILE or the current file if FILE is nil."
  (with-current-buffer (if file
                           (find-file-noselect file)
                         (current-buffer))
    (when-let ((title-prop (plist-get
                            (org-export-get-environment)
                            :title)))
      (substring-no-properties (car title-prop)))))

(defun zettel-insert-refs-using (ref-function target-file depth &optional listed)
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
          (title (file-name-base (cadr file-data))))
      (org-insert-link nil link title))
    (insert "\n")
    (when (< depth zettel-sidebar-max-depth)
      (zettel-insert-refs-using ref-function
                                 (car file-data)
                                 (1+ depth)
                                 (cons target-file listed)))))

(defun zettel-get-all (ref-func)
  "Compute an alist of all files' references using a given REF-FUNC.

REF-FUNC should be a function such as `zettel-get-backrefs' or
`zettel-get-refs', accepting a target-file as its argument,
target-file being a member of `zettel-get-files'."
  (mapcar
   (lambda (target-file)
     (cons target-file
           (funcall ref-func target-file)))
   (zettel-get-files)))

(defun zettel-cached (func cache-name)
  "Either call FUNC or retrieve its result from cache associated with the CACHE-NAME symbol.

FUNC should be a function such as `zettel-get-backrefs' or
`zettel-get-refs', accepting a target-file."
  (lambda (target-file)
    (if-let ((cache-file (concat (file-name-as-directory default-directory)
                                 ".cache/"
                                 (symbol-name cache-name)
                                 ".el"))
             (backref-cache (with-temp-buffer
                              (when (file-exists-p cache-file)
                                (insert-file-contents cache-file)
                                (goto-char (point-min))
                                (read (current-buffer)))))
             (cached (assoc target-file backref-cache)))
        (cdr cached)
      (funcall func target-file))))

(defun zettel-insert-backrefs (target-file)
  (insert "* Backrefs\n\n")
  (zettel-insert-refs-using
   (zettel-cached #'zettel-get-backrefs 'backrefs)
   target-file 0))

(defun zettel-insert-refs (target-file)
  (insert "\n* References\n\n")
  (zettel-insert-refs-using
   (zettel-cached #'zettel-get-refs 'refs)
   target-file 0))

(defun zettel-insert-external-refs (target-file)
  (when-let ((links (zettel-get-external-refs target-file)))
    (insert "\n* External\n\n")
    (dolist (link links)
      (insert "- ")
      (let ((link (car link))
            (title (cadr link)))
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
         (zettel-directory default-directory)
         (buffer (get-buffer-create zettel-sidebar-buffer)))
    (display-buffer-in-side-window buffer
                                   '((side . right)))
    (with-current-buffer buffer
      (setq default-directory zettel-directory)
      (read-only-mode 1)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (zettel-sidebar-mode)
        (insert
         (with-temp-buffer
           (zettel-insert-backrefs target-file)
           (zettel-insert-refs target-file)
           (zettel-insert-external-refs target-file)
           (buffer-string)))
        (goto-char (point-min))))))

(defvar zettel--last-buffer nil
  "The last buffer the sidebar was generated for.

Used to detect the change of buffer.")

(defun zettel-update-hook ()
  (let ((current-buffer (current-buffer)))
    (unless (and (eq zettel--last-buffer current-buffer)
                 (get-buffer-window zettel-sidebar-buffer))
      (run-at-time (when (get-buffer-window zettel-sidebar-buffer)
                     zettel-sidebar-update-delay)
                   nil
                   (lambda ()
                     (when (eq current-buffer (current-buffer))
                       (zettel-sidebar)
                       (setq zettel--last-buffer current-buffer)))))))


(defun zettel-unique-name (name)
  "Add a unique ID to NAME."
  (concat (format-time-string zettel-slug-format)
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
                                       (zettel-get-files)
                                       nil nil
                                       text)))
              (file-name (zettel-absolute-filename title)))
         (list text title file-name))
     (let* ((title (file-name-sans-extension
                    (completing-read "File title: "
                                     (zettel-get-files))))
            (file-name (zettel-absolute-filename title))
            (text (read-from-minibuffer "Description: "
                                        (when (file-exists-p file-name)
                                          (zettel-get-org-title file-name)))))
       (list text title file-name))))
  (let* ((file-name (if (file-exists-p file-name)
                        file-name
                      (zettel-absolute-filename
                        (zettel-unique-name
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

With a prefix ARG this behavior is inverted."
  (interactive "P")
  (if (or (and (region-active-p)
               (not arg))
          (and (not (region-active-p))
               arg))
      (call-interactively #'zettel-insert-note)
    (let ((current-prefix-arg nil))
      (call-interactively #'org-insert-link))))


(defvar zettel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap org-insert-link] #'zettel-insert-link)
    (define-key map (kbd "M-n") #'org-next-link)
    (define-key map (kbd "M-p") #'org-previous-link)
    (define-key map (kbd "C-c C-M-g") #'zettel-sidebar)
    map))

(define-derived-mode zettel-mode org-mode "Zettel"
  "A mode for Zettelkasten-style note-taking based on `org-mode'.

Provides a quicker to use version of `org-insert-link' and
a sidebar outlining the file's relationship with other files."
  (add-hook 'post-command-hook #'zettel-update-hook nil t))

(unless (bound-and-true-p zettel-mode-no-deft)
  (add-to-list 'auto-mode-alist '("/\\.deft/[^/]+\\.org\\'" . zettel-mode)))


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
