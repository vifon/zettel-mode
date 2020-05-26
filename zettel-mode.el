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

(defgroup zettel-mode nil
  "A mode for Zettelkasten-style note-taking."
  :group 'outlines)

(defcustom zettel-link-text-prefix "§ "
  "A prefix for titles of links between notes."
  :type 'string)

(defcustom zettel-backref-max-depth 0
  "Maximum depth of the backreference search."
  :type 'integer)


(defun zettel--get-backrefs (target-file)
  "Get the links to other deft-managed files referencing `target-file'."
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
   (deft-find-all-files-no-prefix)))

(defun zettel--get-refs (target-file)
  "Get the links to other deft-managed files referenced from `target-file'."
  (with-current-buffer (find-file-noselect target-file)
    (mapcar
     (lambda (file)
       (cons file
             (with-current-buffer (find-file-noselect file)
               (car (plist-get
                     (org-export-get-environment)
                     :title)))))
     (sort
      (cl-intersection
       (deft-find-all-files-no-prefix)
       (delete-dups
        (org-element-map (org-element-parse-buffer) 'link
          (lambda (link)
            (let ((path (org-element-property :path link))
                  (type (org-element-property :type link)))
              (when (equal type "file")
                path)))))
       :test #'equal)
      #'string<))))

(defun zettel--insert-refs-using (ref-function target-file depth &optional listed)
  "Insert the org-mode links found using `ref-function' on
`target-file' recursively until `zettel-backref-max-depth' is
reached.  `depth' is used to track the current recursion level,
initially 0.  `listed' holds the files already listed in a given
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
    (when (< depth zettel-backref-max-depth)
      (zettel--insert-refs-using ref-function
                                 (car file-data)
                                 (1+ depth)
                                 (cons target-file listed)))))


(defun zettel--insert-backrefs (target-file)
  (zettel--insert-refs-using #'zettel--get-backrefs target-file 0))

(defun zettel--insert-refs (target-file)
  (zettel--insert-refs-using #'zettel--get-refs target-file 0))


(defvar zettel-backrefs-buffer "*zettel-backrefs*")

(defun zettel-sidebar ()
  "Show or refresh the sidebar with the lists of references."
  (interactive)
  (let* ((target-file (file-name-nondirectory (buffer-file-name)))
         (buffer (get-buffer-create zettel-backrefs-buffer)))
    (display-buffer-in-side-window buffer
                                   '((side . right)))
    (with-current-buffer buffer
      (read-only-mode 1)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (zettel-backrefs-mode)
        (insert "* Backrefs\n\n")
        (zettel--insert-backrefs target-file)
        (insert "\n* References\n\n")
        (zettel--insert-refs target-file)
        (goto-char (point-min))))))

(defvar zettel--last-buffer nil
  "The last buffer that got its backrefs listed.

Used to detect the change of buffer.")

(defun zettel-update-hook ()
  (unless (and (eq zettel--last-buffer (current-buffer))
               (get-buffer-window zettel-backrefs-buffer))
    (zettel-sidebar)
    (setq zettel--last-buffer (current-buffer))))


(defun zettel-insert-note (name)
  "Insert a link to another org file, possibly creating a new file.

If the region is active, use the selected text.
Otherwise interactively ask for a file to link to.

If the file doesn't exist, create it, add a title to it and focus
it in a new window.  Otherwise just display it in the
other window."
  (interactive
   (list (if (region-active-p)
             (buffer-substring-no-properties
              (point) (mark))
           (file-name-sans-extension
            (completing-read "Target: "
                             (deft-find-all-files-no-prefix))))))
  (let* ((file-name (if (region-active-p)
                        (completing-read "File name: "
                                         (deft-find-all-files-no-prefix)
                                         nil nil
                                         name)
                      name))
         (abs-file-name (deft-absolute-filename
                          (concat (format-time-string "%Y%m%d%H%M_")
                                  file-name)))
         (title (file-name-sans-extension name))
         (link (concat "file:" abs-file-name)))
    (org-insert-link nil
                     link
                     (concat zettel-link-text-prefix
                             title))
    (if (file-exists-p abs-file-name)
        (display-buffer (find-file-noselect abs-file-name))
      (find-file-other-window abs-file-name)
      (with-current-buffer (get-file-buffer abs-file-name)
        (insert "#+TITLE: " title "\n\n")
        (goto-char (point-max))))))

(defun zettel-insert-link ()
  "Call `zettel-insert-note' if region is active, otherwise call `org-insert-link'."
  (interactive)
  (if (region-active-p)
      (call-interactively #'zettel-insert-note)
    (call-interactively #'org-insert-link)))


(defvar zettel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap org-insert-link] #'zettel-insert-link)
    map))

(define-derived-mode zettel-mode org-mode "Zettel"
  "A mode for Zettelkasten-style note-taking based on `org-mode'.

Provides a quicker to use version of `org-insert-link' and
a sidebar outlining the file's relationship with other files."
  (add-hook 'post-command-hook #'zettel-update-hook nil t))

(add-to-list 'auto-mode-alist '("/\\.deft/.*\\.org\\'" . zettel-mode))


(defvar zettel-backrefs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'org-next-link)
    (define-key map (kbd "p") #'org-previous-link)
    map))

(define-derived-mode zettel-backrefs-mode org-mode "Zettel-backref"
  "A specialized mode for the `zettel-mode' backreferences list."
  (setq-local org-return-follows-link t
              org-cycle-include-plain-lists 'integrate))


(provide 'zettel-mode)
;;; zettel-mode.el ends here
