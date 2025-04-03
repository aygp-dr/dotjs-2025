;;; export-all-org-files.el --- Export utilities for dotJS 2025

;; Author: Aidan Pace <apace@defrecord.com>
;; Keywords: org-mode, export, conference, notes
;; Version: 0.1.0

;;; Commentary:
;; This file provides functions for exporting org files in the dotJS 2025
;; conference notes repository.

;;; Code:

(require 'org)
(require 'config)

(defun export-all-org-files ()
  "Export all org files in the project to HTML."
  (interactive)
  (dolist (file (directory-files-recursively dotjs-project-root "\\.org$"))
    (with-current-buffer (find-file-noselect file)
      (org-html-export-to-html))))

(defun export-all-org-files-to-pdf ()
  "Export all org files in the project to PDF."
  (interactive)
  (dolist (file (directory-files-recursively dotjs-project-root "\\.org$"))
    (with-current-buffer (find-file-noselect file)
      (org-latex-export-to-pdf))))

(provide 'export-all-org-files)
;;; export-all-org-files.el ends here
