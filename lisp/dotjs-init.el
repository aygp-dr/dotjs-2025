;;; dotjs-init.el --- Initialization for dotJS 2025 note-taking project

;; Author: Aidan Pace <apace@defrecord.com>
;; Keywords: org-mode, conference, notes
;; Version: 0.1.0

;;; Commentary:
;; This file loads all the necessary Elisp modules for working with
;; dotJS 2025 conference notes.

;;; Code:

;; Add this directory to load path
(add-to-list 'load-path (file-name-directory load-file-name))

;; Load project files
(require 'config)
(require 'generate-templates)
(require 'org-capture-templates)

;; Define project-specific functions
(defun dotjs-setup-project ()
  "Set up the dotJS 2025 project by loading all necessary components."
  (interactive)
  (message "Setting up dotJS 2025 project environment...")
  (load-library "org-capture-templates")
  (message "Capture templates loaded."))

(defun dotjs-tangle-file (file)
  "Tangle the specified org FILE."
  (interactive "fOrg file to tangle: ")
  (with-current-buffer (find-file-noselect file)
    (org-babel-tangle)
    (message "Tangled %s" file)))

(defun dotjs-detangle-file (file)
  "Detangle the specified org FILE - sync from source back to org file."
  (interactive "fOrg file to detangle: ")
  (with-current-buffer (find-file-noselect file)
    (org-babel-detangle)
    (message "Detangled %s" file)))

(defun dotjs-tangle-all-files ()
  "Tangle all org files in the project."
  (interactive)
  (let ((files (directory-files-recursively "~/projects/aygp-dr/dotjs-2025" "\\.org$")))
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
        (org-babel-tangle)))
    (message "Tangled %d files" (length files))))

(defun dotjs-html-export ()
  "Export all org files to HTML."
  (interactive)
  (let ((files (directory-files-recursively "~/projects/aygp-dr/dotjs-2025" "\\.org$")))
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
        (org-html-export-to-html)))
    (message "Exported %d files to HTML" (length files))))

(defun dotjs-setup-bibliography ()
  "Set up the bibliography for citation export."
  (interactive)
  (setq org-cite-global-bibliography 
        '("~/projects/aygp-dr/dotjs-2025/resources/bibliography/references.bib"))
  (message "Bibliography configuration updated."))

;; Load the export functionality
(load-library "export-all-org-files")

(provide 'dotjs-init)
;;; dotjs-init.el ends here