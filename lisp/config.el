;;; config.el --- Configuration for dotJS 2025 conference

;; Author: Aidan Pace <apace@defrecord.com>
;; Keywords: org-mode, conference, notes
;; Version: 0.1.0

;;; Commentary:
;; This file contains configuration variables for the dotJS 2025 conference notes.

;;; Code:

;; Project paths
(defvar dotjs-project-root (expand-file-name "~/projects/aygp-dr/dotjs-2025")
  "Root directory of the dotJS 2025 project.")

;; Bibliography configuration
(setq org-cite-global-bibliography 
      `(,(expand-file-name "resources/bibliography/references.bib" dotjs-project-root)))

;; Org-mode configuration
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (js . t)
     (python . t)
     (scheme . t)
     (shell . t)
     (mermaid . t))))

;; Development environment versions
(defconst dotjs-tool-versions
  '((node . "v18.19.0")
    (npm . "9.2.0")
    (python . "3.11.2")
    (poetry . "2.1.1")
    (guile . "3.0.8")
    (emacs . "31.0.50"))
  "Tool versions used in the project.")

;; Load specific project settings
(defun dotjs-setup ()
  "Set up the dotJS project environment."
  (interactive)
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-export-with-smart-quotes t)
  (setq org-export-with-toc t)
  (message "dotJS 2025 environment configured!"))

;; Provide this module
(provide 'config)
;;; config.el ends here