;;; config.el --- Configuration for dotJS 2025 conference

;; Author: Aidan Pace <apace@defrecord.com>
;; Keywords: org-mode, conference, notes
;; Version: 0.1.0

;;; Commentary:
;; This file contains configuration variables for the dotJS 2025 conference notes.
;; Used by .dir-locals.el for project-specific settings.

;;; Code:

;; Development environment versions
(defconst dotjs-tool-versions
  '((node . "v18.19.0")
    (npm . "9.2.0")
    (python . "3.11.2")
    (poetry . "2.1.1")
    (guile . "3.0.8")
    (emacs . "31.0.50"))
  "Tool versions used in the project.")

;; Functions used for tangling and export - keep these for the Makefile
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

;; Provide this module
(provide 'config)
;;; config.el ends here