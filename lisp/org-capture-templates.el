;;; org-capture-templates.el --- Org capture templates for dotJS 2025

;; Author: Aidan Pace <apace@defrecord.com>
;; Keywords: org-mode, capture, conference, notes
;; Version: 0.1.0

;;; Commentary:
;; This file defines Org capture templates for quickly taking notes during
;; the dotJS 2025 conference.

;;; Code:

(require 'org-capture)
(require 'config)

(defvar dotjs-capture-templates
  '(("t" "Talk Insight" entry
     (file+headline (expand-file-name "notes/quick-notes.org" dotjs-project-root) "Conference Insights")
     "* %^{Speaker} - %^{Topic} %U\n%?")
    ("c" "Code Snippet" entry
     (file+headline (expand-file-name "notes/code-snippets.org" dotjs-project-root) "Code Examples")
     "* %^{Description} %U\n#+BEGIN_SRC javascript\n%?\n#+END_SRC")
    ("q" "Question for Q&A" entry
     (file+headline (expand-file-name "notes/questions.org" dotjs-project-root) "Questions")
     "* %^{For Speaker} - %^{Question} %U\n%?"))
  "Org capture templates for dotJS 2025 conference notes.")

(defun dotjs-setup-capture-templates ()
  "Set up Org capture templates for dotJS 2025."
  (interactive)
  (setq org-capture-templates dotjs-capture-templates)
  (message "dotJS 2025 capture templates configured"))

(provide 'org-capture-templates)
;;; org-capture-templates.el ends here
