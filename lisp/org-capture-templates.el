   ;; Add these to your .emacs file
   (setq org-capture-templates
         '(("t" "Talk Insight" entry
            (file+headline "~/projects/aygp-dr/dotjs-2025/notes/quick-notes.org" "Conference Insights")
            "* %^{Speaker} - %^{Topic} %U\n%?")
           ("c" "Code Snippet" entry
            (file+headline "~/projects/aygp-dr/dotjs-2025/notes/code-snippets.org" "Code Examples")
            "* %^{Description} %U\n#+BEGIN_SRC javascript\n%?\n#+END_SRC")
           ("q" "Question for Q&A" entry
            (file+headline "~/projects/aygp-dr/dotjs-2025/notes/questions.org" "Questions")
            "* %^{For Speaker} - %^{Question} %U\n%?")))
