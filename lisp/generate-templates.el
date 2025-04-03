;;; generate-templates.el --- Generate org files for dotJS talks

;; Define the talks
(setq dotjs-talks
      '(;; Morning sessions
        (:speaker "Ryan Dahl" :title "Special Announcement" :session "morning")
        (:speaker "Wes Bos" :title "Running AI models in JavaScript - good idea?" :session "morning")
        (:speaker "Sarah Drasner" :title "The Wind and the Waves: The formation of Framework Waves from the Epicenter" :session "morning")
        (:speaker "Kyle Simpson" :title "Love/Hate: Upgrading to Web2.5 with Local-First" :session "morning")
        ;; Afternoon sessions
        (:speaker "Angie Jones" :title "Modern Day Mashups: How AI Agents are Reviving the Programmable Web" :session "afternoon")
        (:speaker "Matteo Collina" :title "Node.js will use all the memory available, and that's OK!" :session "afternoon")
        (:speaker "Yohan Lasorsa" :title "Prompting is the New Scripting: Meet GenAIScript" :session "afternoon")
        (:speaker "Eduardo San Martin Morote" :title "From terrible to terrific frontend routers" :session "afternoon")
        ;; Lightning talks
        (:speaker "Antoine Caron" :title "Supercharge Web Performance with Shared Dictionaries: The Next Frontier in HTTP Compression" :session "lightning")
        (:speaker "Joyce Lin" :title "Code in the Physical World" :session "lightning")
        (:speaker "Charly Poly" :title "Durable Executions for Mortals" :session "lightning")
        (:speaker "Abbey Perini" :title "Coding and ADHD: Where We Excel" :session "lightning")
        (:speaker "Vadim Smirnov" :title "Recreating Windows Media Player Art With Web MIDI API" :session "lightning")))

;; Function to convert text to slug
(defun slugify (s)
  "Convert a string to a slug."
  (replace-regexp-in-string
   "[^a-z0-9-]" ""
   (replace-regexp-in-string
    "\\s+" "-"
    (downcase s))))

;; Function to generate template file
(defun generate-talk-template (talk)
  "Generate an org file template for a TALK."
  (let* ((speaker (plist-get talk :speaker))
         (title (plist-get talk :title))
         (session (plist-get talk :session))
         (slug (slugify (concat speaker "-" title)))
         (directory (cond
                     ((string= session "morning") "talks/morning-sessions")
                     ((string= session "afternoon") "talks/afternoon-sessions")
                     ((string= session "lightning") "notes/lightning-talks")
                     (t "talks")))
         (filename (expand-file-name (concat slug ".org") directory)))
    
    ;; Create directory if it doesn't exist
    (unless (file-exists-p directory)
      (make-directory directory t))
    
    ;; Create the file
    (with-temp-file filename
      (insert (format "#+TITLE: %s - %s\n" speaker title))
      (insert "#+DATE: April 3, 2025\n")
      (insert "#+CATEGORY: dotJS2025\n")
      (insert "#+PROPERTY: header-args :mkdirp yes\n")
      (insert (format "#+PROPERTY: header-args:js :tangle ../code-examples/demos/%s.js\n\n" slug))
      
      (insert "* Overview\n[Brief summary of the talk]\n\n")
      
      (insert "* Key Points\n- Point 1\n- Point 2\n- Point 3\n\n")
      
      (insert "* Code Examples\n#+BEGIN_SRC javascript\n")
      (insert (format "// Code example from %s's talk on %s\n" speaker title))
      (insert "console.log(\"Example code\");\n")
      (insert "#+END_SRC\n\n")
      
      (insert "* Diagrams\n#+BEGIN_SRC mermaid :file ")
      (insert (format "../diagrams/%s-diagram.svg\n" slug))
      (insert "graph TD\n")
      (insert "    A[Concept A] --> B[Concept B]\n")
      (insert "    B --> C[Concept C]\n")
      (insert "    C --> D[Implementation]\n")
      (insert "#+END_SRC\n\n")
      
      (insert "* Resources\n- [Link to slides]\n")
      (insert "- [Link to speaker's GitHub/website]\n")
      (insert "- [Other relevant resources]\n\n")
      
      (insert "* Questions & Answers\n- Q: [Question asked during Q&A]\n")
      (insert "- A: [Speaker's answer]\n\n")
      
      (insert "* Personal Notes\n- [Your thoughts and insights]\n")
      (insert "- [Implementation ideas]\n")
      (insert "- [Follow-up topics to research]\n"))
    
    (message "Created template for %s's talk: %s" speaker filename)
    filename))

;; Generate all templates
(defun generate-all-templates ()
  "Generate templates for all talks."
  (interactive)
  (dolist (talk dotjs-talks)
    (generate-talk-template talk))
  (message "Generated templates for all %d talks" (length dotjs-talks)))

;; Run the generation when this script is executed
(when load-file-name
  (generate-all-templates))

(provide 'generate-templates)
;;; generate-templates.el ends here
