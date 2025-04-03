;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((eval . (progn
                   ;; Add lisp directory to load path
                   (add-to-list 'load-path
                                (expand-file-name "lisp"
                                                 (file-name-directory
                                                  (let ((d (dir-locals-find-file ".")))
                                                    (if (stringp d) d (car d))))))
                   
                   ;; Load configuration
                   (require 'config nil t)
                   
                   ;; Set up org mode
                   (with-eval-after-load 'org
                     (org-babel-do-load-languages
                      'org-babel-load-languages
                      '((emacs-lisp . t)
                        (js . t)
                        (python . t)
                        (scheme . t)
                        (shell . t)
                        (mermaid . t)))
                     
                     ;; Configure citation
                     (setq org-cite-global-bibliography
                           (list (expand-file-name "resources/bibliography/references.bib"
                                                  (file-name-directory
                                                   (let ((d (dir-locals-find-file ".")))
                                                     (if (stringp d) d (car d))))))
                     
                     ;; Capture templates
                     (with-eval-after-load 'org-capture
                       (require 'org-capture-templates nil t)))))))
 
 (org-mode . ((org-confirm-babel-evaluate . nil)
              (org-src-fontify-natively . t)
              (org-src-tab-acts-natively . t)
              (org-export-with-smart-quotes . t)
              (org-export-with-toc . t))))