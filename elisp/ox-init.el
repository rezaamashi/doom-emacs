;; [[file:../config.org::*Init file][Init file:1]]
;;; ox-init.el -*- lexical-binding: t; -*-
; This is configuration for asynchronous
; exporting for org files

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(defvar doom-private-dir (concat (getenv "HOME") "/.config/doom"))
(load-file (expand-file-name "./elisp/variables.el" doom-private-dir))

<<<<<<< HEAD
(require 'cl)
(require 'org)
(with-eval-after-load 'org
  (setq org-id-locations-file (expand-file-name ".orgids" org-directory))
  (require 'org-transclusion)
  (require 'org-ref)
  (setq bibtex-completion-bibliography bib_files)
  (setq reftex-default-bibliography bib_files)
  (require 'oc)
  (setq org-cite-global-bibliography bib_files)
  (setq citar-bibliography bib_files)
  (require 'ox)
  (with-eval-after-load 'ox
    (require 'ox-extra)
    (ox-extras-activate '(latex-header-blocks ignore-headlines))
    (setq org-export-async-debug t))
  (require 'ox-latex)
  (with-eval-after-load 'ox-latex
   (add-to-list 'org-latex-packages-alist '("" "minted"))
   (setq org-latex-listings 'minted)
   (add-to-list 'org-latex-minted-langs '(ipython "python"))))
;; Init file:1 ends here
=======
(defvar bootstrap-version)
(let ((bootstrap-file
       ;; I use `.local' to reuse the previous repos pulled `doom-emacs'
       (expand-file-name ".local/straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun straight-reload-init ()
  "Reload init.el."
  (interactive)
  (straight-transaction
   (straight-mark-transaction-as-init)
   (message "Reloading init.el...")
   (load user-init-file nil 'nomessage)
   (message "Reloading init.el... done.")))

;;;;;;;;;;;;;;;;;
;; use-package ;;
;;;;;;;;;;;;;;;;;

;; Use-package is a declarative package configurator.
;; We need to set some configurations before "requiring" use-package, so that it
;; integrates better with imenu and this init file.
(setq-default use-package-enable-imenu-support t
              use-package-form-regexp-eval
              `(concat ,(eval-when-compile
                          (concat "^\\s-*("
                                  (regexp-opt '("use-package" "use-feature" "require") t)
                                  "\\s-+\\("))
                       (or (bound-and-true-p lisp-mode-symbol-regexp)
                           "\\(?:\\sw\\|\\s_\\|\\\\.\\)+") "\\)"))

;; Call straight-use-package to bootstrap use-package so we can use it.
(straight-use-package 'use-package)

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

;; When configuring a feature with `use-package', also tell
;; straight.el to install a package of the same name, unless otherwise
;; specified using the `:straight' keyword.
(setq-default straight-use-package-by-default t)

;; Tell `use-package' to always load features lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy. See
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading.
(setq-default use-package-always-defer t)

;;;;;;;;;;;;;;
;; Org Mode ;;
;;;;;;;;;;;;;;

;; Our real configuration for Org comes much later. Doing this now
;; means that if any packages that are installed in the meantime
;; depend on Org, they will not accidentally cause the Emacs-provided
;; (outdated and duplicated) version of Org to be loaded before the
;; real one is registered.
(use-package org)
;; Here we guarantee that org mode gets loaded properly.
(use-package org-contrib)
;; Here we guarantee that org mode gets loaded properly.

(use-feature org
 :config
 (setq-default org-catch-invisible-edits 'smart
               org-special-ctrl-a/e t
               org-image-actual-width '(400)
               org-return-follows-link t
               org-list-allow-alphabetical t
               ;; Aesthetics
               org-blank-before-new-entry '((heading . t) (plain-list-item . nil))
               org-fontify-quote-and-verse-blocks t
               org-hide-macro-markers nil
               org-fontify-whole-heading-line t
               org-fontify-done-headline t
               org-hide-emphasis-markers nil
               org-highlight-latex-and-related '(latex)
               ;; Image display
               org-image-actual-width '(400)))

(use-feature org-src
  :after org
  :demand t
  :config
  (setq-default org-edit-src-content-indentation 0
                org-src-preserve-indentation t
                org-src-fontify-natively t))

(use-feature ob
  :after org
  :demand t
  :config
  (setq-default org-confirm-babel-evaluate nil
                org-confirm-elisp-link-function nil
                org-confirm-shell-link-function nil)

  (dolist (language '((org . t)
                      (python . t)
                      (matlab . t)
                      (shell . t)
                      (latex . t)))
    (add-to-list 'org-babel-load-languages language))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

  :hook (org-babel-after-execute . org-display-inline-images))

(use-feature ox
  :after org
  :demand t
  :config
  ;; This is so that we are not queried if bind-keywords are safe when we set
  ;; org-export-allow-bind to t.
  (put 'org-export-allow-bind-keywords 'safe-local-variable #'booleanp)
  (setq org-export-with-sub-superscripts '{}
        org-export-coding-system 'utf-8
        org-html-checkbox-type 'html))

(use-feature org-attach
  :after org
  :demand t)

(use-feature ox-latex
  :after ox
  :demand t
  :init
  ;; Instead of using the long commands it is better to just use `latexmk', make
  ;; sure `latexmkrc' have properly set up with `makeglossaries' and `makeindex'
  (setq org-latex-pdf-process
        '("latexmk -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf -bibtex -f %f"
          ;; This is for regeneretable files cleanup
          "latexmk -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf -bibtex -f -c %f"))
  ;; these `setq's required by `org-ref' to include index, glossaries and acronym
  (setq org-export-before-parsing-functions
        '(org-latex-header-blocks-filter
          org-attach-expand-links
          org-ref-glossary-before-parsing
          org-ref-acronyms-before-parsing))
  ;; (setq org-latex-pdf-process
  ;;         '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;           "bibtex %b"
  ;;           "makeglossaries %b"
  ;;           "makeindex %b"
  ;;           "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;           "splitindex %b -s oscola"
  ;;           "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  :config

  ;; Sometimes it's good to locally override these two.
  (put 'org-latex-title-command 'safe-local-variable #'stringp)
  (put 'org-latex-toc-command 'safe-local-variable #'stringp)

  ;; Need to let ox know about ipython and jupyter
  (add-to-list 'org-latex-minted-langs '(ipython "python"))
  (add-to-list 'org-babel-tangle-lang-exts '("ipython" . "py"))
  (add-to-list 'org-latex-minted-langs '(jupyter-python "python"))
  (add-to-list 'org-babel-tangle-lang-exts '("jupyter-python" . "py"))

  ;; Default article class but no added package
  (add-to-list 'org-latex-classes
                    '("articlenopack"
                      "\\documentclass{articlenopack}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
                      ("\\section{%s}" . "\\section*{%s}")
                      ("\\subsection{%s}" . "\\subsection*{%s}")
                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                      ("\\paragraph{%s}" . "\\paragraph*{%s}")
                      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;; Mimore class is a latex class for writing articles.
  (add-to-list 'org-latex-classes
               '("mimore"
                 "\\documentclass{mimore}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; Mimosis is a class I used to write my Ph.D. thesis.
  (add-to-list 'org-latex-classes
               '("mimosis"
                 "\\documentclass{mimosis}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]
\\newcommand{\\mboxparagraph}[1]{\\paragraph{#1}\\mbox{}\\\\}
\\newcommand{\\mboxsubparagraph}[1]{\\subparagraph{#1}\\mbox{}\\\\}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\mboxparagraph{%s}" . "\\mboxparagraph*{%s}")
                 ("\\mboxsubparagraph{%s}" . "\\mboxsubparagraph*{%s}")))

  ;; Elsarticle is Elsevier class for publications.
  (add-to-list 'org-latex-classes
               '("elsarticle"
                 "\\documentclass{elsarticle}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (setq org-latex-prefer-user-labels t))

;; Feature `ox-extra' is a library from the org-plus-contrib package.
;; It adds extra keywords and tagging functionality for org export.
(use-feature ox-extra
  ;; Demand so that ignore headlines is always active.
  :demand t
  :after ox
  ;; The ignore-headlines allows Org to understand the tag :ignore: and simply
  ;; remove tagged headings on export, but leave their content in.
  ;; See my blog post about writing thesis with org mode here:
  ;; https://write.as/dani/writing-a-phd-thesis-with-org-mode
  :config (ox-extras-activate '(ignore-headlines)))

(use-package org-transclusion
  :after org
  :demand t
  :straight (org-transclusion :host github
                              :repo "nobiot/org-transclusion"))

;; The `org-ref' package adds functionality to manage, insert and navigate
;; citations (and other references as well, such as equations) within Org mode.
(use-package org-ref
  :straight (org-ref :type git :host github :repo "jkitchin/org-ref")
  :after org
  :demand t ;; Ensure that it loads so that links work immediately.
  :config
  (setq reftex-default-bibliography bib_files
        bibtex-completion-pdf-field "file"
        org-ref-default-citation-link "parencite"))

;;;;;;;;;;;
;; Latex ;;
;;;;;;;;;;;

(use-package auctex)
(use-feature tex
  :config
  (setq-default TeX-auto-save t
                TeX-PDF-mode t
                Tex-show-compilation nil
                TeX-parse-self t)
  :hook ((LaTeX-mode . visual-line-mode)
         (LaTeX-mode . (lambda ()
                         (add-to-list
                          'TeX-command-list
                          '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))))))

(use-feature tex-buf
  :config
  (setq TeX-save-query nil))

(use-package company-auctex
  :demand t
  :after (:all company tex)
  :config
  (company-auctex-init))

(use-package scimax-latex
  :straight (scimax-latex :type git
                          :host github
                          :repo "jkitchin/scimax"
                          :files ("scimax-latex.el"))
  :commands (scimax-latex-setup
             kpsewhich
             texdoc))
;; With Straight.el:1 ends here
>>>>>>> c3ee9b2 (temp commit to be combined with the previous ox-init commit)
