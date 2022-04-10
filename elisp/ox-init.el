;; [[file:../config.org::*Init file][Init file:1]]
;;; ox-init.el -*- lexical-binding: t; -*-
; This is configuration for asynchronous
; exporting for org files

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(defvar doom-private-dir (concat (getenv "HOME") "/.config/doom"))
(load-file (expand-file-name "./elisp/variables.el" doom-private-dir))

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
