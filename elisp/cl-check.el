;; Eval this buffer to check the program that still use 'cl
;; You could also use `doom/help-search-load-path' to locate the file
;; that still has `(require 'cl)' and manually change them to
;; `(eval-when-compile (require 'cl))'
(require 'loadhist)
(file-dependents (feature-file 'cl))
