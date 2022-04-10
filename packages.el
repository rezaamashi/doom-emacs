  (package! git-modes)

(package! graphql-mode)

  (package! latex-preview-pane)

  (package! tree-sitter)
  (package! tree-sitter-langs)

  (package! github-review :recipe
     (:host github
      :repo "charignon/github-review"
      :files ("github-review.el")))

  (package! git-timemachine)

  (package! git-messenger)

  (package! company-org-block)

  (package! company-math :recipe
          (:type git
           :host github
           :repo "vspinu/company-math"))

  (package! multi-vterm)

  (package! yarn
      :recipe (:host github
               :repo "jmfirth/yarn.el"))

  (package! parinfer-rust-mode)

  (package! company-nginx)

(package! speed-type)

  (package! lexic
      :recipe (:host github
               :repo "tecosaur/lexic"))

  (package! org-contrib)

(package! org-appear)

(package! org-pretty-table :recipe (:host github :repo "Fuco1/org-pretty-table"))

  (package! org-ol-tree :recipe (:host github :repo "Townk/org-ol-tree"))

(package! org-diff :recipe (:host github :repo "tecosaur/orgdiff"))

  (package! org-super-agenda)

  (package! org-habit-plus :recipe
      (:host github :repo "myshevchuk/org-habit-plus"
       :files ("org-habit-plus.el")))

  (package! calfw-org)

  (package! scimax-latex :recipe
      (:host github
       :repo "jkitchin/scimax"
       :files ("scimax-latex.el")))

  (package! cdlatex)

  (package! org-roam-ui
   :recipe (:host github
            :repo "org-roam/org-roam-ui"
            :files ("*.el" "out")))

  (package! org-roam-bibtex)

  (package! org-sort-task :recipe
      (:host github
       :repo "felipelalli/org-sort-tasks"
       :files ("org-sort-tasks.el")))

  (package! org-transclusion :recipe
       (:host github
        :repo "nobiot/org-transclusion"))

  (package! org-noter)

  (package! org-pandoc-import
   :recipe (:host github
            :repo "tecosaur/org-pandoc-import"
            :files ("*.el" "filters" "preprocessors")))

  (package! org-recoll)

  (package! org-randomnote)

(package! ivy-bibtex)

  (package! calibredb)

  (package! pdfgrep)

  (package! nov)

  (package! wttrin
    :recipe (:host github
             :repo "tecosaur/emacs-config"
             :files ("lisp/wttrin/wttrin.el")))

  (package! ol-notmuch)

  (package! notmuch-maildir)

  (package! notmuch-bookmarks)

  (package! bbdb-vcard)

  (package! counsel-bbdb)

  (package! org-mime)

  (package! slack)

  (package! ox-slack)

  (package! circe)

;; [[file:config.org::*Installation][Installation:1]]
(package! notdeft
  :recipe
  (:host github
   :repo "hasu/notdeft"))
;; Installation:1 ends here

(package! vlf)

  (package! elfeed-goodies)

  (package! org-web-tools)

  (package! calctex :recipe
      (:host github
       :repo "johnbcoughlin/calctex"
       :files
       ("*.el"
        "calctex/*.el"
        "calctex-contrib/*.el"
        "org-calctex/*.el"
        "vendor")))

  (package! sx)

;; [[file:config.org::*Installation][Installation:1]]
  (package! howdoyou)
;; Installation:1 ends here

  (package! discover-my-major)

  (package! helm-system-packages)

  (package! system-packages)

  (package! wgrep :recipe
       (:host github
        :repo "mhayashi1120/Emacs-wgrep"))

  (package! auto-minor-mode)

  (package! link-hint)
