;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; [[file:config.org::*Exclude Homedir][Exclude Homedir:1]]
(after! projectile
 (setq projectile-project-root-files-bottom-up (remove
            ".git" projectile-project-root-files-bottom-up)))
;; Exclude Homedir:1 ends here

;; [[file:config.org::*Log level][Log level:1]]
(setq! log-warning-minimum-level :error) ;;default :warning
;; Log level:1 ends here

;; [[file:config.org::*Bookmark Saving Configuration][Bookmark Saving Configuration:1]]
(setq bookmark-save-flag 1)
;; Bookmark Saving Configuration:1 ends here

;; [[file:config.org::*User Credentials][User Credentials:1]]
(setq user-full-name "Reza A'masyi"
      user-mail-address "mnurrreza@gmail.com")
;; User Credentials:1 ends here

;; [[file:config.org::*Workflows Variables][Workflows Variables:1]]
(load-file (expand-file-name "elisp/variables.el" doom-private-dir))
;; Workflows Variables:1 ends here

;; [[file:config.org::*Configuration][Configuration:1]]
(setq bare_git_dir (concat "--git-dir=" (expand-file-name "~/.settings/.dotfiles.git")))
(setq bare_work_tree (concat "--work-tree=" (expand-file-name "~")))
;; use maggit on git bare repos like dotfiles repos, don't forget to change `bare-git-dir' and `bare-work-tree' to your needs
(defun rz/magit-status-dotfiles-bare ()
  "set --git-dir and --work-tree in `magit-git-global-arguments' to `bare-git-dir' and `bare-work-tree' and calls `magit-status'"
  (interactive)
  (require 'magit-git)
  (add-to-list 'magit-git-global-arguments bare_git_dir)
  (add-to-list 'magit-git-global-arguments bare_work_tree)
  (call-interactively 'magit-status))

;; if you use `rz/magit-status-bare' you cant use `magit-status' on other other repos you have to unset `--git-dir' and `--work-tree'
;; use `rz/magit-status' insted it unsets those before calling `magit-status'
(defun rz/magit-status ()
  "removes --git-dir and --work-tree in `magit-git-global-arguments' and calls `magit-status'"
  (interactive)
  (require 'magit-git)
  (setq magit-git-global-arguments (remove bare_git_dir magit-git-global-arguments))
  (setq magit-git-global-arguments (remove bare_work_tree magit-git-global-arguments))
  (call-interactively 'magit-status))
;; Configuration:1 ends here

;; [[file:config.org::*Keybinding][Keybinding:1]]
(map! :leader
      :desc "Git dotfiles" "g d" #'rz/magit-status-dotfiles-bare
      :desc "Magit status" "g g" #'rz/magit-status)
;; Keybinding:1 ends here

;; [[file:config.org::*Doom font Configuration][Doom font Configuration:1]]
(setq doom-font (font-spec :family "Iosevka" :size 15)
      doom-big-font (font-spec :family "Iosevka" :size 36)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 18)
      doom-unicode-font (font-spec :family "Iosevka")
      doom-serif-font (font-spec :family "Bookerly" :weight 'light))
;; Doom font Configuration:1 ends here

;; [[file:config.org::*Mixed-Pitch][Mixed-Pitch:1]]
  (use-package! mixed-pitch
    :config
    ;; If you want it in all text modes:
    (add-hook 'text-mode-hook 'mixed-pitch-mode))
;; Mixed-Pitch:1 ends here

;; [[file:config.org::*Theme][Theme:1]]
;; (setq doom-theme 'doom-challenger-deep)
;; (setq doom-theme 'doom-solarized-light)
(setq doom-theme 'doom-city-lights)
;; Theme:1 ends here

;; [[file:config.org::*Time Display Configuration][Time Display Configuration:1]]
(setq display-time-24hr-format t)
;; Time Display Configuration:1 ends here

;; [[file:config.org::*Hide Mode line][Hide Mode line:1]]
(map! :leader
      :desc "Hide mode-line"
      "t m" #'hide-mode-line-mode)
;; Hide Mode line:1 ends here

;; [[file:config.org::*Line Numbering][Line Numbering:1]]
    ;; line numbering
    (column-number-mode)
    (global-display-line-numbers-mode t)
    (setq display-line-numbers-type 'relative)


  ;; disable line number for certain modes
  (dolist (no-line-mode '(org-mode-hook
                          org-agenda-mode-hook
                          term-mode-hook
                          pdf-view-mode-hook
                          shell-mode-hook
                          vterm-mode-hook
                          treemacs-mode-hook
                          eshell-mode-hook
                          nov-mode-hook
                          doc-view-mode-hook
                          image-mode-hook
                          notmuch-hello-mode-hook
                          elfeed-show-mode-hook))
    (add-hook! no-line-mode (lambda () (display-line-numbers-mode 0))))

  (map! :map minibuffer-mode-map
         "C-h" #'evil-delete-backward-char-and-join)

(map! :after evil
      :map ivy-minibuffer-map
       "TAB" #'ivy-alt-done
       "C-l" #'ivy-immediate-done
       "C-h" #'ivy-backward-delete-char
       "C-j" #'ivy-next-line
       "C-k" #'ivy-previous-line
      :map ivy-switch-buffer-map
       "C-k" #'ivy-previous-line
       "C-l" #'ivy-done
       "C-d" #'ivy-switch-buffer-kill
      :map ivy-reverse-i-search-map
       "C-k" #'ivy-previous-line
       "C-d" #'ivy-reverse-i-search-kill)

(define-key! "C-s" #'swiper)

(after! evil
  :config
  (define-key! evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key! evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ; use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

  (defun rz/lsp-mode-setup ()
    (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
    (lsp-headerline-breadcrumb-mode)

    (use-package! lsp-mode
      :commands (lsp lsp-deferred)
      ;; :bind-keymap
      ;; ("s-m" . lsp-command-map)
      :hook (lsp-mode . (lambda () (rz/lsp-mode-setup)))
      :config
      (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-c l' 'C-l', 's-l'
      (setq lsp-enable-which-key-integration t
            read-process-output-max (* 1024 1024)
            lsp-idle-delay 0.5)))

  (use-package! lsp-ui
    :hook (lsp-mode . (lambda () (lsp-ui-mode)))
    :init
    (general-setq lsp-ui-doc-enable nil)
    :custom
    (lsp-ui-doc-position 'bottom))

  (use-package! lsp-ivy
    :after lsp-mode)

  (use-package! lsp-treemacs
    :after lsp)

  (after! lsp
    (add-to-list 'lsp-language-id-configuration '(terraform-mode . "terraform"))

    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection '("usr/bin/terraform-lsp" "-enable-log-file")) ;; installed from AUR
                      :major-modes '(terraform-mode)
                      :server-id 'terraform-ls))

    (add-hook 'terraform-mode-hook #'lsp))

  (use-package! typescript-mode
    :mode "\\.ts\\'"
    :hook (typescript-mode . (lambda () (lsp-deferred)))
    :config
    (setq typescript-indent-level 2))

  (use-package! crontab-mode
    :defer t)

  (use-package! git-modes
    :config
    (add-to-list 'auto-mode-alist
                 (cons "/.dockerignore\\'" 'gitignore-mode)))

  (use-package! graphql-mode
    :hook (graphql-mode . (lambda () (lsp-deferred)))
    :mode "\\.graphql\\'")

  (use-package! latex-preview-pane
    :hook ((tex-mode
            latex-mode
            yatex-mode) . (lambda () (latex-preview-pane-mode))))

  (use-package! markdown-mode
    ;; :mode ("README\\.md\\'" . gfm-mode)
    :init (setq markdown-command "multimarkdown"))

      (use-package! python-mode
        :defer t)

(use-package! lsp-pyright)
  ;; :hook (python-mode . (lambda ()
                          ;; (require 'lsp-pyright)
                          ;; (lsp))  ; or lsp-deferred

  (use-package! web-mode
    :hook ((web-mode . lsp-deferred))
    ;; :mode ("\\.phtml\\'"
    ;;         "\\.tpl\\.php\\'"
    ;;         "\\.[agj]sp\\'"
    ;;         "\\.as[cp]x\\'"
    ;;         "\\.erb\\'"
    ;;         "\\.mustache\\'"
    ;;         "\\.djhtml\\'"
    ;;         "\\.html?\\'"
    ;;         "\\.css\\'"
    ;;         "\\.json\\'"
    ;;         "\\.tsx\\'")
    :config
          ;; Indentations
    (setq web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2
          ;; Features
          web-mode-enable-css-colorization t
          web-mode-enable-block-face t
          web-mode-enable-part-face t
          web-mode-enable-comment-interpolation t
          web-mode-enable-auto-pairing t
          web-mode-enable-heredoc-fontification t
          web-mode-enable-current-element-highlight t
          web-mode-enable-current-column-highlight t))

  (use-package! yaml-mode
    :defer t)
    ;; :hook (yaml-mode . (lambda())))
                         ;; (highlight-indent-guides-mode)
                         ;; (lsp-deferred))

  (use-package! tree-sitter
    :custom-face
    ;; (tree-sitter-hl-face:method.call   ((t (:inherit font-lock-function-name-face))))
    ;; (tree-sitter-hl-face:function.call ((t (:inherit font-lock-function-name-face))))
    ;; (tree-sitter-hl-face:operator      ((t (:inherit default))))
    ;; (tree-sitter-hl-face:type.builtin  ((t (:inherit font-lock-type-face))))
    ;; (tree-sitter-hl-face:number        ((t (:inherit highlight-numbers-number))))
    :config
    (global-tree-sitter-mode)
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

  (use-package! tree-sitter-langs
    :after tree-sitter)

  (use-package! projectile
    :diminish projectile-mode
    :config (projectile-mode)
    :custom
    (projectile-completion-system 'ivy)
    :init
    ;; note: set this to the folder where you keep your git repos!
    (when (file-directory-p (expand-file-name proj_dir))
      (setq projectile-project-search-path '("~/Documents/Reza/Project")))
    (setq projectile-switch-project-action #'projectile-dired)
    (setq projectile-enable-caching t))

(after! projectile (setq projectile-project-root-files-bottom-up (remove
            ".git" projectile-project-root-files-bottom-up)))

(use-package! counsel-projectile
  :after projectile
  :config (counsel-projectile-mode +1))

  (use-package! yasnippet
    :config
    ;; (setq 'yas-snippet-dirs
    ;;       '("~/.config/doom/snippets/"))
    (setq yas-triggers-in-field t)
    (yas-global-mode 1))

  (after! yasnippet
      (add-hook 'yas-minor-mode-hook (lambda ()
                                      (yas-activate-extra-mode 'fundamental-mode))))

  (use-package! auto-yasnippet
    :after yasnippet)

  (use-package! emmet-mode
    :hook (web-mode . (lambda () (emmet-mode))))

  (use-package! format-all
    ;; :preface
    ;; (defun ian/format-code ()
    ;;   "Auto-format whole buffer."
    ;;   (interactive)
    ;;   (if (derived-mode-p 'prolog-mode)
    ;;       (prolog-indent-buffer)
    ;;     (format-all-buffer)))
    :hook (prog-mode . (lambda () (format-all-mode))))
    ;; :config)
    ;; (global-set-key (kbd "M-F") #'ian/format-code)
    ;; (add-hook 'format-all-mode-hook #'format-all-ensure-formatter))

  (dolist (mode '(c-mode-hook))
   (add-hook mode (lambda () (format-all-mode 0))))

  (use-package! highlight-indent-guides
    :hook ((prog-mode conf-mode) . (lambda () (highlight-indent-guides-mode)))
    :init
    (setq highlight-indent-guides-method 'character
          highlight-indent-guides-suppress-auto-error t)
    :config
    (set-face-background 'highlight-indent-guides-odd-face "darkgray")
    (set-face-background 'highlight-indent-guides-even-face "dimgray")
    (set-face-foreground 'highlight-indent-guides-character-face "dimgray"))

  (use-package! magit
    :commands magit-status
    :config
    (setq magit-diff-refine-hunk 'all)
    :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

  (after! magit
    (setq project-switch-commands t))

  (use-package! forge
    :after magit)

  (use-package! github-review)

  (use-package! magit-todos
    :after magit
    :hook
    (magit-mode . (lambda () (magit-todos-mode))))

  (use-package! git-timemachine
    :after magit)

  (use-package! browse-at-remote
    :commands
    (browse-at-remote
     browse-at-remote-kill)
    :config
    (evil-define-key 'normal 'prog-mode-map
      (kbd "g D") #'browse-at-remote))

  (use-package! git-gutter-fringe
    :hook
    (prog-mode . (lambda () (git-gutter-mode))))

  (use-package! git-messenger
    :commands (git-messenger:popup-message))

  (use-package! company
    :after lsp-mode
    :hook (lsp-mode . (lambda () (company-mode)))
    :bind
    (:map company-active-map
          ("<tab>" . company-complete-selection)
          ("C-h" . evil-delete-backward-char-and-join)
          ("C-g" . evil-normal-state)
     :map lsp-mode-map
          ("<tab>" . company-indent-or-complete-common))
    :config
    (setq company-minimum-prefix-length 1
          company-idle-delay 0
          company-tooltip-align-annotations t))
          ;; company-show-quick-access t))
          ;; company-frontends '(company-tng-frontend company-box-frontend)
          ;; company-backends '(company-bbdb company-semantic company-cmake company-clang company-files
                             ;; (company-dabbrev-code company-gtags company-etags company-keywords)
                             ;; company-oddmuse company-dabbrev))

  (use-package! company-prescient
    :requires (prescient)
    :hook (company-mode . (lambda () (company-prescient-mode))))

  (use-package! company-dict
    :config
    (setq company-dict-dir (concat doom-private-dir "dict/"))
    )

  (use-package! company-org-block
    :after org
    :custom
    (setq company-org-block-edit-style 'inline) ;; 'auto, 'prompt, or 'inline
    )

  (use-package! company-math
    :after company)

(use-package! dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :commands dap-debug
  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup)) ;; Automatically installs Node debug adapter if needed

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  ;; (general-define-key
  ;;   :keymaps 'lsp-mode-map
  ;;   :prefix lsp-keymap-prefix
  ;;   "d" '(dap-hydra t :wk "debugger")))

  (use-package! vterm
    :commands vterm)
    ;; :bind ("C-c v" . vterm-other-window))

  (use-package! multi-vterm
          :config
          (add-hook 'vterm-mode-hook
                          (lambda ()
                          (setq-local evil-insert-state-cursor 'box)
                          (evil-insert-state)))
          (define-key vterm-mode-map [return]                      #'vterm-send-return)

          (setq vterm-keymap-exceptions nil)
          (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
          (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
          (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
          (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
          (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
          (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
          (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
          (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
          (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
          (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
          (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
          (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
          (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
          (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
          (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
          (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
          (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
          (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
          (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
          (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
          (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
          (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
          (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
          (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
          (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume)
          )

  (use-package! minimap
    :defer t
    :custom (minimap-window-location 'right))

  (use-package! evil-nerd-commenter)
    ;; :bind ("M-/" . evilnc-comment-or-uncomment-lines))

  (use-package! yarn)

  (use-package! npm
    :commands npm
    :config
    (setq npm-test-library nil)) ;; default is jest

  (use-package! hl-todo
    :hook (prog-mode . (lambda () (hl-todo-mode)))
    :config
    (setq hl-todo-keyword-faces
          `(("TODO" . ,(face-foreground 'warning))
            ("PROJ"  . ,(face-foreground 'error))
            ("SOMEDAY"  . ,(face-foreground 'warning))
            ("TODO"  . ,(face-foreground 'warning))
            ("PROG" . ,(face-foreground 'error))
            ("NEXT" . ,(face-foreground 'error))
            ("WAIT" . ,(face-foreground 'warning))
            ("CANCEL" . ,(face-foreground 'error))
            ("DELEGATED" . ,(face-foreground 'error))
            ("IDEA" . ,(face-foreground 'warning))
            ("RDNOTE" . ,(face-foreground 'warning))
            ("GOAL" . ,(face-foreground 'warning))
            ("DUD" . ,(face-foreground 'error))
            ("RD" . ,(face-foreground 'warning))
            ("RDING" . ,(face-foreground 'warning))
            ("TMPDROP" . ,(face-foreground 'warning))
            ("DROP" . ,(face-foreground 'error))
            ("FNSHED" . ,(face-foreground 'success))
            ("DONE"  . ,(face-foreground 'success)))))

  (use-package! ws-butler
    :hook
    (prog-mode . (lambda () (ws-butler-mode)))
    :config
    (ws-butler-global-mode))

  (use-package! highlight-numbers
    :hook
    (prog-mode . (lambda () (    highlight-numbers-mode))))

  (use-package! rainbow-delimiters
    :hook (prog-mode . (lambda () (rainbow-delimiters-mode))))

  (use-package! smartparens
    :hook ((org-mode prog-mode) . (lambda () (smartparens-mode)))
    :bind
    (:map sp-pair-overlay-keymap
          ("C-g" . evil-normal-state))
    :config
    (sp-local-pair
     '(org-mode)
     "<<" ">>"
     :actions '(insert)))

  (use-package! parinfer-rust-mode
      :hook ( emacs-lisp-mode . (lambda () (parinfer-rust-mode)))
      :init
      (setq parinfer-rust-auto-download t))

  (use-package! ansible
    :commands ansible-auto-decrypt-encrypt
    :init
    (put 'ansible-vault-password-file 'safe-local-variable #'stringp)
    :config
    (setq ansible-section-face 'font-lock-variable-name-face
          ansible-task-label-face 'font-lock-doc-face))

  (use-package! ansible-doc
    :defer t)

  (use-package! company-ansible
    :after ansible)

  (use-package! docker)
    ;; :bind ("C-c d" . docker))

  (use-package! dockerfile-mode
    :defer t)

  (use-package! docker-compose-mode
    :defer t)

  (use-package! docker-tramp
    :defer t)

  (use-package! jenkins
    :commands jenkins)

  (use-package! jenkins-watch
    :after jenkins)

  (use-package! jenkinsfile-mode
    :defer t)

  (use-package! kubernetes
    :commands kubernetes-overview
    :config
    (setq kubernetes-poll-frequency 3600
          kubernetes-redraw-frequency 3600))

  (use-package! kubernetes-evil
    :after kubernetes)

  (use-package! nginx-mode
    :defer t
    :config
    (add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)))

  (use-package! company-nginx
    :hook (nginx-mode . (lambda () (add-to-list 'company-backends #'company-nginx))))

  (use-package! terraform-mode
    :defer t
    :hook
    ((terraform-mode .  (lambda () (add-to-list 'company-backends #'company-terraform)))
     (terraform-mode . (lambda () (lsp-deferred)))))

  (use-package! company-terraform
    :after terraform-mode
    :config
    (company-terraform-init))

  (use-package! editorconfig
    :hook (( prog-mode conf-mode ) . editorconfig-mode)
    :config
      (setq editorconfig-trim-whitespaces-mode
        'ws-butler-mode))
    ;; (editorconfig-mode 1))

  (use-package! speed-type
    :commands speed-type-text)

  (use-package! lexic
    :hook (lexic-mode . (lambda () (visual-line-mode)))
    :commands (lexic-search-word-at-point lexic-search lexic-list-dictionary)
    :bind ("<f12>" . lexic-search-word-at-point))

  (defun rz/org-mode-setup ()
    (org-indent-mode)
    ;; (variable-pitch-mode 1)
    (mixed-pitch-mode 1)
    (setq line-spacing 3)
    (visual-line-mode 1)
    (setq evil-auto-indent nil))

  (defun rz/org-header-setup ()
    ;; Set faces for heading levels
    (dolist (face '((org-level-1 . 1.2)
                    (org-level-2 . 1.15)
                    (org-level-3 . 1.1)
                    (org-level-4 . 1.075)
                    (org-level-5 . 1.05)
                    (org-level-6 . 1.05)
                    (org-level-7 . 1.05)
                    (org-level-8 . 1.05)))
      (set-face-attribute (car face) nil :font "Fira Sans" :weight 'regular :height (cdr face)))
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-document-title nil :font "Bookerly" :weight 'bold :height 200)
    (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-headline-done nil  :foreground "#56697A" :strike-through t)
    (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
    (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

  (after! org
    (setq org-fontify-whole-heading-line t)
    (setq org-fontify-done-headline t)
    (setq org-fontify-quote-and-verse-blocks t))

  (after! org
    (use-package! org-checklist)
    (setq-default prettify-symbols-alist
                  '(;; Blocks
                   ; Comment
                   ("#+BEGIN_COMMENT" . "")
                   ("#+END_COMMENT" . "")
                   ("#+begin_comment" . "")
                   ("#+end_comment" . "")
                   ; Center
                   ("#+BEGIN_CENTER" . "")
                   ("#+END_CENTER" . "")
                   ("#+begin_center" . "")
                   ("#+end_center" . "")
                   ; Example
                   ("#+BEGIN_EXAMPLE" . "")
                   ("#+END_EXAMPLE" . "")
                   ("#+begin_example" . "")
                   ("#+end_example" . "")
                   ; Verse
                   ("#+BEGIN_VERSE" . "")
                   ("#+END_VERSE" . "")
                   ("#+begin_verse" . "")
                   ("#+end_verse" . "")
                   ; Export
                   ("#+BEGIN_EXPORT" . "")
                   ("#+END_EXPORT" . "")
                   ("#+begin_export" . "")
                   ("#+end_export" . "")
                   ;; Arrows
                   (">=" . "≥")
                   ("<=" . "≤")
                   ("=>" . "⇨")
                   ;; Check Boxes
                   ("[ ]" .  "")
                   ("[X]" . "" )
                   ("[-]" . "" )
                   ;; Properties
                   (":LOGBOOK:" . "▤")
                   (":PROPERTIES:" . "⚙")
                   (":END:" . "⏏")
                   ("DEADLINE:" . "☎")
                   ("SCHEDULED:" . "")
                   (":Effort:" . "")
                   ;; Header
                   ("#+STARTUP:" . "➶")
                   ("#+TITLE: " . "")
                   ("#+RESULTS:" . "")
                   ("#+NAME:" . "")
                   ("#+OPTIONS:" . "")
                   ("#+PROPERTY:" . "⚙")
                   ("#+FILETAGS:" . "")
                   ("#+HTML_HEAD:" . "")
                   ("#+SUBTITLE:" . "")
                   ("#+AUTHOR:" . "")
                   ("#+DATE:" . "")
                   ("#+EMAIL:" . "✉")
                   ("#+SETUPFILE:" . "")
                   ;; ;; Todos
                   ;; ; Main
                   ;; ("TODO" . "")
                   ;; ("NEXT" . "")
                   ;; ("PROG" . "")
                   ;; ("PROJ" . "")
                   ;; ("WAIT" . "")
                   ;; ("CANCEL" . "")
                   ;; ("DONE" . "")
                   ;; ; Reading
                   ;; ("RD" . "")
                   ;; ("RDING" . "")
                   ;; ("TMPDROP" . "")
                   ;; ("DROP" . "")
                   ;; ("FNSHED" . "")
                   ))
    (setq prettify-symbols-unprettify-at-point 'right-edge)
    (add-hook 'org-mode-hook 'prettify-symbols-mode))

  (use-package! org
    :hook ((org-mode . (lambda()
                        (rz/org-mode-setup)
                        (rz/org-header-setup))))
    ;;       (org-mode . (lambda()
    ;;                     (add-to-list 'company-backends
    ;;                                 '(company-capf
    ;;                                   company-bbdb
    ;;                                   company-ispell
    ;;                                   company-files
    ;;                                   company-math-symbols-latex
    ;;                                   company-math-symbols-unicode))

    ;;                    (company-mode +1)))
    :config
    (setq org-ellipsis " ⤵"
          org-startup-indented t
          org-hide-emphasis-markers t
          org-directory org_file_dir
          org-priority-lowest ?D
          org-pretty-entities t))

    (defun rz/org-find-time-file-property (property &optional anywhere)
      "Return the position of the time file PROPERTY if it exists.
       When ANYWHERE is non-nil, search beyond the preamble."
      (save-excursion
        (goto-char (point-min))
        (let ((first-heading
               (save-excursion
                 (re-search-forward org-outline-regexp-bol nil t))))
          (when (re-search-forward (format "^#\\+%s:" property)
                                   (if anywhere nil first-heading)
                                   t)
            (point)))))

    (defun rz/org-has-time-file-property-p (property &optional anywhere)
      "Return the position of time file PROPERTY if it is defined.

  As a special case, return -1 if the time file PROPERTY exists but
  is not defined."
      (when-let ((pos (rz/org-find-time-file-property property anywhere)))
        (save-excursion
          (goto-char pos)
          (if (and (looking-at-p " ")
                   (progn (forward-char)
                          (org-at-timestamp-p 'lax)))
              pos
            -1))))

    (defun rz/org-set-time-file-property (property &optional anywhere pos)
      "Set the time file PROPERTY in the preamble.
  When ANYWHERE is non-nil, search beyond the preamble.
  If the position of the file PROPERTY has already been computed,
  it can be passed in POS."
      (when-let ((pos (or pos
                          (rz/org-find-time-file-property property))))
        (save-excursion
          (goto-char pos)
          (if (looking-at-p " ")
              (forward-char)
            (insert " "))
          (delete-region (point) (line-end-position))
          (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
            (insert now)))))

    (defun rz/org-set-last-modified ()
      "Update the LAST_MODIFIED file property in the preamble."
      (when (derived-mode-p 'org-mode)
        (rz/org-set-time-file-property "LAST_MODIFIED")))

  (use-package! org-appear
    :hook (org-mode . org-appear-mode)
    :config
    (setq org-appear-autoemphasis t
          org-appear-autosubmarkers t
          org-appear-autolinks nil)
    ;; for proper first-time setup, `org-appear--set-elements'
    ;; needs to be run after other hooks have acted.
    (run-at-time nil nil #'org-appear--set-elements))

  (use-package! org-pretty-table
    :hook (org-mode . (lambda() (org-pretty-table-mode))))

  (use-package! org-superstar
    :hook (org-mode . (lambda () (org-superstar-mode)))
    :config
    (setq org-superstar-special-todo-items t
          org-superstar-todo-bullet-alist
          '(("TODO" . 61708)
            ("NEXT" . 61469)
            ("PROG" . 61729)
            ("PROJ" . 61729)
            ("WAIT" . 62092)
            ("CANCEL" . 61532)
            ("DONE" . 61533)
            ("RD" . 61708)
            ("RDING" . 61469)
            ("TMPDROP" . 62092)
            ("DROP" . 61532)
            ("FNSHED" . 61533))))

  (use-package! org-ol-tree
    :commands org-ol-tree)

  (after! org
    (require 'ox-extra)
    (ox-extras-activate '(latex-header-blocks ignore-headlines)))

  (after! org
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (python     . t)
      (shell      . t)
      (js         . t)
      (perl       . t)
      (clojure    . t)
      (ruby       . t)
      (dot        . t)
      (css        . t)
      (plantuml   . t)))
  (add-to-list 'org-src-lang-modes '("dot" . "graphviz-dot"))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

  (use-package! ob-async
    :after org)

  (after! org
    (add-to-list 'org-src-lang-modes '("inline-js" . javascript))
    (defvar org-babel-default-header-args:inline-js
      '((:results . "html")
        (:exports . "results")))
    (defun org-babel-execute:inline-js (body _params)
      (format "<script type=\"text/javascript\">\n%s\n</script>" body)))

  (after! org
   (add-to-list 'org-src-lang-modes '("latex-macros" . latex))

   (defvar org-babel-default-header-args:latex-macros
     '((:results . "raw")
       (:exports . "results")))

   (defun prefix-all-lines (pre body)
     (with-temp-buffer
       (insert body)
       (string-insert-rectangle (point-min) (point-max) pre)
       (buffer-string)))

   (defun org-babel-execute:latex-macros (body _params)
     (concat
      (prefix-all-lines "#+LATEX_HEADER: " body)
      "\n#+HTML_HEAD_EXTRA: <div style=\"display: none\"> \\(\n"
      (prefix-all-lines "#+HTML_HEAD_EXTRA: " body)
      "\n#+HTML_HEAD_EXTRA: \\)</div>\n")))

(after! org
  ;; this is needed as of org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

  (use-package! orgdiff
    :defer t
    :config
    (defun rz/orgdiff-nicer-change-colours ()
      (goto-char (point-min))
      ;; Set red/blue based on whether chameleon is being used
      (if (search-forward "%% make document follow Emacs theme" nil t)
          (setq red  (substring (doom-blend 'red 'fg 0.8) 1)
                blue (substring (doom-blend 'blue 'teal 0.6) 1))
        (setq red  "c82829"
              blue "00618a"))
      (when (and (search-forward "%DIF PREAMBLE EXTENSION ADDED BY LATEXDIFF" nil t)
                 (search-forward "\\RequirePackage{color}" nil t))
        (when (re-search-forward "definecolor{red}{rgb}{1,0,0}" (cdr (bounds-of-thing-at-point 'line)) t)
          (replace-match (format "definecolor{red}{HTML}{%s}" red)))
        (when (re-search-forward "definecolor{blue}{rgb}{0,0,1}" (cdr (bounds-of-thing-at-point 'line)) t)
          (replace-match (format "definecolor{blue}{HTML}{%s}" blue)))))
    (add-to-list 'orgdiff-latexdiff-postprocess-hooks #'+orgdiff-nicer-change-colours))

  (after! org-agenda
    (setq org-agenda-files (list
                            (concat org_agenda "projects.org")
                            (concat org_agenda "daily_habits.org")
                            (concat org_agenda "weekly_habits.org")
                            (concat org_agenda "monthly_habits.org")
                            ;; (concat org_file_dir "quarterly_habits.org")
                            ;; (concat org_file_dir "personal.org")
                            (concat org_agenda "inbox.org")
                            (concat org_agenda "next.org")
                            (concat org_agenda "waiting.org")
                            (concat org_agenda "future.org")
                            (concat org_agenda "this_month.org")
                            (concat org_agenda "mail.org")
                            (concat org_agenda "pe.org")
                            (concat org_agenda "birthdays/")
                            (concat org_agenda "reading_list.org")
                            ;; org_file_dir
                            ;; work-path
                            ;; (concat org_file_dir "projects/2021/")
                            ;; (concat org_file_dir "journal/")
                            )))

    (after! org-agenda
      (setq org-agenda-time-grid
            (quote
             ((daily today require-timed) ()
              "......" "----------------"))))

  (after! org-agenda
    (setq org-agenda-start-with-log-mode t)
    (setq org-log-done 'time)
    (setq org-log-into-drawer t))

  (after! org-agenda
    (use-package! org-depend)
    (use-package! org-choose)
    (use-package! org-effectiveness)
    (setq org-todo-keywords '((sequence
                               "TODO"
                               "PROJ"
                               "NEXT(n)"
                               "PROG(p!)"
                               "WAIT(w@/!)"
                               "SOMEDAY"
                               "|"
                               "DONE(d)"
                               "CANCEL(c@)"
                               "DELEGATED(@)")

                              (sequence
                               "IDEA"
                               "RDNOTE"
                               "GOAL"
                               "|"
                               "DUD(@)")
                              (sequence
                               "RD"
                               "RDING"
                               "TMPDROP"
                               "|"
                               "DROP"
                               "FNSHED"))))

  (use-package! org-super-agenda
    :after org-agenda
    :config
    (setq org-agenda-skip-deadline-if-done t
          org-agenda-skip-scheduled-if-done t
          org-agenda-include-deadlines t)
    (org-super-agenda-mode))

  (after! org-super-agenda
  (setq org-agenda-custom-commands '(("d" "Dashboard"
                                       ((agenda "" ((org-agenda-span 'day)
                                                    (org-agenda-start-day "+0d")
                                                    (org-agenda-overriding-header "")
                                                    (org-super-agenda-groups
                                                     '((:name "Important" :priority "A" :order 1)
                                                       (:name "Email" :tag "email" :order 2)
                                                       (:name "Today"
                                                        :discard (:tag "email")
                                                        :time-grid t
                                                        :date today
                                                        :scheduled today
                                                        :deadline today
                                                        :todo "TODAY")
                                                       (:name "Work" :tag "@work" :order 2)
                                                       (:name "School" :tag "@school" :order 2)
                                                       (:name "Hobby" :tag "hobby" :order 2)
                                                       (:todo "PROG")
                                                       (:name "My Goals" :todo "GOAL" :order 1)
                                                       (:name "Next Actions" :todo "NEXT" :order 1)
                                                       (:name "Waiting For" :todo "WAIT" :order 1)
                                                       (:name "Your Projects":todo "PROJ" :order 1)
                                                       (:name "Quick Picks" :effort< "0:20" :order 2)
                                                       (:name "Tasks" :discard(:habit) :todo "TODO" :order 1)
                                                       (:name "My Goals" :todo "GOAL" :order 1)
                                                       (:name "Books You Are Reading"
                                                               :todo "RDING" :order 2)
                                                       (:priority<= "B" :order 99)
                                                       (:discard (:todo ("IDEA" "SOMEDAY" "TMPDROP" "RD") :habit))
                                                       ))))))
                                     ("j" "Overview"
                                       ((alltodo "" ((org-agenda-span 'day)
                                                    (org-agenda-overriding-header "Overview")
                                                    (org-super-agenda-groups
                                                     '((:name "Important" :priority "A" :order 1)
                                                       (:name "Email" :tag "email" :order 2)
                                                       (:name "Work" :tag "@work" :order 2)
                                                       (:name "School" :tag "@school" :order 2)
                                                       (:name "Hobby" :tag "hobby" :order 2)
                                                       (:todo "PROG")
                                                       (:name "My Goals" :todo "GOAL" :order 1)
                                                       (:name "Next Actions" :todo "NEXT" :order 1)
                                                       (:name "Waiting For" :todo "WAIT" :order 1)
                                                       (:name "Your Projects":todo "PROJ" :order 1)
                                                       (:name "Quick Picks" :effort< "0:20" :order 2)
                                                       (:name "Tasks" :discard(:habit) :todo "TODO" :order 1)
                                                       (:name "My Goals" :todo "GOAL" :order 1)
                                                       (:name "Books You Are Reading"
                                                               :todo "RDING" :order 2)
                                                       (:priority<= "B" :order 99)
                                                       (:discard (:todo ("IDEA" "SOMEDAY" "TMPDROP" "RD") :habit))
                                                        ))))))
                                     ("k" "Kanban"
                                      ((alltodo "" ((org-agenda-overriding-header "Kanban Board")
                                                    (org-super-agenda-groups
                                                      '((:name "Backlog" :tag "backlog" :order 1)
                                                        (:name "Planning" :tag "plan" :order 1)
                                                        (:name "In Progress" :tag "active" :order 1)
                                                        (:name "Testing" :tag "testing" :order 1)
                                                        (:name "Completed" :tag "active" :order 1)
                                                        (:name "Canceled" :tag "Canceled" :order 1)
                                                        (:discard (:anything t))
                                                       ))))))
                                      ("b" "Books"
                                       ((alltodo "" ((org-agenda-overriding-header "Books that Garner Your Insterests")
                                                    (org-super-agenda-groups
                                                     '((:name "Books You Are Reading"
                                                              :todo "RDING")
                                                       (:name "Books To Read"
                                                              :todo "RD")
                                                       (:name "Books You Left Temporarily"
                                                              :todo "TMPDROP")
                                                       (:discard (:anything t)))))))))))

  (after! org-agenda
    (setq org-agenda-dim-blocked-tasks 'invisible))

  (after! org
    (use-package! org-interactive-query)

    (setq org-tag-alist
          '(; Environmental Context
            (:startgroup)
            ("@home" . ?H)
            ("@work" . ?W)
            ("@college" . ?C)
            ("@everywhere" . ?E)
            (:endgroup)
            ; Workflow Context
            (:startgroup)
            ("@smartphone" . ?s)
            ("@laptop" . ?l)
            (:endgroup)
            ;Agenda Context
            (:startgroup)
            ("@errand" . ?e)
            ("@job" . ?o)
            ("@favor" . ?f)
            (:endgroup)
            ;Kanban
            (:startgroup)
            ("review" . ?w)
            ("plan" . ?p)
            ("active" . ?v)
            ("backlog" . ?b)
            ("testing" . ?r)
            (:startgroup)
            ("completed" . ?d)
            ("canceled" . ?c)
            (:endgroup)
            (:endgroup)
            ("publish" . ?P)
            ("batch" . ?b)
            ("thesis" . ?t)
            ("agenda" . ?a)
            ("project" . ?j)
            ("email" . ?m)
            ("note" . ?n)
            ("idea" . ?i))))

  (after! org
    (setq org-refile-targets
          '(("../archive/Archive.org" :maxlevel . 1)
            ("../archive/Read.org" :maxlevel . 1)
            ("future.org" :maxlevel . 1)
            ("projects.org" :maxlevel . 1)
            ("this_month.org" :maxlevel . 1)
            ("inbox.org" :maxlevel . 1)
            ("waiting.org" :maxlevel . 1)
            ("next.org" :maxlevel . 1)))
    ;; Save Org buffers after refiling!
    (advice-add 'org-refile :after 'org-save-all-org-buffers))

  (after! org-capture
      (setq org-capture-templates
          ;; Tasks
        `(("t" "Tasks / Projects")
          ("tt" "Today" entry (file+olp ,(concat org_agenda "next.org") "Next Actions")
           "* NEXT %?\nSCHEDULED:%^T  %U\n  %a\n  %i" :empty-lines 1)
          ("td" "Today Deadline" entry (file ,(concat org_agenda "next.org") "Next Actions")
           "* TODO %? \nDEADLINE: %^T\n %U" :empty-lines 1)
          ("ts" "Inbox Scheduled" entry (file+olp ,(concat org_agenda "inbox.org") "Tasks")
           "* TODO %?\nSCHEDULED:%^T  %U\n  %a\n  %i" :empty-lines 1)
          ("tD" "Inbox Deadline" entry (file ,(concat org_agenda "inbox.org") "Tasks")
           "* TODO %? \nDEADLINE: %^T\n %U" :empty-lines 1)
          ("tw" "Wait deadline" entry (file+olp ,(concat org_agenda "waiting.org") "Waiting For")
           "* WAIT %? From _%^{Delegated To}_ \nDEADLINE: %^T\n %U\n %a" :empty-lines 1)

          ;; Catchall for faster capture "SPC-x-x"
          ("x" "Inbox" entry (file+olp ,(concat org_agenda "inbox.org") "Everything/Notes")
           "* %? \n %U\n %a" :empty-lines 1)

          ;; Events
          ("e" "Event" entry (file ,(concat org_agenda "next.org"))
           "* %? \n%^{Event}T\n %U\n %a" :empty-lines 1)

          ;; Reading Lists
          ("r" "Reading List" entry
           (file+olp ,(concat org_agenda "reading_list.org") "Catchall")
           "* RD %? \n%U\n%a" :empty-lines 1)

          ;; Birthdays
          ("b" "Birthdays")
          ("br" "Relatives/Family" entry
           (file+olp ,(concat org_agenda "birthdays/relatives.org") "Relatives")
           "* %^{Who?} \n%^{Birthday}t\n%U" :empty-lines 1)
          ("ba" "Acquintances" entry
           (file+olp ,(concat org_agenda "birthdays/acquintances.org") "Acquintances")
           "* %^{Who?} \n%^{Birthday}t\n%U" :empty-lines 1)
          ("bf" "Friends" entry
           (file+olp ,(concat org_agenda "birthdays/friends.org") "Friends")
           "* %^{Who?} \n%^{Birthday}t\n%U" :empty-lines 1)
          ("bo" "Others" entry
           (file+olp ,(concat org_agenda "birthdays/others.org") "Others")
           "* %^{Who?} \n%^{Birthday}t\n%U" :empty-lines 1)

          ;; workflow
          ("m" "Meeting" entry
           (file+olp+datetree ,(concat org_file_dir "meetings.org") "Active")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume :empty-lines 1)
          ("E" "Emails")
          ("Er" "Read Later" entry
           (file+olp ,(concat org_agenda "mail.org") "Read Later")
           (file ,(concat doom-private-dir "orgtemplates/mailreadlater.org"))
           :empty-lines 1 :immediate-finish t)
          ("Ef" "Follow Up" entry (file+olp ,(concat org_agenda "mail.org") "Follow Up")
           (file ,(concat doom-private-dir "orgtemplates/mailfollowup.org"))
           :empty-lines 1 :immediate-finish t)
          ("Es" "Send Mail" entry
           (file+olp ,(concat org_agenda "mail.org") "Send Mail")
           (file ,(concat doom-private-dir "orgtemplates/mailsendmail.org"))
           :empty-lines 1 :immediate-finish t)

          ;; Tracking
          ("M" "Metrics Capture")
          ("Mw" "Weight" table-line
           (file+headline ,(concat org_file_dir "weight.org") "Weight")
           "| %U | %^{Weight} | %^{Notes} |" :immediate-finish t)
          ("Mp" "PE" table-line
           (file+headline ,(concat org_agenda "pe.org") "Measurements")
           "| %U | %^{BPEL} | %^{EG} | %^{NBPEL} | %^{BPFSL} |"
           :immediate-finish t)
          ("Ml" "Lead Managements" table-line
           (file+headline "~/Videos/Intergender Dynamic/Occam's Razor - Ultimate Seduction System/Template For Managing Leads/Template for Managing Leads.org" "Leads")
           "| %U | %^{Girl Name} | %^{Date Time}T | %^{Had Sex?} | %^{Repeat?} | %^{Source (Daygame, Nightgame, Onlinegame, Else)} | %^{Description} | %^{Next Actions} |"
           :immediate-finish t)

          ;; Journal
          ("j" "Journal Entries")
          ("jj" "Journal Entry" entry
           (function rz/org-journal-find-location)
           "\n** %<%I:%M %p> - %? :journal:\n" :empty-lines 1)
          ("js" "Scheduled Journal" entry
           (function org-journal-date-location)
           "* TODO %?\n <%(princ org-journal--date-location-scheduled-time)>\n"
           :empty-lines 1)
          ("jm" "Morning Journal entry" entry
           (function rz/org-journal-find-location)
           (file ,(concat doom-private-dir "orgtemplates/morningroutine.org"))
           :empty-lines 1 :jump-to-captured t)
          ("jn" "Night Journal entry" entry
           (function rz/org-journal-find-location)
           (file ,(concat doom-private-dir "orgtemplates/nightroutine.org"))
           :empty-lines 1 :jump-to-captured t)
          ("jw" "Weekly Review" entry
           (function rz/org-journal-find-location)
           (file ,(concat doom-private-dir "orgtemplates/weeklyreview.org"))
           :empty-lines 1 :jump-to-captured t)

          ;; Cookbook
          ("c" "Cookbook")
          ("cc" "Web Fetch" entry (file "~/org/cookbook.org")
           "%(org-chef-get-recipe-from-url)"
           :empty-lines 1)
          ("cm" "Manual Cookbook" entry (file ,(concat org_file_dir "cookbook.org"))
           "* %^{Recipe title: }\n:PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")

          ;; Protocol
          ("Q" "Protocol Quote" entry (file+olp ,(concat org_agenda "inbox.org") "Web Quote")
           "* %^{Quote From}\n:PROPERTIES:\nSOURCE: %:annotation\nCREATED_AT:%u\n:END:\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n%?"
           :prepend t
           :kill-buffer t)
          ("L" "Protocol Link" entry (file+olp ,(concat org_agenda "reading_list.org") "Web")
           "* RD [[%:link][%:description]]\n%? "
           :prepend t
           :kill-buffer t))))

  (use-package! org-habit-plus
    :after org-agenda
    :init
    (add-to-list 'org-modules 'org-habit-plus)
    :custom
    (setq org-habit-graph-column 60
          org-habit-show-habits-only-for-today t))

  (use-package! calfw
    :after org-agenda)

  (use-package! calfw-org
    :after calfw)

  (defun my-open-calendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-source "Green")  ; org-agenda source
      ;; (cfw:org-create-file-source "cal" "/path/to/cal.org" "Cyan")  ; other org source
      ;; (cfw:howm-create-source "Blue")  ; howm source
      ;; (cfw:cal-create-source "Orange") ; diary source
      ;; (cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
      ;; (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed") ; google calendar ICS
     )))

  (use-package! scimax-latex
    :defer t
    :commands (scimax-latex-setup
               kpsewhich
               texdoc))

  (use-package! cdlatex
    :config
    (add-hook 'org-mode-hook #'turn-on-org-cdlatex))

  (use-package! org-fragtog
    :commands org-fragtog-mode)

  (use-package! citeproc)

  (use-package! org-ref
      :config
      (setq
           org-ref-completion-library 'org-ref-ivy-cite
           org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
           reftex-default-bibliography '("~/Documents/Reza/BibTex/Zotero-mylib/Zotero-mylib.bib" "~/Documents/Reza/BibTex/Zotero-mylib/CalibreBib.bib")
           org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
           org-ref-notes-directory org_noter
           org-ref-notes-function 'orb-edit-notes))

  (global-set-key (kbd "<f6>") #'org-ref-helm-insert-cite-link)

  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (call-process "foliate" nil 0 nil fpath))
        bibtex-completion-notes-path bib_notes
        bibtex-completion-bibliography
        '("~/Documents/Reza/BibTex/Zotero-mylib/Zotero-mylib.bib"
          "~/Documents/Reza/BibTex/Zotero-mylib/CalibreBib.bib")
        bibtex-completion-library-path zot_col
        bibtex-completion-pdf-field "file")

  ;; Based on SM-5
  (use-package! org-learn)

  (use-package! org-roam
    :after org
    :preface
    (defvar org-roam-directory (expand-file-name org_roam))
    :init
    (setq org-roam-v2-ack t)
    :commands
    (org-roam-buffer
     org-roam-setup
     org-roam-capture
     org-roam-node-find)
    :config
    (setq org-roam-mode-section-functions
          '(org-roam-backlinks-section
            org-roam-reflinks-section
            org-roam-unlinked-references-section)) ;; disable this because it still quite slow
    (require 'find-lisp)
    (setq org-id-extra-files (find-lisp-find-files org-roam-directory "\.org$"))
    (evil-define-key 'insert org-roam-mode-map
      (kbd "C-<tab>") 'company-capf)
    (define-key org-roam-mode-map
      [mouse-1] #'org-roam-visit-thing)
    (org-roam-setup))

  (after! org
    (use-package! org-collector)

    (defun org-hide-properties ()
     "Hide all org-mode headline property drawers in buffer. Could be slow if buffer has a lot of overlays."
     (interactive)
     (save-excursion
       (goto-char (point-min))
       (while (re-search-forward
               "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
         (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
           (overlay-put ov_this 'display "")
           (overlay-put ov_this 'hidden-prop-drawer t)))))

   (defun org-show-properties ()
     "Show all org-mode property drawers hidden by org-hide-properties."
     (interactive)
     (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t))

   (defun org-toggle-properties ()
     "Toggle visibility of property drawers."
     (interactive)
     (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
         (progn
           (org-show-properties)
           (put 'org-toggle-properties-hide-state 'state 'shown))
       (progn
         (org-hide-properties)
         (put 'org-toggle-properties-hide-state 'state 'hidden))))

  ;; Set hide properties as default behaviour
   (add-hook 'org-mode-hook #'org-hide-properties))

  (after! org-roam
    (setq orb-file-field-extensions '("pdf" "epub" "djvu" "mobi" "azw3"))
    (setq orb-preformat-keywords
          '(("citekey" . "=key=") "title" "cover" "url" "tags" "date" "abstract" "year" "journal" "note" "volume" "pages" "doi" "isbn" "issn" "publisher" "file" "author-or-editor" "keywords"))
    (setq org-roam-capture-templates
            '(("d" "default" plain
               (file "~/.config/chemacs/dotemacsen/Reza/orgtemplates/roamDefault.org")
               :if-new
               (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+TITLE: ${title}\n")
               :unnarrowed t)
              ("b" "bookref" plain
               (file "~/.config/chemacs/dotemacsen/Reza/orgtemplates/bookRef.org")
               :if-new
               (file+head "noter/${citekey}.org"
                          "#+TITLE: bref-${title}\n")
               :unnarrowed t)
              ("a" "articref" plain
               (file "~/.config/chemacs/dotemacsen/Reza/orgtemplates/articRef.org")
               :if-new
               (file+head "noter/${citekey}.org"
                          "#+TITLE: aref-${title}\n")
               :unnarrowed t)
              ("p" "people" plain
               (file "~/.config/chemacs/dotemacsen/Reza/orgtemplates/roamPeople.org")
               :if-new
               (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+TITLE: ${title}\n")
               :unnarrowed t))))

  (after! org-roam
        (setq org-roam-capture-ref-templates
              '(("r" "ref" plain
                 (file "~/.config/chemacs/dotemacsen/Reza/orgtemplates/webRef.org")
                 :if-new
                 (file+head "web/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+TITLE: web-${title}\n")
                 :unnarrowed t))))

  (after! org-roam
    (setq org-roam-graph-viewer "librewolf"))

  (use-package! org-roam-ui
    :preface
    (use-package! websocket)
    (use-package! simple-httpd)
    :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

  (after! org
    (require 'org-protocol)
    (require 'org-roam-protocol)

  ;; This part is taken from nobiot's fix org-protocol interaction with chrome in "Zero To Emacs"
    (load-file (expand-file-name "elisp/+org-protocol-check-filename-for-protocol.el" doom-private-dir))
    (advice-add 'org-protocol-check-filename-for-protocol :override '+org-protocol-check-filename-for-protocol))

  (use-package! org-roam-bibtex
    :defer t
    ;; :after org
    :config
    (require 'org-ref)
    (org-roam-bibtex-mode 1))

  (use-package! org-sort-tasks
    :commands org-sort-tasks)

  (use-package! org-transclusion
    :hook (org-mode . (lambda () (org-transclusion-mode)))
    :commands (org-transclusion-add)
    :config
    (set-face-attribute 'org-transclusion-fringe nil :foreground "green" :background "green")
    (set-face-attribute 'org-transclusion nil :inherit 'org-block))

  (use-package! org-noter
    ;; :after (:any org pdf-view)
    :config
    (setq
     ;; The WM can handle splits
     org-noter-notes-window-location 'other-frame
     ;; Please stop opening frames
     org-noter-always-create-frame nil
     ;; I want to see the whole file
     org-noter-hide-other nil
     ;; Everything is relative to the main notes file
     org-noter-notes-search-path (list org_noter))

    (evil-define-key 'normal org-noter-notes-mode-map (kbd "S <tab>") 'org-noter-sync-current-note))

  (use-package! org-journal
    :after org
    :hook (org-journal-mode . (lambda () (emojify-mode)))
    :commands
    (org-journal-open-current-journal-file
     org-journal-search-forever
     org-journal-search-calendar-week
     org-journal-search-calendar-month
     org-journal-search-calendar-year)
    :config
    (setq org-journal-carryover-items ""
          org-journal-enable-cache t
          org-journal-enable-encryption t
          org-journal-encrypt-journal t))

  (after! org-journal
      (setq org-journal-dir org_journal
            org-journal-file-format "%Y-%m-%d.org"
            org-journal-date-prefix "#+DATE: "
            org-journal-date-format "%A, %d %B %Y"
            org-extend-today-until 4))

  (defun org-journal-file-header-func (time)
    (concat
      (pcase org-journal-file-type
        (`daily "#+TITLE: Reza's Daily Journal\n#+STARTUP: content")
        (`weekly "#+TITLE: Reza's Weekly Journal\n#+STARTUP: folded")
        (`monthly "#+TITLE: Reza's Monthly Journal\n#+STARTUP: folded")
        (`yearly "#+TITLE: Reza's Yearly Journal\n#+STARTUP: folded"))))

  (after! org-journal
  (setq org-journal-file-header 'org-journal-file-header-func))

  ;; Uploading Images to Journal
  (defun rz/img-path-string (date)
    (interactive)
    (mapconcat (function (lambda (x) (concat (concat "\[\[" x) "\]\]\n")))
               (seq-filter (function (lambda (x) (cl-search date x)))
                           (directory-files-recursively "~/Pictures/wallpapers/"
                                                        "\\`[^.].*\\.jpg\\'"
                                                        )
                           )
               ""
               )
    )

  (defun rz/upload-imgs-to-journal ()
    "I use this function to attach images to the journal for the date that I choose"
    (interactive)
    (let ((date
           (replace-regexp-in-string "[^[:digit:]]" "" (org-read-date))))

      (append-to-file
       (concat "* Images\n" (rz/img-path-string date))
       nil
       (concat org-journal-dir (concat date ".org"))
       )
      )
    )

  (defvar org-journal--date-location-scheduled-time nil)
  (defun org-journal-date-location (&optional scheduled-time)
    (let ((scheduled-time (or scheduled-time (org-read-date nil nil nil "Date:"))))
      (setq org-journal--date-location-scheduled-time scheduled-time)
      (org-journal-new-entry t (org-time-string-to-time scheduled-time))
      (unless (eq org-journal-file-type 'daily)
        (org-narrow-to-subtree))
      (goto-char (point-max))))

  (defun rz/org-journal-today-entry-new ()
    "Insert new entry using org-roam-capture template"
    (interactive)
    (org-capture nil "jj"))

(defun rz/org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))

   (defun org-journal-save-entry-and-exit()
      (interactive)
      (save-buffer)
      (kill-buffer-and-window))

  (after! org-journal
    (define-key org-journal-mode-map (kbd "C-c C-c") 'org-journal-save-entry-and-exit))

  (use-package! org-sidebar
    :commands org-sidebar-toggle)

  (use-package! org-cliplink
    :commands (org-cliplink org-cliplink-capture))

  (use-package! org-chef
    :commands (org-chef-insert-recipe org-chef-edit-servings))

  (use-package! org-pomodoro
    :commands org-pomodoro
    :config
    (setq
     alert-user-configuration
     (quote ((((:category . "org-pomodoro")) libnotify nil)))
     org-pomodoro-length 25
     org-pomodoro-short-break-length 5))

  (after! org-pomodoro
    (defun rz/org-pomodoro-time ()
    "Return the remaining pomodoro time"
    (if (org-pomodoro-active-p)
        (cl-case org-pomodoro-state
          (:pomodoro
             (format "%d minutes - %s" (/ (org-pomodoro-remaining-seconds) 60) org-clock-heading))
          (:short-break
           (format "Short Break: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
          (:long-break
           (format "Long Break: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
          (:overtime
           (format "Overtime! %d minutes" (/ (org-pomodoro-remaining-seconds) 60))))
      "No active pomo")))

  (use-package! org-download
    :hook (dired-mode . (lambda () (org-download-enable)))
    :init
    (setq
     org-download-image-dir (concat org_file_dir "images/")
     org-download-method 'directory
     org-download-heading-lvl nil
     org-download-backend "wget"))
    ;; :bind
    ;; (:map org-mode-map
    ;;       (("s-Y" . org-download-screenshot)
    ;;        ("s-y" . org-download-yank))))

  (use-package! org-pandoc-import
    :after org)

  (use-package! org-recoll
    :commands
    (org-recoll-search
     org-recoll-update-index)
    :config
    (evil-define-key
      'normal org-recoll-mode-map
      (kbd "g ]") 'org-recoll-next-page
      (kbd "g [") 'org-recoll-previous-page
      (kbd "q") 'delete-window)
    )

  (use-package! org-randomnote
    ;; :bind ("C-c r". org-randomnote)
    :config
    (load-library "find-lisp")
    (setq org-randomnote-candidates
          (find-lisp-find-files org_journal "\.org$")))

;; [[file:config.org::*Keybinding][Keybinding:1]]
(map! :map org-export-stack-mode-map
      :ne "RET" #'org-export-stack-view)
;; Keybinding:1 ends here

;; [[file:config.org::*Configuration][Configuration:1]]
(after! org
  (setq org-export-async-init-file (expand-file-name "elisp/ox-init.el" doom-private-dir)))
;; Configuration:1 ends here

  (defun rz/org-mode-visual-fill ()
    (setq visual-fill-column-width 100
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

  (use-package! visual-fill-column
    :hook (org-mode . (lambda () (rz/org-mode-visual-fill))))

(advice-add 'text-scale-adjust
            :after #'visual-fill-column-adjust)

    (use-package! calibredb
      :commands calibredb-find-counsel
      :init
      (autoload 'calibredb "calibredb")

      (setq calibredb-size-show t))

  (after! calibredb
    (setq calibredb-root-dir "~/Documents/Calibre")
    (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)))

  (use-package! pdf-tools
    :defer t
    ;; :mode "\\.pdf\\'"
    :config
    (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
    (add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)
        (evil-define-key
          'normal pdf-view-mode-map
          (kbd "q") 'kill-this-buffer
          (kbd "u") 'pdf-view-scroll-down-or-previous-page
          (kbd "d") 'pdf-view-scroll-up-or-next-page)
    (pdf-loader-install))

  (use-package! pdfgrep
    :defer t)

  (use-package! doc-view
    :defer t
    :config
    (setq doc-view-continuous t)
    (add-to-list 'auto-mode-alist '("\\.djvu\\'" . doc-view-mode)))

  (use-package! nov
    :preface
    (defun rz/nov-font-setup ()
      (interactive)
      (face-remap-add-relative
       'variable-pitch
       :family "Amazon Ember"
       :height 1.0)
      (visual-line-mode 1)
      (visual-fill-column-mode 1))
    ;; :mode ("\\.\\(epub\\|mobi\\)\\'" . (lambda () (nov-mode)))
    :config
    (setq nov-text-width t)
    (add-hook 'nov-mode-hook 'rz/nov-font-setup))

  (use-package! calendar
    :init
    (general-setq holiday-islamic-holidays t)
    (general-setq holiday-hebrew-holidays nil)
    (general-setq calendar-chinese-all-holidays-flag nil)
    (general-setq holiday-solar-holidays nil)
    (general-setq holiday-bahai-holidays nil)
    :config
    (evil-define-key 'normal calendar-mode-map (kbd "J") 'org-journal-read-entry))

  (use-package! wttrin
    :commands wttrin)

  (use-package! notmuch
    :init
    (setq message-directory
          (expand-file-name mail_dir))
    (setq send-mail-function 'sendmail-send-it)
    ;; Send from correct email account
    (setq message-sendmail-f-is-evil t)
    ;; (setq message-sendmail-extra-arguments '("--read-envelope-from"))
    (setq mail-specify-envelope-from t)
    (setq mail-envelope-from 'header)
    (setq message-sendmail-envelope-from 'header)
    :config
    (setq sendmail-program
          (executable-find "msmtp"))
    (setq notmuch-show-logo nil)
    ;; Writing email
    (setq message-default-mail-headers "Cc: \nBcc: \n") ;; Always show BCC
    (setq notmuch-always-prompt-for-sender t)
    ;; PGP Encryption
    ;; (add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)
    ;; (setq notmuch-crypto-process-mime t)
    ;; postponed message is put in the following draft directory
    (setq message-auto-save-directory
          (expand-file-name draft_dir))
    (setq message-kill-buffer-on-exit t)
    ;; Saving sent mail in folders depending on from
    (setq notmuch-fcc-dirs
          '(("mnurrreza@gmail.com" . "mnurrreza-gmail/Sent")
            ("mnurreza@yahoo.co.id" . "mnurreza-yahoo/Sent")))
    ;; Show all tags in *notmuch-hello*
    (setq notmuch-show-all-tags-list t)
    (setq notmuch-search-oldest-first nil)
    (setq notmuch-show-indent-content nil)
    (setq notmuch-archive-tags '("-inbox", "-unread", "+archived"))
    (setq notmuch-saved-searches
          '((:name "unread"
                   :query "tag:unread"
                   :key "u"
                   :sort-order newest-first
                   :search-type 'tree)
            (:name "inbox-gmail"
                   :query "tag:inbox to:mnurrreza@gmail.com"
                   :key "g"
                   :sort-order newest-first
                   :search-type 'tree)
            (:name "inbox-yahoo"
                   :query "tag:inbox to:mnurreza@yahoo.co.id"
                   :key "y"
                   :sort-order newest-first
                   :search-type 'tree)
            (:name "inbox-uni"
                   :query "tag:inbox to:muhammad.nur.reza.amasyi@mail.ugm.ac.id"
                   :key "U"
                   :sort-order newest-first
                   :search-type 'tree)
            (:name "flagged"
                   :query "tag:flagged"
                   :key "f"
                   :sort-order newest-first)
            (:name "sent"
                   :query "tag:sent"
                   :key "t"
                   :sort-order newest-first)
            (:name "drafts"
                   :query "tag:draft"
                   :key "d"
                   :sort-order newest-first)
            (:name "all mail"
                   :query "*"
                   :key "a"
                   :sort-order newest-first
                   :search-type 'tree)))
    )

  (use-package! ol-notmuch)

  (use-package! notmuch-maildir
    :after notmuch
    :init
    (notmuch-maildir-inject-section))

  (use-package! notmuch-bookmarks
    :after notmuch
    :config
    (notmuch-bookmarks-mode))

  (use-package! bbdb
    :hook (bbdb-mode . (lambda () (evil-mode)))
    :custom
    (bbdb-mua-auto-update-p 'query)
    :config
    (setq bbdb-file (expand-file-name "bbdb/bbdb.el" org_file_dir))
    (add-hook 'mail-setup-hook 'bbdb-mail-aliases)
    (add-hook 'message-setup-hook 'bbdb-mail-aliases))

  (after! bbdb
    (evil-define-key 'normal bbdb-mode-map
      (kbd "s") 'bbdb-save
      (kbd "c") 'bbdb-create
      (kbd "?") 'bbdb-help
      (kbd "m") 'bbdb-mail
      (kbd "d") 'bbdb-dial
      (kbd "D") 'bbdb-delete-field-or-record
      (kbd "i") 'bbdb-insert-field
      (kbd "h") 'bbdb-info
      (kbd ";") 'bbdb-edit-foo
      (kbd "e") 'bbdb-edit-field
      (kbd "T") 'bbdb-display-records-completely
      (kbd "g d") 'bbdb-browse-url))

  (use-package! bbdb-vcard
    :after bbdb)

  (use-package! counsel-bbdb
    :defer t)

  (use-package! org-mime
    :commands notmuch-mua-new-mail
    :config
    (setq org-mime-export-options '(:section-numbers nil
                                  :with-author nil
                                  :with-toc nil)))

  (after! org-mime
  (add-hook 'org-mime-html-hook
            (lambda ()
              (org-mime-change-element-style
               "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                             "#E6E1DC" "#232323"))))
  ;; the following can be used to nicely offset block quotes in email bodies
  (add-hook 'org-mime-html-hook
            (lambda ()
              (org-mime-change-element-style
               "blockquote" "border-left: 2px solid gray; padding-left: 4px;"))))

  (after! message
    (add-hook 'message-send-hook 'org-mime-htmlize))

  (defun org-mime-compose (body fmt file &optional to subject headers)
    (require 'message)
    (let ((bhook
           (lambda (body fmt)
             (let ((hook (intern (concat "org-mime-pre-"
                                         (symbol-name fmt)
                                         "-hook"))))
               (if (> (eval `(length ,hook)) 0)
                   (with-temp-buffer
                     (insert body)
                     (goto-char (point-min))
                     (eval `(run-hooks ',hook))
                     (buffer-string))
                 body))))
          (fmt (if (symbolp fmt) fmt (intern fmt)))
          (files (org-element-map (org-element-parse-buffer) 'link
                   (lambda (link)
                     (when (string= (org-element-property :type link) "file")
                       (file-truename (org-element-property :path link)))))))
      (compose-mail to subject headers nil)
      (message-goto-body)
      (cond
       ((eq fmt 'org)
        (require 'ox-org)
        (insert (org-export-string-as
                 (org-babel-trim (funcall bhook body 'org)) 'org t)))
       ((eq fmt 'ascii)
        (require 'ox-ascii)
        (insert (org-export-string-as
                 (concat "#+TITLE:\n" (funcall bhook body 'ascii)) 'ascii t)))
       ((or (eq fmt 'html) (eq fmt 'html-ascii))
        (require 'ox-ascii)
        (require 'ox-org)
        (let* ((org-link-file-path-type 'absolute)
               ;; we probably don't want to export a huge style file
               (org-export-htmlize-output-type 'inline-css)
               (org-html-with-latex 'dvipng)
               (html-and-images
                (org-mime-replace-images
                 (org-export-string-as (funcall bhook body 'html) 'html t)))
               (images (cdr html-and-images))
               (html (org-mime-apply-html-hook (car html-and-images))))
          (insert (org-mime-multipart
                   (org-export-string-as
                    (org-babel-trim
                     (funcall bhook body (if (eq fmt 'html) 'org 'ascii)))
                    (if (eq fmt 'html) 'org 'ascii) t)
                   html)
                  (mapconcat 'identity images "\n")))))
      (mapc #'mml-attach-file files)))

  (use-package! slack
    :commands (slack-start)
    :init
    (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
    (setq slack-prefer-current-team t)
    :config
    ;; (slack-register-team
    ;;  :name "emacs-slack"
    ;;  :default t
    ;;  :token "xoxs-sssssssssss-88888888888-hhhhhhhhhhh-jjjjjjjjjj"
    ;;  :subscribed-channels '(test-rename rrrrr)
    ;;  :full-and-display-names t)

    ;; (slack-register-team
    ;;  :name "test"
    ;;  :token "xoxs-yyyyyyyyyy-zzzzzzzzzzz-hhhhhhhhhhh-llllllllll"
    ;;  :subscribed-channels '(hoge fuga))

    (evil-define-key 'normal slack-info-mode-map
      ",u" 'slack-room-update-messages)
    (evil-define-key 'normal slack-mode-map
      ",c" 'slack-buffer-kill
      ",ra" 'slack-message-add-reaction
      ",rr" 'slack-message-remove-reaction
      ",rs" 'slack-message-show-reaction-users
      ",pl" 'slack-room-pins-list
      ",pa" 'slack-message-pins-add
      ",pr" 'slack-message-pins-remove
      ",mm" 'slack-message-write-another-buffer
      ",me" 'slack-message-edit
      ",md" 'slack-message-delete
      ",u" 'slack-room-update-messages
      ",2" 'slack-message-embed-mention
      ",3" 'slack-message-embed-channel
      "\C-n" 'slack-buffer-goto-next-message
      "\C-p" 'slack-buffer-goto-prev-message
     (evil-define-key 'normal slack-edit-message-mode-map
      ",k" 'slack-message-cancel-edit
      ",s" 'slack-message-send-from-buffer
      ",2" 'slack-message-embed-mention
      ",3" 'slack-message-embed-channel)))

  (use-package! ox-slack
    :after ox)

  (use-package! alert
    :commands (alert)
    :init
    (setq alert-default-style 'libnotify))

  (use-package! circe
    :defer t)

  (use-package! twittering-mode
    :commands twit
    :config
    ;; Fix bug void-function
    (defalias 'epa--decode-coding-string 'decode-coding-string)
    (setq twittering-use-master-password t
          twittering-cert-file "/etc/ssl/certs/ca-certificates.crt"
          twittering-icon-mode t))

;; [[file:config.org::*Configuration][Configuration:1]]
  (use-package! diff-hl
    :config
    (setq diff-hl-flydiff-delay 0.05)
    ;; use margin instead of fringe
    (diff-hl-margin-mode))

  (add-hook! 'dired-mode-hook #'diff-hl-dired-mode-unless-remote)
  (add-hook! 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (add-hook! 'prog-mode-hook #'diff-hl-mode)
  (add-hook! 'diff-hl-mode-hook #'diff-hl-flydiff-mode)
;; Configuration:1 ends here

;; [[file:config.org::*Keybindings][Keybindings:1]]
(map! :map dired-mode-map "C-c C-r" #'dired-rsync)
;; Keybindings:1 ends here

;; [[file:config.org::*Configuration][Configuration:1]]
  (use-package! peep-dired
    :config
    (evil-define-key 'normal peep-dired-mode-map (kbd "g") 'peep-dired-scroll-page-down
                                               (kbd "G") 'peep-dired-scroll-page-up
                                               (kbd "j") 'peep-dired-next-file
                                               (kbd "k") 'peep-dired-prev-file)
    (add-hook 'peep-dired-hook 'evil-normalize-keymaps)
    (setq peep-dired-ignored-extensions '("mkv" "iso" "mp4")
          peep-dired-cleanup-eagerly t))
;; Configuration:1 ends here

;; [[file:config.org::*Configuration][Configuration:1]]
(map! :leader
      :desc "trashed" "ft" #'trashed)
;; Configuration:1 ends here

;; [[file:config.org::*Configuration][Configuration:1]]
  (use-package! notdeft
    :preface
    (defalias 'deft 'notdeft) ;; avoid Doom use of deft
    (defvar notdeft-directory nil)
    (defvar notdeft-directories nil)
    :config
    (add-to-list 'load-path (expand-file-name "straight/repos/notdeft/extras" straight-base-dir))
    (setq ;; Directory of xapian program must be absolute path
          notdeft-allow-org-property-drawers t ;; to treat :properties: on top as comment
          notdeft-directories '("~/org/roam")
          notdeft-xapian-home (expand-file-name "straight/repos/notdeft/xapian" straight-base-dir)
          notdeft-xapian-program (expand-file-name "straight/repos/notdeft/xapian/notdeft-xapian" straight-base-dir)
          notdeft-secondary-extensions '("md" "tex"))
    (evil-define-key 'normal notdeft-mode-map (kbd "q") 'quit-window))

  (add-hook! 'notdeft-load-hook #'notdeft-xapian-make-program-when-uncurrent)
;; Configuration:1 ends here
  (use-package! tramp
    :config
    (setenv "SHELL" "/bin/bash")
    (setq tramp-shell-prompt-pattern "\\(?:^\\|
  \\)[^]#$%>\n]*#?[]#$%>] *\\(�\\[[0-9;]*[a-zA-Z] *\\)*"))

  (use-package! vlf
    :after dired
    :config
    (require 'vlf-setup))

;; [[file:config.org::*Configuration][Configuration:1]]
  (use-package! elfeed
    :preface
    ;; Watch Video Using MPV
    (defun rz/elfeed-v-mpv (url)
      "Watch a video from URL in MPV"
      (async-shell-command (format "mpv %s" url)))
    (defun rz/elfeed-view-mpv (&optional use-generic-p)
      "Youtube-feed link"
      (interactive "P")
      (let ((entries (elfeed-search-selected)))
        (cl-loop for entry in entries
                 do (elfeed-untag entry 'unread)
                 when (elfeed-entry-link entry)
                 do (rz/elfeed-v-mpv it))
        (mapc #'elfeed-search-update-entry entries)
        (unless (use-region-p) (forward-line))))

    ;; Download Video using yt-dlp
    (defun rz/yt-dlp-it (url)
      "Downloads the URL in an async shell"
      (let ((default-directory "~/Videos"))
        (async-shell-command (format "yt-dlp %s" url))))
    (defun rz/elfeed-youtube-dl (&optional use-generic-p)
      "Youtube-DL link"
      (interactive "P")
      (let ((entries (elfeed-search-selected)))
        (cl-loop for entry in entries
                 do (elfeed-untag entry 'unread)
                 when (elfeed-entry-link entry)
                 do (rz/yt-dl-it it))
        (mapc #'elfeed-search-update-entry entries)
        (unless (use-region-p) (forward-line))))

    ;; Send entry to email for later read
    (defun rz/mail-todo (text &optional body)
      (interactive "sTodo: ")
      (compose-mail-other-window "mnurrreza@gmail.com" text)
      (mail-text)
      (if body
          (insert body))
      (message-send-and-exit))
    (defun rz/elfeed-mail-todo (&optional use-generic-p)
      "Mail this to myself for later reading"
      (interactive "P")
      (let ((entries (elfeed-search-selected)))
        (cl-loop for entry in entries
                 do (elfeed-untag entry 'unread)
                 when (elfeed-entry-title entry)
                 do (rz/mail-todo it (elfeed-entry-link entry)))
        (mapc #'elfeed-search-update-entry entries)
        (unless (use-region-p) (forward-line))))
    :commands elfeed
    :config
    ;; (defface elfeed-show-title-face '((t (:weight ultrabold :slant italic :height 1.5)))
    ;;     "title face in elfeed show buffer"
    ;;     :group 'elfeed)
    ;; (defface elfeed-show-author-face `((t (:weight light)))
    ;;   "title face in elfeed show buffer"
    ;;   :group 'elfeed)
    ;; (set-face-attribute 'elfeed-search-title-face nil
    ;;                     :foreground 'nil
    ;;                     :weight 'light)
    (setf url-queue-timeout 30)
    (evil-define-key 'normal elfeed-search-mode-map
      (kbd "w") 'rz/elfeed-view-mpv
      (kbd "d") 'rz/yt-dlp-it
      (kbd "m") 'rz/elfeed-mail-todo
      (kbd "b") 'elfeed-show-visit
      (kbd "y") 'elfeed-show-yank
      (kbd "r") 'elfeed-search-tag-all-read
      (kbd "u") 'elfeed-search-tag-all-unread
      (kbd "U") 'elfeed-update
      (kbd "+") 'elfeed-search-tag-all
      (kbd "-") 'elfeed-search-untag-all)
    (elfeed-org))

(add-hook! 'elfeed-show-mode-hook
  (visual-line-mode 1)(visual-fill-column-mode 1)(mixed-pitch-mode 1))
;; Configuration:1 ends here

;; [[file:config.org::*Org Interface (elfeed-org)][Org Interface (elfeed-org):1]]
  (after! elfeed-org
    (setq rmh-elfeed-org-files
          (list
           (expand-file-name
            "orgtemplates/elfeed.org"
            doom-private-dir))))

  (add-hook! 'elfeed-search-hook #'elfeed-org)
;; Org Interface (elfeed-org):1 ends here

;; [[file:config.org::*Configuration][Configuration:1]]
  (use-package! elfeed-goodies
    :after elfeed
    :init
    (elfeed-goodies/setup)
    :config
    (evil-define-key 'normal elfeed-show-mode-map
      (kbd "J") 'elfeed-goodies/split-show-next
      (kbd "K") 'elfeed-goodies/split-show-prev)
    (evil-define-key 'normal elfeed-search-mode-map
      (kbd "J") 'elfeed-goodies/split-show-next
      (kbd "K") 'elfeed-goodies/split-show-prev))
;; Configuration:1 ends here

  (use-package! org-web-tools
    :after org)

  (use-package! calctex
    :commands calctex-mode
    :init
    (add-hook 'calc-mode-hook #'calctex-mode))
  ;; :config
  ;;   (setq calctex-additional-latex-packages "
  ;; \\usepackage[usenames]{xcolor}
  ;; \\usepackage{soul}
  ;; \\usepackage{adjustbox}
  ;; \\usepackage{amsmath}
  ;; \\usepackage{amssymb}
  ;; \\usepackage{siunitx}
  ;; \\usepackage{cancel}
  ;; \\usepackage{mathtools}
  ;; \\usepackage{mathalpha}
  ;; \\usepackage{xparse}
  ;; \\usepackage{arevmath}"
  ;;         calctex-additional-latex-macros
  ;;         (concat calctex-additional-latex-macros
  ;;                 "\n\\let\\evalto\\Rightarrow")))

  (use-package! sx
    :commands
    (sx-ask
     sx-tab-all-questions
     sx-open-link
     sx-tab-unanswered-my-tags
     sx-inbox
     sx-search))
    ;; :config)
    ;; (bind-keys :prefix "C-c s"
    ;;            :prefix-map my-sx-map
    ;;            :prefix-docstring "Global keymap for SX."
    ;;            ("q" . sx-tab-all-questions)
    ;;            ("i" . sx-inbox)
    ;;            ("o" . sx-open-link)
    ;;            ("u" . sx-tab-unanswered-my-tags)
    ;;            ("a" . sx-ask)
    ;;            ("s" . sx-search)))

  (after! sx
    (evil-define-key 'normal
      sx-question-list-mode-map
      (kbd "<return>") 'sx-display
      (kbd "k") 'sx-question-list-previous
      (kbd "j") 'sx-question-list-next
      (kbd "A") 'sx-answer
      (kbd "C") 'sx-comment
      (kbd "D") 'sx-downvote
      (kbd "U") 'sx-upvote
      (kbd "E") 'sx-edit
      (kbd "W") 'sx-tab-week
      (kbd "g d") 'sx-visit-externally)
    (evil-define-key 'normal
      sx-question-mode-map
      (kbd "] ]") 'sx-question-mode-next-section
      (kbd "[ [") 'sx-question-mode-previous-section
      (kbd "g R") 'sx-question-mode-refresh))

;; [[file:config.org::*Configuration][Configuration:1]]
  (use-package! howdoyou
    :commands howdoyou-query)
;; Configuration:1 ends here

;; [[file:config.org::*Keybindings][Keybindings:1]]
  (map! :map howdoyou-mode-map :n
        "q" #'quit-window
        "g ]" #'howdoyou-next-link
        "g [" #'howdoyou-previous-link
        "g r" #'howdoyou-reload-link
        "g R" #'howdoyou-go-back-to-first-link)
;; Keybindings:1 ends here

  (use-package! discover-my-major
    :commands (discover-my-major discover-my-mode))

  (use-package! emacs-everywhere
    :commands emacs-everywhere
    :config
    (setq emacs-everywhere-markdown-windows '("Stack Exchange" "Stack Overflow" "Reddit" "Pull Request" "Issue" "Comparing .*\\.\\.\\." "Discord")
          emacs-everywhere-markdown-apps '("Discord")))

  (use-package! helm-system-packages
    :commands helm-system-packages
    :config
    (require 'em-tramp)
    (setq password-cache t)
    (setq password-cache-expiry 3600))

  (use-package! system-packages
    :defer t)

  (use-package! wgrep)

  (use-package! auto-minor-mode)

  (use-package! link-hint
    :defer t
    :config
    (setq browse-url-browser-function 'browse-url-generic)
    (setq browse-url-generic-program "librewolf")
    ;; Open urls in a new tab instead of window; can also be set in the config file
    (setq browse-url-generic-args '("--target" "tab"))
    (evil-define-key 'normal
      (kbd "SPC y") 'link-hint-copy-link
      (kbd "SPC F") 'link-hint-open-link)
    )
