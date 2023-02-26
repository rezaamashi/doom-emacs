;; [[file:../config.org::*User Credentials][User Credentials:1]]
(setq user-full-name "Reza A'masyi"
      user-mail-address "mnurrreza@gmail.com")
;; User Credentials:1 ends here

;; [[file:../config.org::*Org Mode Features][Org Mode Features:1]]
  (defvar org_file_dir (concat (getenv "HOME") "/org/"))
  (defvar org_agenda_dir (concat (getenv "HOME") "/org/Orgzly/"))

;; Org journal weekly review
  (defvar org_journal_weekly_dir (concat (getenv "HOME") "/org/journals/weekly/"))
;; Org Mode Features:1 ends here

;; [[file:../config.org::*Finance][Finance:1]]
(defvar hledger_journal_dir (concat (getenv "HOME") "/org/hledger/"))
;; Finance:1 ends here

;; [[file:../config.org::*Zettelkasten Setup][Zettelkasten Setup:1]]
  (defvar org_journal (concat (getenv "HOME") "/org/journals/"))
  (defvar org_noter (concat (getenv "HOME") "/org/roam/noter"))
  (defvar org_roam (concat (getenv "HOME") "/org/roam/"))
  (defvar bib_notes (concat (getenv "HOME") "/org/roam/bib_notes/bib_notes.org"))
  (defvar bib_files '("~/Documents/Reza/BibTex/Zotero-mylib/CalibreBib.bib"
                      "~/Documents/Reza/BibTex/Zotero-mylib/Zotero-mylib.bib"))
  (defvar zot_col (concat (getenv "HOME") "/Documents/Reza/BibTex/Zotero-mylib/files/Input"))
;; Zettelkasten Setup:1 ends here

;; [[file:../config.org::*Project Directory][Project Directory:1]]
  (defvar proj_dir (concat (getenv "HOME") "/Documents/Reza/Project"))
  (defvar proj_file (expand-file-name "projectile/projectile.projects"  doom-private-dir))
;; Project Directory:1 ends here

;; [[file:../config.org::*Email][Email:1]]
  (defvar draft_dir (concat (getenv "HOME") "/Maildir/draft/"))
  (defvar mail_dir (concat (getenv "HOME") "/Maildir/"))
  (defvar mail_script (concat (getenv "HOME") "/.local/script/email"))
;; Email:1 ends here
