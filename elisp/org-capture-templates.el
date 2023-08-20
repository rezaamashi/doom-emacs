;; [[file:../config.org::*Templates][Templates:2]]
(with-eval-after-load 'org
 (setq org-capture-templates
     ;; Tasks
   `(("t" "Tasks / Projects")
     ("tt" "Today" entry (file+olp ,(concat org_agenda_dir "next.org") "Today's Tasks")
      "* NEXT %?\nSCHEDULED:%^T  %U\n  %a\n  %i" :empty-lines 1)
     ("td" "Today Deadline" entry (file+olp ,(concat org_agenda_dir "next.org") "Today's Tasks")
      "* TODO %? \nDEADLINE: %^T\n %U" :empty-lines 1)
     ("ts" "Inbox Scheduled" entry (file+olp ,(concat org_agenda_dir "inbox.org") "Tasks")
      "* TODO %?\nSCHEDULED:%^T  %U\n  %a\n  %i" :empty-lines 1)
     ("tD" "Inbox Deadline" entry (file+olp ,(concat org_agenda_dir "inbox.org") "Tasks")
      "* TODO %? \nDEADLINE: %^T\n %U" :empty-lines 1)
     ("tw" "Wait deadline" entry (file+olp ,(concat org_agenda_dir "waiting.org") "Waiting For")
      "* WAIT %? From _%^{Delegated To}_ \nDEADLINE: %^T\n %U\n %a" :empty-lines 1)

     ;; Catchall for faster capture "SPC-x-x"
     ("x" "Inbox" entry (file+olp ,(concat org_agenda_dir "inbox.org") "Everything/Notes")
      "* %? \n %U\n %a" :empty-lines 1)

     ;; Events
     ("e" "Event" entry (file ,(concat org_agenda_dir "gcal/events.org"))
      "* %?\n %U":empty-lines 1)

     ;; Reading Lists
     ("r" "Reading List")
     ("ra" "Article" entry
      (file+olp ,(concat org_agenda_dir "reading_list.org") "Journal Article")
      "* RD %? \n%U\n%a" :empty-lines 1)
     ("rb" "Books" entry
      (file+olp ,(concat org_agenda_dir "reading_list.org") "Books")
      "* RD %? \n%U\n%a" :empty-lines 1)

     ;; Birthdays
     ("b" "Birthdays")
     ("br" "Relatives/Family" entry
      (file+olp ,(concat org_agenda_dir "birthdays.org") "Relatives")
      "* %^{Who?} \n%^{Birthday}t\n%U" :empty-lines 1 :immediate-finish 1)
     ("ba" "Acquintances" entry
      (file+olp ,(concat org_agenda_dir "birthdays.org") "Acquintances")
      "* %^{Who?} \n%^{Birthday}t\n%U" :empty-lines 1 :immediate-finish 1)
     ("bf" "Friends" entry
      (file+olp ,(concat org_agenda_dir "birthdays.org") "Friends")
      "* %^{Who?} \n%^{Birthday}t\n%U" :empty-lines 1 :immediate-finish 1)
     ("bo" "Others" entry
      (file+olp ,(concat org_agenda_dir "birthdays.org") "Others")
      "* %^{Who?} \n%^{Birthday}t\n%U" :empty-lines 1 :immediate-finish 1)

     ;; workflow
     ("m" "Meeting" entry
      (file+olp+datetree ,(concat org_file_dir "meetings.org") "Active")
      "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
      :clock-in :clock-resume :empty-lines 1)
     ("E" "Emails")
     ("Er" "Read Later" entry
      (file+olp ,(concat org_agenda_dir "mail.org") "Read Later")
      (file ,(concat doom-private-dir "orgtemplates/mailreadlater.org"))
      :empty-lines 1 :immediate-finish t)
     ("Ef" "Follow Up" entry (file+olp ,(concat org_agenda_dir "mail.org") "Follow Up")
      (file ,(concat doom-private-dir "orgtemplates/mailfollowup.org"))
      :empty-lines 1 :immediate-finish t)
     ("Es" "Send Mail" entry
      (file+olp ,(concat org_agenda_dir "mail.org") "Compose Mail")
      (file ,(concat doom-private-dir "orgtemplates/mailsendmail.org"))
      :empty-lines 1 :immediate-finish t)

     ;; Tracking
     ("M" "Metrics Capture")
     ("Mw" "Weight" table-line
      (file+headline ,(concat org_file_dir "weight.org") "Weight")
      "| %U | %^{Weight} | %^{Notes} |" :immediate-finish t)
     ("Mp" "PE" table-line
      (file+headline ,(concat org_agenda_dir "pe.org") "Measurements")
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
      (function rz/org-journal-date-location)
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
      (file ,(concat org_journal_weekly_dir "2022.org"))
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
     ("Q" "[pro] Web Quote" entry (file+olp ,(concat org_agenda_dir "inbox.org") "Web Quote")
      "* %^{Quote from/Who said this}\n:PROPERTIES:\n:SOURCE: %:annotation\n:CREATED_AT: %U\n:END:\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n%^{What do you think about this quote}"
      :prepend t :empty-lines 1
      :immediate-finish t)
     ("L" "[pro] Web Link" entry (file+olp ,(concat org_agenda_dir "reading_list.org") "Web")
      "* RD [[%:link][%:description]]\n%U\n%^{What is the gist of it} "
      :prepend t :empty-lines 1
      :immediate-finish t)
     ("W" "[pro] Web Reading List" entry
      (file+olp ,(concat org_agenda_dir "reading_list.org") "Web")
      "* RD %? \n%U\n%a" :empty-lines 1))))
;; Templates:2 ends here
