;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

(setq user-full-name "Alex Chuprin"
      user-mail-address "btnevermind@gmail.com")

(setq doom-font (font-spec :family "Fira Code" :style "Light"  :size 18 ))

;; Theme
(add-to-list 'load-path "~/.emacs.d/modus-themes")
(require 'modus-themes)
(load-theme 'modus-operandi t t)
(enable-theme 'modus-operandi)
(setq doom-theme 'modus-operandi-deuteranopia)

;; Misc
(setq display-line-numbers-type t) ;; option: relative
(setq auto-save-default t)
(setq org-startup-align-all-tables t)
(setq select-enable-clipboard t
      select-enable-primary t)
(add-to-list 'exec-path "${sqlite3}/bin")
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Org
(after! org
  (setq org-directory "~/org/")
  (setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
  (setq org-scheduled-past-days 0 )
  (add-to-list 'org-capture-templates
               '("c" "capturing stuff"
                 entry
                 (file+headline "~/org/captures.org" "Capturing")
                 "* CAPTURED %?"
                 ))
  (setq org-todo-keywords
        '(
          (sequence "TOREAD(r)" "READING(p)" "|" "FINISHED READING(d)")
          (sequence "CAPTURED(c)" "PROCESSING" "SUMMARIZING" "DISTILLING" "|" "DONE(d)")
          (sequence "TODO(t)"  "DOING(p)" "|" "DONE(d)")
          (sequence "IDEA(i)"  "THINKING(p)" "|" "DONE(d)")
          (sequence "DISCUSS(d)" "BEING DISCUSSED(p)" "|" "DISCUSSED(d)")
          (sequence "[ ](t)" "|" "[x](d)")
          ))

  (use-package! org-glossary
    :hook (org-mode . org-glossary-mode))

  (use-package! org-pandoc-import )
  )

(after! org-roam
  (require 'org-roam-protocol)
  )

;; JS WORLD STUFF
(after! projectile
  (projectile-register-project-type 'npm '("package.json")
                                    :project-file "package.json"
                                    :compile "yarn"
                                    :run "yarn start")
  )
(setq prettier-js-args '(
                         "--trailing-comma" "es5"
                         "--tabWidth" "4"
                         "--semi" "false"
                         "--single-quote" "true"
                         ))
(add-hook 'js-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook 'prettier-js-mode)
(add-hook 'typescript-tsx-mode-hook 'prettier-js-mode)

;; MAPPINGS
(map! :leader
      :desc "new todo for project"
      "p j a"
      #'org-projectile-capture-capture-for-current-project
      )

(map! :leader
      :desc "checkout to branch"
      "g b"
      #'magit-checkout
      )

(map! :leader
      :desc "rename"
      "c R"
      #'lsp-rename)

(map! :leader
      :desc "find references"
      "c r"
      #'lsp-find-references)



(map! :leader "g k"
      (defun magit-add-current-buffer-to-kill-ring ()
        "Show the current branch in the echo-area and add it to the `kill-ring'."
        (interactive)
        (let ((branch (magit-get-current-branch)))
          (if branch
              (progn (kill-new branch)
                     (message "%s" branch))
            (user-error "There is no current branch"))))
      )
(map! "s-."  #'+company/complete)
(map! "M-o"  #'ace-window)





;; RSS
(add-hook! 'elfeed-search-mode-hook 'elfeed-update)

;; SRS config
;; Org-capture templates
(setq org-my-anki-file "~/org/roam/anki.org")
;;(add-to-list 'org-capture-templates
;;'("a" "Anki basic"
;;entry
;;(file+headline org-my-anki-file "Dispatch Shelf")
;;"* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: ONE BIG DECK\n:END:\n** Front\n%?\n** Back\n%x\n"))
;;(add-to-list 'org-capture-templates
;;'("A" "Anki cloze"
;;entry
;;(file+headline org-my-anki-file "Dispatch Shelf")
;;"* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: ONE BIG DECK\n:END:\n** Text\n%x\n** Extra\n"))
;;(add-to-list 'org-capture-templates
;;'("S" "Solving template"
;;entry
;;(file+headline "~/org/journal.org" "Solving")
;;"* Solving %\i  %i\n  %a \n %^{PROBLEM_NAME} \n"
;;))

;; SYNC GMAIL WITH ORG
;;(require 'org-gcal)
;;(setq org-gcal-client-id "735889577127-ebobr6a5cl3ak1i9g3ote0kl6ma5g9tc.apps.googleusercontent.com"
;;      org-gcal-client-secret "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"
;;      org-gcal-recurring-events-mode 'nested
;;      org-gcal-fetch-file-alist '(("alexandret@jam.gg" .  "~/org/schedule.org")
;;                                  ))
