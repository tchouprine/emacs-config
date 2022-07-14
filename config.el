;$DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Alexandre Tchourpine"
      user-mail-address "alexandret@jam.gg")
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))


;; (setq doom-font (font-spec :family "Share Tech Mono" :style "Regular" :size 18 ))
(setq doom-font (font-spec :family "JetBrains Mono" :style "SemiBold" :size 16 ))
(setq doom-theme 'doom-one-light)
(solaire-global-mode +1)
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


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
(after! projectile
(projectile-register-project-type 'npm '("package.json")
                                  :project-file "package.json"
                                  :compile "yarn"
                                  :run "yarn start")
)
;; PRETTIER CONFIG
(setq prettier-js-args '(
  "--trailing-comma" "es5"
  "--tabWidth" "4"
  "--semi" "false"
  "--single-quote" "true"
  ))
(add-hook 'js-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook 'prettier-js-mode)
;; SYNC GMAIL WITH ORG
(require 'org-gcal)
(setq org-gcal-client-id "735889577127-ebobr6a5cl3ak1i9g3ote0kl6ma5g9tc.apps.googleusercontent.com"
      org-gcal-client-secret "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"
      org-gcal-recurring-events-mode 'nested
      org-gcal-fetch-file-alist '(("alexandret@jam.gg" .  "~/org/schedule.org")
                                 ))

(map! :leader
      :desc "new todo for project"
      "p j a"
       #'org-projectile-capture-for-current-project
)

(map! :leader
      :desc "checkout to branch"
      "g b"
      #'magit-checkout
      )

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'auto-mode-alist '("src\\/.*\\.tsx\\'" . rjsx-mode))
;; (add-to-list 'auto-mode-alist '("com ponents\\/.*\\.tsx\\'" . rjsx-mode))

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

(map! "s-."  #'tide-jump-to-definition)

(setq org-todo-keywords
'((sequence "TOREAD(r)" "|" "READING(p)" "|" "DONE(d)")
(sequence "TODO(t)" "|" "DOING(p)" "|" "DONE(d)")
(sequence "IDEA(i)" "|" "THINKING(p)" "|" "DONE(d)")
(sequence "DISCUSS(d)" "|" "DISCUSSED(p)" "|" "DONE(d)")
      ))

(after! org-roam
  (require 'org-roam-protocol)
  )
