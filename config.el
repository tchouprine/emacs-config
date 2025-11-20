;;; User Info
(setq user-full-name "Alex Chuprin"
      user-mail-address "btnevermind@gmail.com")

;;; Theme & Fonts
(setq doom-theme 'doom-homage-white)
(setq doom-font (font-spec :family "JetBrains Mono" :size 14 :weight 'light))
(setq doom-serif-font (font-spec :family "Merriweather" :size 18 :weight 'light))
(setq doom-variable-pitch-font (font-spec :family "EtBembo" :size 20))

;;; Mac Modifiers
(setq mac-right-command-modifier 'hyper)
(setq mac-right-option-modifier 'meta)

;;; Misc Settings
(setq auto-save-default t)
(setq org-startup-align-all-tables t)
(setq select-enable-clipboard t
      select-enable-primary t)
(add-to-list 'exec-path "${sqlite3}/bin")
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq-default line-spacing 0.3)

;;; Flycheck (tame it for elisp)
(after! flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;;; Eglot Enhancements
(use-package! eldoc-box
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode))

(after! eglot
  (setq eglot-inlay-hints-mode t)
  (setq eldoc-echo-area-use-multiline-p nil))

;;; Org Mode
(after! org
  (setq org-directory "~/org/")
  (setq org-agenda-files (directory-files-recursively "~/org/roam" "\\.org$"))
  (setq org-scheduled-past-days 0)

  (defun my-org-capture-idea ()
    (interactive)
    (let* ((file-path "~/org/ideas.org")
           (org-buffer (find-file-noselect file-path))
           (temp-buffer-name "*org-capture-idea-temp*")
           (temp-buffer-window (get-buffer-window temp-buffer-name)))
      (with-current-buffer org-buffer
        (let* ((headings (org-element-map (org-element-parse-buffer 'headline) 'headline
                           (lambda (headline)
                             (if (= (org-element-property :level headline) 1)
                                 (org-element-property :raw-value headline)
                               nil))))
               (selected-heading (ivy-read "Choose or create a heading: " headings :require-match nil))
               (current-time (format-time-string "[%Y-%m-%d %H:%M]"))
               (idea-text (read-string "Enter your idea: ")))
          (goto-char (point-min))
          (unless (re-search-forward (format "^\\* %s$" (regexp-quote selected-heading)) nil t)
            (goto-char (point-max))
            (insert (format "\n* %s\n" selected-heading)))
          (org-end-of-subtree)
          (insert (format "\n** %s\n%s" current-time idea-text))
          (save-buffer)))))

  (add-to-list 'org-capture-templates
               '("i" "Idea" entry (function my-org-capture-idea)
                 "" :empty-lines 1 :immediate-finish t :prepend nil))

  (add-to-list 'org-capture-templates
               '("c" "capturing stuff"
                 entry
                 (file+headline "~/org/captures.org" "Capturing")
                 "* CAPTURED %?")))
;;; Bike Links
(defun open-bike-link-and-focus (path)
  "Open a bike:// link and bring Bike to the foreground."
  (let ((script (format
                 "open location \"bike://%s\"
                 delay 0.2
                 tell application \"Bike\" to activate"
                 path)))
    (do-applescript script)))

(org-link-set-parameters
 "bike"
 :follow #'open-bike-link-and-focus
 :export (lambda (path desc format)
           (cond
            ((eq format 'html) (format "<a href=\"bike://%s\">%s</a>" path desc))
            ((eq format 'latex) (format "\\href{bike://%s}{%s}" path desc))
            (t (format "bike://%s" path)))))

;;; Keybindings
(map! :leader
      :desc "new todo for project"
      "p j a"
      #'org-projectile-capture-capture-for-current-project)

(map! :leader
      :desc "checkout to branch"
      "g b"
      #'magit-checkout)

(map! :leader
      :desc "rename"
      "c R"
      #'lsp-rename)

(map! :leader
      :desc "find references"
      "c r"
      #'lsp-find-references)

;;(require 'nano-theme)
;;(require 'nano-modeline)
;;(load-theme 'nano-light t)
;;(load-theme 'doom-earl-grey)
