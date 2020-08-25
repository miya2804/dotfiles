(defconst emacs-start-time (current-time))

(setq package-enable-at-startup nil
      auto-window-vscroll nil)



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Functions and Variables
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(90))))

(defun set-my-default-faces ()
  "Can be used to set a default faces if the themes isn't installed."
  (interactive)
  (custom-set-faces
   '(font-lock-function-name-face ((t (:foreground "brightgreen"))))
   '(hl-line ((t (:background "gray25"))))
   '(web-mode-html-tag-bracket-face ((t (:foreground "ghost white"))))
   '(web-mode-html-tag-face ((t (:foreground "pale green")))))
  (set-face-background 'default "gray13")
  (set-face-foreground 'default "ghost white"))

(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-char (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))

(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Environment
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

;;;; install use-package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;;(add-to-list 'package-archives '("maralade" . "https://marmalade-repo.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq load-path (cons "~/.emacs.d/elisp" load-path))

  (require 'use-package)

  ;;;; debug
  (if init-file-debug
      (setq use-package-verbose t
            use-package-expand-minimally nil
            use-package-compute-statistics t
            debug-on-error t)
    (setq use-package-verbose nil
          use-package-expand-minimally t))

  ;;;; language
  ;; (set-language-environment "Japanese")
  ;; (set-local-environment nil)
  ;; (prefer-coding-system 'utf-8)
  ;; (set-keyboard-coding-system 'utf-8)
  ;; (set-terminal-coding-system 'utf-8)
  ;; (set-default 'buffer-file-cording-system 'utf-8)

  ;;;; proxy
  ;;(setq url-proxy-services '(("http" . "proxy.hoge.com:8080"))) ;; proxy

  ;;;; custom file
  (setq custom-file (locate-user-emacs-file "custom.el")))



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

;; -------------------------------------
;; Etc

;;;; visualization of space and tab
;;(global-whitespace-mode 1)

(setq-default tab-width 4 indent-tabs-mode nil)

;;;; scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control . 40)))
      mouse-wheel-progressive-speed nil)
(setq scroll-conservatively 1)
(setq scroll-margin 5)

;;;; windmove setting
;;(windmove-default-keybindings) ;; use shift+arrow
;;(windmove-default-keybindings 'meta) ;; use alt+arrow

;;;; my-keybind
(global-set-key (kbd "C-o") 'other-window-or-split)
(global-set-key (kbd "C-c r") 'window-resizer)
(global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)

;;;; backup (xxx~)
;; execution on or off
(setq make-backup-files t)
;; change directory
(setq backup-directory-alist '((".*" . "~/.emacs.d/.ehist/")))
;; save multiple backupfiles
(setq version-control     t) ;; exucution on or off
(setq kept-new-versions   2) ;; latest number
(setq kept-old-versions   1) ;; oldest number
(setq delete-old-versions t) ;; delete out of range

;;;; auto-save (#xxx#)
(setq auto-save-timeout 10) ;; 10sec (def:30)
(setq auto-save-interval 100) ;; 100char (def:300)
(setq delete-auto-save-files t) ;; successful completion
;;;; create auto-save file in ~/.emacs.d/.ehist
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/.ehist/" t)))

;;;; lockfile (.#xxx)
;; execution on of off
(setq create-lockfiles nil)

;;;; open bufferlist on current window
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;;;; new frame
(global-set-key (kbd "C-c n") 'make-frame)

;; -------------------------------------
;; Appearance

;;;; hide startup message
(setq inhibit-startup-message t)

;;;; hide *scratch* buffer message
(setq initial-scratch-message nil)

;;;; hide menu bar
(menu-bar-mode 0)

;;;; hide tool bar
(if (display-graphic-p)
    (tool-bar-mode 0))

;;;; show full path in title
(setq frame-title-format "%f")

;;;; region highlight
(transient-mark-mode t)

;;;; alpha
(if (display-graphic-p) (set-alpha 90))

;;;; window size settings
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; (defun set-frame-size-according-to-resolution ()
;;   (interactive)
;;   (if (display-graphic-p)
;;   (progn
;;     ;; use 120 char wide window for largeish displays
;;     ;; and smaller 80 column windows for smaller displays
;;     ;; pick whatever numbers make sense for you
;;     (if (> (x-display-pixel-width) 1280)
;;            (add-to-list 'default-frame-alist (cons 'width 120))
;;            (add-to-list 'default-frame-alist (cons 'width 50)))
;;     ;; for the height, subtract a couple hundred pixels
;;     ;; from the screen height (for panels, menubars and
;;     ;; whatnot), then divide by the height of a char to
;;     ;; get the height we want
;;     (add-to-list 'default-frame-alist
;;          (cons 'height (/ (- (x-display-pixel-height) 200)
;;                              (frame-char-height)))))))
;; (set-frame-size-according-to-resolution)

;;;; display-time
;; (setq display-time-day-and-date nil)
;; (setq display-time-24hr-format t)
;; (display-time)

;; -------------------------------------
;; Built-in packages

;;;; display line number
(if (version<= "26.0.50" emacs-version)
    (progn
      (add-hook 'after-init-hook 'global-display-line-numbers-mode)
      (setq-default indicate-empty-lines nil)
      (setq-default indicate-buffer-boundaries 'left))
  (progn
    ;;;; linum
    (add-hook 'after-init-hook 'global-linum-mode)
    (with-eval-after-load global-linum-mode (setq linum-format "%3d "))))

;;;; hl-line
(add-hook 'after-init-hook 'global-hl-line-mode)

;;;; org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-directory "~/Dropbox/document/org")
(setq org-agenda-files '("~/Dropbox/document/org/agenda/"))
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-startup-truncated nil)
;; org-dir外のrefile設定(bufferで開いていれば指定可能)
;; cf. https://www.emacswiki.org/emacs/OrgMode#toc21
(defun mhatta/org-buffer-files ()
  "Return list of opened Org mode buffer files"
  (mapcar (function buffer-file-name)
          (org-buffer-list 'files)))
(setq org-refile-targets
      '((nil :maxlevel . 3)
          (mhatta/org-buffer-files :maxlevel . 1)
          (org-agenda-files :maxlevel . 3)))
;; templates
(setq org-capture-templates
      '(("a" "Memoｃ⌒っﾟωﾟ)っφ　ﾒﾓﾒﾓ..."
         entry (file+headline "memos.org" "MEMOS")
         "* %U\n  %?"
         :empty-lines 1 :jump-to-captured 1)
        ("n" "Notes....φ(・ω・｀ )ｶｷｶｷ"
         entry (file+headline org-default-notes-file "NOTES")
         "* %?\n  Entered on %U\n  %a"
         :empty-lines 1)
        ("m" "Minutes( ´・ω) (´・ω・) (・ω・｀) (ω・｀ )"
         entry (file+datetree "minutes.org" "MINUTES")
         "* %?\n  Entered on %T\n"
         :empty-lines 1 :jump-to-captured 1)))
;; notes.orgを確認できる関数定義,キーへのbind
(defun show-org-buffer (file)
  "Show an org-file FILE on the current buffer."
  (interactive)
  (if (get-buffer file)
      (let ((buffer (get-buffer file)))
        (switch-to-buffer buffer)
        (message "%s" file))
    (find-file (concat org-directory file))))
(global-set-key (kbd "C-M-^") '(lambda () (interactive) (show-org-buffer "/notes.org")))

;;;; paren
;; illuminate corresponding brackets
(add-hook 'after-init-hook 'show-paren-mode)
(setq show-paren-style 'mixed)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-when-point-in-periphery t)
;; custom-face
(with-eval-after-load 'doom-dracula-theme
  (custom-set-faces
   '(show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c"))))
   ))

;; -------------------------------------
;; Fonts

(when (display-graphic-p)
  (when (x-list-fonts "SourceHanCodeJP")
    ;;;; create fontset
    (create-fontset-from-ascii-font "SourceHanCodeJp-9:weight=normal:slant=normal" nil "SourceHanCodeJp")
    ;;;; set font
    (set-fontset-font "fontset-SourceHanCodeJp" 'unicode "SourceHanCodeJp" nil 'append)
    ;;;; apply fontset to frame
    (add-to-list 'default-frame-alist '(font . "fontset-SourceHanCodeJp"))))



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Libraries
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(use-package diminish :ensure t :demand t)
(use-package use-package-ensure-system-package :ensure t :demand t)



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Packages
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

;;;; all-the-icons
;; Make dependent with doom-themes.
;; Fonts install ->  "M-x all-the-icons-install-fonts"
(with-eval-after-load 'doom-modeline
  (use-package all-the-icons :ensure t :defer t))

;;;; anzu
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :hook (after-init . global-anzu-mode)
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :config
  (setq anzu-search-threshold 1000)
  (setq anzu-replace-threshold 1000)
  (setq anzu-minimum-input-length 3)
  (with-eval-after-load 'migemo
    (setq anzu-use-migemo t)))

;;;; dashborad
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  ;; set the title
  (when (eq system-type 'gnu/linux)
    (setq dashboard-banner-logo-title
          (concat "Welcome to Emacs " emacs-version
                  " - "
                  "Kernel " (shell-command-to-string "uname -smo"))))
  ;; set the bunner
  ;; value can be
  ;; 'official which displays the official emacs logo
  ;; 'logo which displays an alternative emacs logo
  ;; 1, 2 or 3 which displays one of the text banners
  ;; "path/to/your/image.png" which displays whatever image you would prefer
  (setq dashboard-startup-banner 'logo)
  ;; centering
  (setq dashboard-center-content t)
  ;; use icons
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  ;; init-info (default: init time)
  (setq dashboard-set-init-info t)
  ;;(setq dashboard-init-info "This is an init message!")
  ;; footer
  (setq dashboard-set-footer t)
  ;;(setq dashboard-footer-messages '("Dashboard is pretty cool!"))
  ;;(setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
  ;;                                                   :height 1.1
  ;;                                                   :v-adjust -0.05
  ;;                                                   :face 'font-lock-keyword-face))
  )

;;;; doom-modeline
;; https://github.com/seagle0128/doom-modeline
;; Make dependent with doom-themes.
(with-eval-after-load 'doom-themes
  (use-package doom-modeline
    :ensure t
    :hook (after-init . doom-modeline-mode)
    :config
    (line-number-mode 0)
    (column-number-mode 1)
    (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
    (setq doom-modeline-minor-modes nil)
    (setq doom-modeline-buffer-encoding t)
    ;; display env version
    (setq doom-modeline-env-version t)
    (setq doom-modeline-env-load-string "...")
    ;; icon
    (setq doom-modeline-icon (display-graphic-p))
    (setq doom-modeline-major-mode-icon t)
    (setq doom-modeline-major-mode-color-icon t)
    (setq doom-modeline-buffer-state-icon t)
    (setq doom-modeline-buffer-midification-icon t)
    (setq doom-modeline-persp-icon t)
    (setq doom-modeline-modal-icon t)
    (setq doom-modeline-unicode-fallback t)
    ;; persp
    ;;(setq doom-modeline-persp-name t)
    ;;(setq doom-modeline-display-default-persp-name nil)
    ;; lsp
    ;;(setq doom-modeline-lsp t)
    ;; github ;; requires ghub
    ;;(setq doom-modeline-github nil)
    ;;(setq doom-modeline-github-interval (* 30 60))
    ))

;;;; elscreen
(use-package elscreen
  :ensure t
  :bind ("C-c e" . elscreen-next)
  :config
  ;; Turn off peripheral functions of tab.
  (setq elscreen-display-tab nil
        elscreen-tab-display-kill-screen nil
        elscreen-tab-display-control nil)
  ;; init
  (elscreen-start)
  (elscreen-create))

;;;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;;; iflipb
;; https://github.com/jrosdahl/iflipb
(use-package iflipb
  :ensure t
  :bind (("M-o" . iflipb-next-buffer)
         ("M-O" . iflipb-previous-buffer))
  :config
  (setq iflipb-ignore-buffers (list "^[*]"))
  (setq iflipb-wrap-around t))

;;;; smart-newline
(use-package smart-newline
  :ensure t
  :diminish smart-newline-mode
  :bind (;;("RET" . smart-newline-mode)
         ("C-m" . smart-newline-mode))
  :hook ((emacs-lisp-mode . smart-newline-mode)
         (org-mode . smart-newline-mode)))

;;;; swap-buffers
(use-package swap-buffers
  :ensure t
  :bind (("C-M-o" . swap-buffers-keep-focus)
         ("C-M-O" . swap-buffers))
  :config
  (defun swap-buffers-keep-focus ()
    (interactive)
    (swap-buffers t)))

;;;; markdown-mode
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'"
         "\\.markdown\\'"))

;;;; mozc
;; require external package -> "emacs-mozc-bin"
(use-package mozc
  :ensure t :defer t
  ;;:ensure-system-package emacs-mozc-bin
  :config
  (setq default-input-method "japanese-mozc"))

;;;; nyan-mode
(use-package nyan-mode
  :ensure t
  :hook
  (after-init . nyan-mode)
  (nyan-mode . nyan-start-animation)
  :config
  (setq nyan-bar-length 15)
  (setq nyan-cat-face-number 4)
  (setq nyan-minimum-window-width 50))

;;;; org-bullets
;; https://github.com/sabof/org-bullets
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :config
  ;;(setq org-bullets-bullet-list '("" "" "" "" "" "" "" "" "" ""))
  )

;;;; org-journal
(use-package org-journal
  :ensure t
  :bind ("C-c j" . org-journal-new-entry)
  :config
  (setq org-journal-dir "~/Dropbox/document/org/journal")
  (setq org-journal-date-format "%Y-%m-%d %A")
  ;;(setq org-journal-time-format "%R")
  (setq org-journal-file-format "%Y%m%d.org")
  (setq org-journal-find-file 'find-file)
  (setq org-extend-today-until '3)
  ;; 折返しが起こったときの挙動の修正
  (add-hook 'visual-line-mode-hook
            '(lambda()
               (setq word-wrap nil))))

;;;; recentf-ext
(use-package recentf-ext
  :ensure t
  :config
  (setq resentf-max-saved-items 500)
  (setq recentf-exclude
        '("/TAGS$" "/var/tmp/")))

;;;; volatile-highlights
(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :hook (after-init . volatile-highlights-mode)
  :config
  ;; custom-face
  (with-eval-after-load 'doom-dracula-theme
    (custom-set-faces
     '(vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD"))))
     )))

;;;; web-mode
(use-package web-mode
  :ensure t
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[gj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.html?\\'")
  :config
  (setq web-mode-engines-alist
        '(("php" . "\\.phtml\\'")
          ("blade" . "\\.blade\\'"))))



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Themes
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(message "Loading themes...")

;;;; doom-themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-italic t
        doom-themes-enable-bold t)
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  ;;(doom-themes-neotree-config)
  (doom-themes-org-config))

;;;; ice-berg-theme
(use-package iceberg-theme :disabled
  :config
  (iceberg-theme-create-theme-file)
  (load-theme 'solarized-iceberg-dark t))

;;;; Check if any enabled themes.
;;;; If nothing enabled themes, load my-default-faces.
(if custom-enabled-themes
    (when init-file-debug
      (message "Enabled themes: %s" custom-enabled-themes))
  (progn
    (when init-file-debug
      (message "Enabled themes is noghing!")
      (message "Loading my-default-faces...done"))
    (set-my-default-faces)))

;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Finalization
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

;;;; Load time mesurement of init.el
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)



;; init.el ends here
