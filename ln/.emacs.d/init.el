;;;; init.el --- My Emacs Initialization/Customization file  -*- lexical-binding: t -*-
;;;; compile
;;(byte-recompile-directory (expand-file-name "~/.emacs.d/") 0)



;;;; code:
(defconst emacs-start-time (current-time))
(message (format "[Startup time: %s]" (format-time-string "%Y/%m/%d %H:%M:%S")))
(eval-when-compile (require 'cl))



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Functions and Variables and Macros
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

;;;; with-eval-after-load (Emacs 24.4 以上)
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    `(eval-after-load ,file
       `(funcall (function ,(lambda () ,@body))))))

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
  (let ((dx (if (= (nth 0 (window-edges)) 0) 1
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
               (let ((command (key-binding action)))
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

(eval-and-compile

  ;;;; Package Manager
  ;; package.el
  (when (require 'package nil t)
    (set-variable
     'package-archives
     '(("melpa" . "https://melpa.org/packages/")
       ;;("melpa-stable" . "https://stable.melpa.org/packages/")
       ;;("org" . "https://orgmode.org/elpa/")
       ;;("maralade" . "https://marmalade-repo.org/packages/")
       ))
    (package-initialize)
    (set-variable 'package-enable-at-startup nil))

  ;;;; load-path
  (setq load-path (cons "~/.emacs.d/elisp" load-path))

  ;;;; proxy
  ;; ~/.emacs.d/elisp/secret/myproxy.elにプロキシ設定を書き込む
  (load "secret/myproxy" t)

  ;;;; debug
  (if init-file-debug
      (progn
        (setq debug-on-error t)
        (setq force-load-messages t)
        (set-variable 'use-package-verbose t)
	    (set-variable 'use-package-expand-minimally nil)
	    (set-variable 'use-package-compute-statistics t))
    (progn
      (set-variable 'use-package-verbose nil)
      (set-variable 'use-package-expand-minimally t))))

;;;; use-package
;; - https://github.com/jwiegley/use-package
;;   非標準パッケージは use-package で管理する。（標準ライブラリは use-package では管理しない）
;; - 起動時の use-package の抑止
;;   init.el を外部に持ちだした時など、use-package を抑止したいときはEmacs を、オプション "--qq" で起動する。
;; - use-package が未インストールか、抑止されている場合は空マクロにする。
;; インストールされていなければインストールを実行
;;(unless (package-installed-p 'use-package)
;;  (package-refresh-contents)
;;  (package-install 'use-package))
(eval-and-compile
  (when (or (member "--qq" command-line-args)
            (null (require 'use-package nil t)))
    (warn "`use-package' is unavailable!  Please install it via `M-x list-packages' if possible.")
    (defmacro use-package (&rest _args))))
;; 後の startup.el におけるオプション認識エラーを防止
(add-to-list 'command-switch-alist '("--qq" . (lambda (switch) nil)))
(use-package use-package :ensure t :defer t) ;; 形式的宣言

;;;; bind-key
;; bind-key* は、emulation-mode-map-alists を利用することにより、
;; minor-mode よりも優先させたいキーのキーマップを定義できる。
;; bind-key.el がない場合は普通のbind-key として振る舞う。
(use-package bind-key :ensure t :defer t)
(eval-and-compile
  (unless (require 'bind-key nil t)
    (defun bind-key (key cmd &optional keymap)
      (define-key (or keymap global-map) (kbd key) cmd))
    (defun bind-key* (key cmd) (global-set-key (kbd key) cmd))))



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

;; -------------------------------------
;; Etc

;;;; tab幅
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
(bind-key "C-o" 'other-window-or-split)
(bind-key "C-c r" 'window-resizer)
(bind-key "<zenkaku-hankaku>" 'toggle-input-method)
(bind-key "C-c n" 'make-frame)
(bind-key "C-c w" 'delete-frame)

;;;; backup (xxx~)
;; execution on or off
(setq make-backup-files t)
;; change directory
(setq backup-directory-alist
      (cons (cons ".*" (expand-file-name "~/.emacs.d/.ehist"))
            backup-directory-alist))
;; save multiple backupfiles
(setq version-control     t) ;; exucution on or off
(setq kept-new-versions   5) ;; latest number
(setq kept-old-versions   1) ;; oldest number
(setq delete-old-versions t) ;; delete out of range

;;;; auto-save (#xxx#)
(setq auto-save-timeout 10) ;; 10sec (def:30)
(setq auto-save-interval 100) ;; 100char (def:300)
(setq delete-auto-save-files t) ;; successful completion
(setq auto-save-file-name-transforms
      `((".*", (expand-file-name "~/.emacs.d/.ehist") t)))

;;;; lockfile (.#xxx)
;; execution on of off
(setq create-lockfiles nil)

;;;; open bufferlist on current window
;; helm-for-filesが後に置き換え
;; 置き換えられない場合コチラがセット
(bind-key "C-x C-b" 'buffer-menu)

;;;; language
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-default 'buffer-file-cording-system 'utf-8)

;;;; saving customization
(setq custom-file (locate-user-emacs-file "elisp/custom.el"))

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

;;;; display-time
;; (setq display-time-day-and-date nil)
;; (setq display-time-24hr-format t)
;; (display-time)

;;;; indicater
(setq-default indicate-empty-lines nil)
(setq-default indicate-buffer-boundaries 'left)

;;;; visualization of space and tab
;;(global-whitespace-mode 1)

;;;; 行末の空白表示
(setq-default show-trailing-whitespace nil)
(defun turn-on-show-trailing-whitespace  () (interactive) (setq show-trailing-whitespace t))
(defun turn-off-show-trailing-whitespace () (interactive) (setq show-trailing-whitespace nil))
(defun toggle-show-trailing-whitespace () (interactive) (callf null show-trailing-whitespace))
(add-hook 'prog-mode-hook 'turn-on-show-trailing-whitespace)
(add-hook 'org-mode-hook 'turn-on-show-trailing-whitespace)



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Libraries
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(use-package diminish :ensure t :demand t)
;;(use-package use-package-ensure-system-package :ensure t :demand t)



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Fonts
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(when (display-graphic-p)
  (when (x-list-fonts "SourceHanCodeJP")
    ;;;; create fontset
    (create-fontset-from-ascii-font "SourceHanCodeJp-9:weight=normal:slant=normal" nil "SourceHanCodeJp")
    ;;;; set font
    (set-fontset-font "fontset-SourceHanCodeJp" 'unicode "SourceHanCodeJp" nil 'append)
    ;;;; apply fontset to frame
    (add-to-list 'default-frame-alist '(font . "fontset-SourceHanCodeJp"))))



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Themes
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(when init-file-debug (message "Loading themes..."))

;;;; doom-themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-italic t
        doom-themes-enable-bold t)
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

;;;; ice-berg-theme
(use-package iceberg-theme :disabled
  :config
  (iceberg-theme-create-theme-file)
  (load-theme 'solarized-iceberg-dark t))

;;;; zenburn-theme
(use-package zenburn-theme :disabled
  :config
  (load-theme 'zenburn t))

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
;; Packages
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

;; -------------------------------------
;; standard packages

;;;; display-line-numbers.el
;;;; linum.el
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
  (progn
    (global-linum-mode)
    (set-variable 'linum-format "%3d ")))

;;;; hl-line.el
(global-hl-line-mode)

;;;; org.el
(defvar org-directory)
(declare-function org-buffer-list "org")
;; cf. https://www.emacswiki.org/emacs/OrgMode#toc21
(defun mhatta/org-buffer-files ()
  "Return list of opened Org mode buffer files"
  (mapcar (function buffer-file-name)
          (org-buffer-list 'files)))
;; org-directory内の(file)を確認できる関数
(defun show-org-buffer (file)
  "Show an org-file FILE on the current buffer."
  (interactive)
  (if (get-buffer file)
      (let ((buffer (get-buffer file)))
        (switch-to-buffer buffer)
        (message "%s" file))
    (find-file (concat org-directory file))))
;; key binds
(bind-key "C-c c" 'org-capture)
(bind-key "C-M-^" '(lambda () (interactive) (show-org-buffer "/notes.org")))
;; mode
;;(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(push '("\\.org\\'" . org-mode) auto-mode-alist)
(with-eval-after-load 'org
  (setq org-directory "~/Dropbox/document/org")
  (set-variable 'org-agenda-files '("~/Dropbox/document/org/agenda/"))
  (set-variable 'org-default-notes-file
                (concat org-directory "/notes.org"))
  (set-variable 'org-startup-truncated nil)
  ;; org-dir外のrefile設定(bufferで開いていれば指定可能)
  (set-variable 'org-refile-targets
        '((nil :maxlevel . 3)
          (mhatta/org-buffer-files :maxlevel . 1)
          (org-agenda-files :maxlevel . 3)))
  ;; templates
  (set-variable 'org-capture-templates
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
           :empty-lines 1 :jump-to-captured 1))))

;;;; paren.el
;; illuminate corresponding brackets
(show-paren-mode t)
(set-variable 'show-paren-style 'mixed)
(set-variable 'show-paren-when-point-inside-paren t)
(set-variable ' show-paren-when-point-in-periphery t)
;; custom-face
(with-eval-after-load 'doom-dracula-theme
  (custom-set-faces
   '(show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c"))))
   ))

;;;; recentf.el
(set-variable 'recentf-max-saved-items 500)
;;(set-variable 'recentf-auto-cleanup 'never)
(set-variable 'recentf-exclude
              '("/TAGS$" "/var/tmp/" ".recentf"))

;; -------------------------------------
;; Non-standard Packages

;;;; ace-isearch
(use-package ace-isearch
  :ensure t
  :diminish ace-isearch-mode
  :hook (after-init . global-ace-isearch-mode)
  :config (setq ace-isearch-jump-delay 0.7))

;;;; ace-jump-mode
(use-package ace-jump-mode
  :ensure t :defer t
  :config
  (setq ace-jump-mode-move-keys
        (append "asdfghjkl;:]qwertyuiop@zxcvbnm,." nil))
  ;; ace-jump-word-modeのとき文字を尋ねないようにする
  (setq ace-jump-word-mode-use-query-char nil))

;;;; all-the-icons
;; Make dependent with doom-themes.
;; Fonts install ->  "M-x all-the-icons-install-fonts"
(use-package all-the-icons :ensure t :defer t)

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

(use-package auto-async-byte-compile
  :ensure t :defer t
  :hook (emacs-lisp-mode . enable-auto-async-byte-compile-mode)
  :config
  (set-variable 'auto-async-byte-compile-exclude-files-regexp "/secret/"))

;;;; beacon
(use-package beacon
  :ensure t
  :diminish beacon-mode
  :hook (after-init . beacon-mode)
  :config
  (with-eval-after-load 'doom-dracula-theme
    (setq beacon-color "yellow"
          beacon-size 20
          beacon-blink-duration 0.2)))

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
  ;; dashboard items
  (setq dashboard-items '((recents  . 10)
                        (bookmarks . 5)
                        (agenda . 5)))
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
  (setq doom-modeline-buffer-modification-icon t)
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
  )

;;;; elscreen
(use-package elscreen
  :ensure t :defer nil :no-require t
  :functions (elscreen-create)
  :config
  ;; 衝突回避(org-modeと衝突) ;; bind-key*で解決済
  ;; (define-minor-mode overriding-minor-mode
  ;;   "強制的にC-tを割り当てる"             ;説明文字列
  ;;   t                                ;デフォルトで有効にする
  ;;   ""                               ;モードラインに表示しない
  ;;   `((,(kbd "C-<tab>") . elscreen-next)))
  (bind-key* "C-<tab>" 'elscreen-next)
  ;; Turn off peripheral functions of tab.
  (set-variable 'elscreen-display-tab nil)
  (set-variable 'elscreen-tab-display-kill-screen nil)
  (set-variable 'elscreen-tab-display-control nil)
  ;; init
  (elscreen-start)
  (elscreen-create))

;;;; git-gutter
(use-package git-gutter
  :ensure t :no-require t
  :diminish git-gutter-mode
  :hook (after-init . global-git-gutter-mode)
  :functions (git-gutter:previous-hunk git-gutter:next-hunk
              git-gutter:stage-hunk git-gutter:revert-hunk
              git-gutter:popup-hunk)
  :custom-face
  (git-gutter:modified ((t (:background "#f1fa8c"))))
  (git-gutter:added    ((t (:background "#50fa7b"))))
  (git-gutter:deleted  ((t (:background "#ff79c6"))))
  :config
  (set-variable 'git-gutter:modified-sign "~")
  (set-variable 'git-gutter:added-sign    "+")
  (set-variable 'git-gutter:deleted-sign  "-"))

;;;; helm
(use-package helm
  :ensure t
  :diminish helm-migemo-mode
  :functions (helm-execute-persistent-action helm-kill-selection-and-quit
              helm-select-action helm-migemo-mode)
  :bind (("C-x C-f" . helm-find-files)
         ("C-c h" . helm-command-prefix)
         ("C-x C-b" . helm-for-files)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring))
  :config
  (require 'helm-config)
  (with-eval-after-load 'migemo
    (helm-migemo-mode 1))
  ;; helm-map keybinds
  (bind-keys :map helm-map
             ("<tab>" . helm-execute-persistent-action)
             ("C-c C-k" . helm-kill-selection-and-quit)
             ("C-i" . helm-execute-persistent-action)
             ("C-z" . helm-select-action))
  ;; fuzzy matting
  (set-variable 'helm-M-x-fuzzy-match t)
  (set-variable 'helm-buffers-fuzzy-matching t)
  (set-variable 'helm-recentf-fuzzy-match t)
  (set-variable 'helm-apropos-fuzzy-match t)
  (set-variable 'helm-lisp-fuzzy-completion t)
  ;; helm-for-files
  (set-variable 'helm-for-files-preferred-list
        '(helm-source-buffers-list
          helm-source-recentf
          helm-source-bookmarks
          helm-source-file-cache
          helm-source-files-in-current-dir
          helm-source-bookmark-set
          ;;helm-source-locate
          )))

;;;; helm-swoop
(use-package helm-swoop
  :ensure t
  :requires helm
  :commands (helm-swoop helm-multi-swoop)
  :config
  (setq helm-swoop-move-to-line-cycle nil))

;;;; highlight-indent-guides
(use-package highlight-indent-guides
  :ensure t
  :diminish highlight-indent-guides-mode
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-auto-enabled t)
  (setq highlight-indent-guides-responsive t)
  (setq highlight-indent-guides-method
        (if (display-graphic-p) 'bitmap 'character)))

;;;; hydra
(use-package hydra
  :ensure t :defer nil :no-require t
  :config
  (with-eval-after-load 'git-gutter
    (defhydra hydra-git-gutter nil
      "git hunk"
      ("p" git-gutter:previous-hunk "previous")
      ("n" git-gutter:next-hunk "next")
      ("s" git-gutter:stage-hunk "stage")
      ("r" git-gutter:revert-hunk "revert")
      ("SPC" git-gutter:popup-hunk "diffinfo"))
    (bind-key "C-c g" 'hydra-git-gutter/body)))

;;;; iflipb
;; https://github.com/jrosdahl/iflipb
(use-package iflipb
  :ensure t
  :bind (("M-o" . iflipb-next-buffer)
         ("M-O" . iflipb-previous-buffer))
  :config
  (setq iflipb-ignore-buffers (list "^[*]"))
  (setq iflipb-wrap-around t))

;;;; markdown-mode
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'"
         "\\.markdown\\'"))

;;;; migemo
(defvar migemo-command (executable-find "cmigemo"))
(defvar migemo-dictionary
  (locate-file "migemo-dict"
               '("/usr/share/cmigemo/utf-8"))) ;; debian
(use-package migemo
  :if (and migemo-command migemo-dictionary)
  :ensure t :defer nil :no-require t
  :functions (migemo-init)
  :config
  (set-variable 'migemo-options '("-q" "--emacs"))
  (set-variable 'migemo-coding-system 'utf-8-unix)
  (set-variable 'migemo-user-dictionary nil)
  (set-variable 'migemo-regex-dictionary nil)
  (load-library "migemo")
  (migemo-init))

;;;; mozc
;; require external package -> "emacs-mozc-bin"
(use-package mozc
  :if (executable-find "mozc_emacs_helper")
  :ensure t :defer t
  :config
  (setq default-input-method "japanese-mozc"))

;;;; neotree
(use-package neotree
  :ensure t
  :bind ("C-q" . neotree-toggle)
  :config
  (setq neo-theme
        (if (display-graphic-p) 'nerd2 'arrow))
  (setq neo-show-hidden-files t))

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

;;;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;;; recentf-ext
(use-package recentf-ext :ensure t :defer nil)

;;;; redo+
(require 'redo+)
(bind-key "C-M-/" 'redo)

;;;; smart-newline
(use-package smart-newline
  :ensure t :defer t
  :diminish smart-newline-mode
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

;;;; which-key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :hook (after-init . which-key-mode))



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
