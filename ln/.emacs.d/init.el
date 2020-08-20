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

(defvar is-load-theme nil
  "This var should be non-nil if an external theme is loaded.")

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

(defun swap-buffers-keep-focus ()
  (interactive)
  (swap-buffers t))



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
  (require 'use-package)

  ;;;; debug
  (if init-file-debug
      (setq use-package-verbose t
            use-package-expand-minimally nil
            use-package-compute-statistics t
            debug-on-error t)
    (setq use-package-verbose nil
          use-package-expand-minimally t)))

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
(setq custom-file (locate-user-emacs-file "custom.el"))
;;(load custom-file)



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

;; -------------------------------------
;; etc

;;;; visualization of space and tab
;;(global-whitespace-mode 1)

(setq-default tab-width 4 indent-tabs-mode nil)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 5))
      mouse-wheel-progressive-speed nil)

(setq scroll-step 1)

;;;; windmove setting
(global-set-key (kbd "C-o") '(lambda () (interactive) (other-window -1)))
;;(windmove-default-keybindings) ;; use shift+arrow
;;(windmove-default-keybindings 'meta) ;; use alt+arrow

(global-set-key (kbd "C-c r") 'window-resizer)

;; -------------------------------------
;; appearance

;;;; hide startup message
(setq inhibit-startup-message t)

;;;; hide *scratch* buffer message
(setq initial-scratch-message nil)

;;;; hide menu bar
(menu-bar-mode 0)

;;;; hide tool bar
(if (display-graphic-p)
    (tool-bar-mode 0)
  )

;;;; illuminate corresponding brackets
(show-paren-mode t)

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

;;;; linum
(global-linum-mode t)
(setq linum-format "%3d ")

;;;; hl-line
(global-hl-line-mode t)

;; -------------------------------------
;; font

(when (display-graphic-p)
  (when (x-list-fonts "SourceHanCodeJP")
    ;;;; create fontset
    (create-fontset-from-ascii-font "SourceHanCodeJp-9:weight=normal:slant=normal" nil "SourceHanCodeJp")
    ;;;; set font
    (set-fontset-font "fontset-SourceHanCodeJp" 'unicode "SourceHanCodeJp" nil 'append)
    ;;;; apply fontset to frame
    (add-to-list 'default-frame-alist '(font . "fontset-SourceHanCodeJp"))))

;; -------------------------------------
;; backup (xxx~)

;;;; execution on or off
(setq make-backup-files t)

;;;; change directory
(setq backup-directory-alist '((".*" . "~/.emacs.d/.ehist/")))

;;;; save multiple backupfiles
(setq version-control     t) ;; exucution on or off
(setq kept-new-versions   2) ;; latest number
(setq kept-old-versions   1) ;; oldest number
(setq delete-old-versions t) ;; delete out of range

;; -------------------------------------
;; auto-save (#xxx#)

(setq auto-save-timeout 10) ;; 10sec (def:30)
(setq auto-save-interval 100) ;; 100char (def:300)
(setq delete-auto-save-files t) ;; successful completion

;;;; create auto-save file in ~/.emacs.d/.ehist
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/.ehist/" t)))

;; -------------------------------------
;; lockfile (.#xxx)

;;;; execution on of off
(setq create-lockfiles nil)



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; libraries
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(use-package diminish :ensure t :demand t)
(use-package use-package-ensure-system-package :ensure t :defer t)



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; packages
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
  :init
  (global-anzu-mode t)
  :commands (anzu-query-replace-at-cursor)
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :config
  (setq anzu-search-threshold 1000)
  (setq anzu-replace-threshold 1000)
  (setq anzu-minimum-input-length 1)
  (if (locate-library "migemo")
      (setq anzu-use-migemo t)))

;;;; dashborad
(use-package dashboard
  :ensure t
  :hook (after-init . dashboard-setup-startup-hook))

;;;; doom-themes
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-italic t
        doom-themes-enable-bold t)
  (load-theme 'doom-dracula t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (setq is-load-theme t))

;;;; doom-modelfine
;; Make dependent with doom-themes.
(with-eval-after-load 'doom-themes
  (use-package doom-modeline
    :ensure t
    :config
    (doom-modeline-mode 1)
    (line-number-mode 0)
    (column-number-mode 1)
    (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
    (setq doom-modeline-icon (display-graphic-p))
    (setq doom-modeline-major-mode-icon nil)
    (setq doom-modeline-minor-modes nil)))

;;;; elscreen
(use-package elscreen
  :ensure t
  :bind ("C-c b" . elscreen-next)
  :config
  ;; Turn off peripheral functions of tab.
  (setq elscreen-display-tab nil
        elscreen-tab-display-kill-screen nil
        elscreen-tab-display-control nil)
  ;; init
  (elscreen-start)
  (elscreen-create))

;;;; iflipb
;;https://github.com/jrosdahl/iflipb
(use-package iflipb
  :ensure t
  :bind (("C-<tab>" . iflipb-next-buffer)
         ("C-<iso-lefttab>" . iflipb-previous-buffer))
  :config
  (setq iflipb-ignore-buffers (list "^[*]")))

;;;; smart-newline
(use-package smart-newline
  :ensure t
  :diminish smart-newline-mode
  :bind (;;("RET" . smart-newline-mode)
         ("C-m" . smart-newline-mode))
  :hook (emacs-lisp-mode . smart-newline-mode))

;;;; swap-buffers
(use-package swap-buffers
  :ensure t
  :bind ("<f2>" . swap-buffers-keep-focus)
  )

;;;; markdown-mode
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'"
         "\\.markdown\\'"))

;;;; mozc
;; require external package -> "emacs-mozc-bin"
(use-package mozc
  :ensure t
  ;;:ensure-system-package emacs-mozc-bin
  :bind ("M-\\" . toggle-input-method)
  :config
  (setq default-input-method "japanese-mozc"))

;;;; nyan-mode
(use-package nyan-mode
  :ensure t :demand t
  :if (display-graphic-p)
  :config
  (nyan-mode)
  (nyan-start-animation))

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
;; Finalization
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

;;;; Check if any themes is installed.
(if is-load-theme
    (message "Checking if theme is loaded...OK")
  (progn
    (message "Checking if theme is loaded...FAILD")
    (message "Load my-default-faces...done")
    (set-my-default-faces)))

;; (let (is-themes)
;;   ;; Check the existence of the themes
;;   ;; If install theme, add below.
;;   (when (locate-library "doom-themes")
;;     (setq is-themes t))

;;   ;;If any theme is installed, set my-default-faces.
;;   (if is-themes
;;       (message "Checking existence of themes...Theme is exist.")
;;     (progn
;;       (message "Checking existence of themes...Theme isn't exist.")
;;       (set-my-default-faces)
;;       (message "Load my-default-faces."))))

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
