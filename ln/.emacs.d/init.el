(defconst emacs-start-time (current-time))

(setq package-enable-at-startup nil
      auto-window-vscroll nil)



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Functions
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(90))))



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
    (setq use-package-verbose nil)))

;;;; language
;; (set-language-environment "Japanese")
;; (set-local-environment nil)
;; (prefer-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-default 'buffer-file-cording-system 'utf-8)

;;;; proxy
;;(setq url-proxy-services '(("http" . "proxy.hoge.com:8080"))) ;; proxy



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
(global-set-key (kbd "C-o") (lambda () (interactive) (other-window -1)))
;;(windmove-default-keybindings) ;; use shift+arrow
;;(windmove-default-keybindings 'meta) ;; use alt+arrow

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
(toggle-frame-maximized)
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
(global-set-key (kbd "M-o h") 'global-hl-line-mode)

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

(use-package use-package-ensure-system-package :ensure t :defer t)



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; packages
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

;;;; all-the-icons
;; Make dependent with doom-themes.
;; Fonts install ->  "M-x all-the-icons-install-fonts"
(with-eval-after-load 'doom-modeline
  (use-package all-the-icons :ensure t :defer t))

;;;; doom themes
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-italic t
        doom-themes-enable-bold t)
  (load-theme 'doom-dracula t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config))

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
;; custom variables and faces settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (use-package markdown-mode mozc web-mode php-mode))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(font-lock-function-name-face ((t (:foreground "brightgreen"))))
;; '(hl-line ((t (:background "gray25"))))
;; '(web-mode-html-tag-bracket-face ((t (:foreground "ghost white"))))
;; '(web-mode-html-tag-face ((t (:foreground "pale green")))))
;; (set-face-background 'default "gray13")
;; (set-face-foreground 'default "ghost white")



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; Finalization
;; ++++++++++++++++++++++++++++++++++++++++++++++++++

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
