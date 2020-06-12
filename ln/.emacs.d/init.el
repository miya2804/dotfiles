;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; package settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;;(add-to-list 'package-archives '("maralade" . "https://marmalade-repo.org/packages/") t)
(package-initialize)
;;(package-refresh-contents)



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; language settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;;(set-local-environment nil)
;;(set-language-environment "Japanese")
;;(set-terminal-coding-system 'utf-8)
;;(set-keyboard-coding-system 'utf-8)
;;(set-buffer-file-coding-system 'utf-8)
;;(setq default-buffer-file-coding-system 'utf-8)
;;(set-default-coding-system 'utf-8)
;;(prefer-coding-system 'utf-8)



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; appearance settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; alpha
(if window-system
    (progn
      (set-frame-parameter nil 'alpha 80)))



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; etc
;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;;(global-whitespace-mode 1) ;; visualization of space and tab
(setq-default tab-width 4 indent-tabs-mode nil) ;; use space on tab
(setq inhibit-startup-message t) ;; hide startup message
(setq initial-scratch-message "") ;; hide *scratch* buffer message
(menu-bar-mode -1) ;; hide menu bar
(tool-bar-mode 0) ;; hide tool bar
(show-paren-mode 1) ;; illuminate corresponding brackets
(setq frame-title-format "%f") ;; show full path in title
(display-time-mode t)
(transient-mark-mode 1) ;; region highlight
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))
(setq mouse-wheel-progressive-speed nil)
;;(setq url-proxy-services '(("http" . "proxy.hoge.com:8080"))) ;; proxy


;; windmove setting
(windmove-default-keybindings) ;; use shift+arrow
;;(windmove-default-keybindings 'meta) ;; use alt+arrow



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; backup setting (xxx~)
;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; execution on or off
(setq make-backup-files t)

;; change directory
(setq backup-directory-alist '((".*" . "~/.emacs.d/.ehist/")))

;; save multiple backupfiles
(setq version-control     t) ;; exucution on or off
(setq kept-new-versions   2) ;; latest number
(setq kept-old-versions   1) ;; oldest number
(setq delete-old-versions t) ;; delete out of range



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; auto-save setting (#xxx#)
;; ++++++++++++++++++++++++++++++++++++++++++++++++++
(setq auto-save-timeout 10) ;; 10sec (def:30)
(setq auto-save-interval 100) ;; 100char (def:300)
(setq delete-auto-save-files t) ;; successful completion

;; create auto-save file in ~/.emacs.d/.ehist
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/.ehist/" t)))



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; lockfile setting (.#xxx)
;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; execution on of off
(setq create-lockfiles nil)



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; display line numbers
;; ++++++++++++++++++++++++++++++++++++++++++++++++++
(require 'linum)
(global-linum-mode t)
(setq linum-format "%3d ")
(line-number-mode t)

