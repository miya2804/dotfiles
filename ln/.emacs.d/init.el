(defconst emacs-start-time (current-time))

(setq package-enable-at-startup nil)


;; ++++++++++++++++++
;; Environment
;; ++++++++++++++++++
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

  (if init-file-debug
      (setq use-package-verbose t
            use-package-expand-minimally nil
            use-package-compute-statistics t
            debug-on-error t)
    (setq use-package-verbose nil)))



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; user package settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;;;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

;;;; markdown-mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; etc
;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;;;; visualization of space and tab
;;(global-whitespace-mode 1)

;;;; use space on tab
(setq-default tab-width 4 indent-tabs-mode nil)

;;;; mouse wheel
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))
(setq mouse-wheel-progressive-speed nil)

;;;; proxy
;;(setq url-proxy-services '(("http" . "proxy.hoge.com:8080"))) ;; proxy

;;;; windmove setting
(global-set-key (kbd "C-o") (lambda () (interactive) (other-window -1)))
;;(windmove-default-keybindings) ;; use shift+arrow
;;(windmove-default-keybindings 'meta) ;; use alt+arrow



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; appearance settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;;;; hide startup message
(setq inhibit-startup-message t)

;;;; hide *scratch* buffer message
(setq initial-scratch-message "")

;;;; hide menu bar
(menu-bar-mode 0)

;;;; hide tool bar
(if window-system
    (tool-bar-mode 0)
  )

;;;; illuminate corresponding brackets
(show-paren-mode t)

;;;; show full path in title
(setq frame-title-format "%f")

;;;; region highlight
(transient-mark-mode t)
;;;; alpha
(if window-system
    (progn
      (set-frame-parameter nil 'alpha 50)))

;;;; line highlight
(global-hl-line-mode t)

;;;; window size settings
;; (defun set-frame-size-according-to-resolution ()
;;   (interactive)
;;   (if window-system
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

;;;; display line numbers
(require 'linum)
(global-linum-mode t)
(setq linum-format "%3d ")
(line-number-mode t)

;;;; display-time
;;(setq display-time-day-and-date t) 
(setq display-time-24hr-format t)
(display-time)



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; language settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++
(require 'mozc)
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")
(prefer-coding-system 'utf-8)
;;(set-local-environment nil)
;;(set-terminal-coding-system 'utf-8)
;;(set-keyboard-coding-system 'utf-8)
;;(set-buffer-file-coding-system 'utf-8)
;;(setq default-buffer-file-coding-system 'utf-8)
;;(set-default-coding-system 'utf-8)



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; font settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++
(when window-system
  (when (x-list-fonts "SourceHanCodeJP")
    ;;;; create fontset
    (create-fontset-from-ascii-font "SourceHanCodeJp-9:weight=normal:slant=normal" nil "SourceHanCodeJp")
    ;;;; set font
    (set-fontset-font "fontset-SourceHanCodeJp" 'unicode "SourceHanCodeJp" nil 'append)
    ;;;; apply fontset to frame
    (add-to-list 'default-frame-alist '(font . "fontset-SourceHanCodeJp"))))



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; backup setting (xxx~)
;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;;;; execution on or off
(setq make-backup-files t)

;;;; change directory
(setq backup-directory-alist '((".*" . "~/.emacs.d/.ehist/")))

;;;; save multiple backupfiles
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

;;;; create auto-save file in ~/.emacs.d/.ehist
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/.ehist/" t)))



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; lockfile setting (.#xxx)
;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;;;; execution on of off
(setq create-lockfiles nil)



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; custom variables and faces settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (use-package markdown-mode mozc web-mode php-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-function-name-face ((t (:foreground "brightgreen"))))
 '(hl-line ((t (:background "gray25"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "ghost white"))))
 '(web-mode-html-tag-face ((t (:foreground "pale green")))))
(set-face-background 'default "gray13")
(set-face-foreground 'default "ghost white")



;; ++++++++++++++++++
;; Finalization
;; ++++++++++++++++++

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
