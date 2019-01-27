;; ++++++++++
;;    etc
;; ++++++++++
;;(global-whitespace-mode 1) ;; visualization of space and tab
(setq-default tab-width 4 indent-tabs-mode nil) ;; use space on tab



;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; backup setting (xxx~)
;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; execution on or off
(setq make-backup-files t)
;; change directory
(setq backup-directory-alist '((".*" . "~/.emacs.d/.ehist")))
;; save multiple backupfiles
(setq version-control     t) ;; exucution on or off
(setq kept-new-versions   2) ;; latest number
(setq kept-old-versions   1) ;; oldest number
(setq delete-old-versions t) ;; delete out of range



;; +++++++++++++++++++++++++
;; auto-save setting (#xxx#)
;; +++++++++++++++++++++++++
(setq auto-save-timeout 10) ;; 10sec (def:30)
(setq auto-save-interval 100) ;; 100char (def:300)
(setq delete-auto-save-files t) ;; successful completion
;; create auto-save file in ~/.emacs.d/.ehist
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/.ehist") t)))



;; ++++++++++++++++++++++++
;; lockfile setting (.#xxx)
;; ++++++++++++++++++++++++
;; execution on of off
(setq create-lockfiles nil)



;; ++++++++++++++++++++
;; display line numbers
;; ++++++++++++++++++++
(require 'linum)
(global-linum-mode t)
(setq linum-format "%3d ")
(line-number-mode t)






