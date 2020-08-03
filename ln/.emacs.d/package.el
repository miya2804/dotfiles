;; ;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; ;; package install
;; ;; ++++++++++++++++++++++++++++++++++++++++++++++++++
;; ;;;; set package repositorys
;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; ;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; ;;(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;; ;;(add-to-list 'package-archives '("maralade" . "https://marmalade-repo.org/packages/") t)
;; (package-initialize)

(package-refresh-contents)

;;;; package list to install
(defvar my/packages
  '(
    ;; input
    mozc

    ;; java, javascript, php, etc...
    web-mode
    php-mode

    ;; markdown
    markdown-mode
    ))

;;;; install packages from my/packages
(dolist (package my/packages)
  (unless (package-installed-p package)
    (package-install package)))
