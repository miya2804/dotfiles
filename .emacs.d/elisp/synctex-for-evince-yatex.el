;;; synctex-for-evince-yatex.el --- functions to set synctex for Evince and YaTeX

;; Author: Takayuki YAMAGUCHI <d@ytak.info>
;; Keywords: LaTeX TeX YaTeX Evince
;; Version: 0.0.1
;; Created: Fri Feb 14 16:46:50 2014
;; URL: http://gitorious.org/latex-math-preview/synctex-for-evince-yatex

;; synctex-for-evince-yatex.el includes functions
;; to use synctex for Evince in Linux and YaTeX.
;; The function of forward search is only for YaTeX,
;; but the function of inverse search works properly in other major-mode.

;; Copyright 2014 Takayuki YAMAGUCHI
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later 
;; version. 
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT 
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
;; 
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; synctex-for-evince-yatex.el uses synctex for Evince in Linux and YaTeX via dbus.
;; The dbus setting of foward search is set by `synctex-for-evince-dbus-initialize'.
;; So, before Evince sends a message via dbus, we must call `synctex-for-evince-dbus-initialize'.
;; Any dbus setting for inverse search is not needed and
;; we just call `synctex-for-evince-yatex-forward-search'.
;; 
;; Of course, to use synctex, we need to compile a tex file with "-synctex" option.
;; The author does not think that this program works on Windows
;; because dbus on Windows is not usually installed.

;;; Usage:
;; To use synctex, add the following settings to ~/.emacs.d/init.el;
;; 
;;   (require 'synctex-for-evince-yatex)
;;   (synctex-for-evince-dbus-initialize)
;;   (add-hook 'yatex-mode-hook
;;             '(lambda ()
;;                (YaTeX-define-key "f" 'synctex-for-evince-yatex-forward-search)))
;; 

(defvar synctex-for-evince-frame-focus t
  "Focus emacs frame when emacs accepts signal of inverse search from evince")

(defun synctex-for-evince-un-urlify (fname-or-url)
  "A trivial function that replaces a prefix of file:/// with just /."
  (if (string= (substring fname-or-url 0 8) "file:///")
      (substring fname-or-url 7)
    fname-or-url))

(defun synctex-for-evince-dbus-callback-inverse-search (file linecol &rest ignored)
  (let* ((fname (synctex-for-evince-un-urlify file))
         (buf (or (get-file-buffer fname) (find-file fname)))
         (frame-cur nil))
    (if (null buf)
        (message "[Synctex]: %s is not opened..." fname)
      (pop-to-buffer buf)
      (when synctex-for-evince-frame-focus
        (select-frame-set-input-focus (selected-frame)))
      (with-current-buffer buf
        (goto-line (car linecol))
        (let ((col (cadr linecol)))
          (unless (= col -1) (move-to-column col)))))))

(defvar synctex-for-evince-dbus-initialized nil)

(defun synctex-for-evince-dbus-initialize ()
  (unless synctex-for-evince-dbus-initialized
    (require 'dbus)
    (dbus-register-signal
     :session nil "/org/gnome/evince/Window/0"
     "org.gnome.evince.Window" "SyncSource"
     'synctex-for-evince-dbus-callback-inverse-search)
    (setq synctex-for-evince-dbus-initialized t)))

(defun synctex-for-evince-yatex-pdf-path ()
  (expand-file-name
   (concat (file-name-sans-extension
            (or YaTeX-parent-file
                (save-excursion
                  (YaTeX-visit-main t)
                  buffer-file-name)))
           ".pdf")))

(defun synctex-for-evince-yatex-forward-search ()
  (interactive)
  (let ((pdf-path (concat "file://" (synctex-for-evince-yatex-pdf-path)))
        (line (save-restriction
                (widen)
                (count-lines (point-min) (point))))
        (tex-path (expand-file-name (buffer-name))))
    (let ((dbus-name (dbus-call-method
                      :session "org.gnome.evince.Daemon" "/org/gnome/evince/Daemon" "org.gnome.evince.Daemon"
                      "FindDocument" pdf-path t))
          (utime (truncate (float-time))))
      (dbus-call-method
       :session dbus-name "/org/gnome/evince/Window/0" "org.gnome.evince.Window"
       "SyncView" tex-path (list :struct :int32 line :int32 0) utime))))

(provide 'synctex-for-evince-yatex)

;;; synctex-for-evince-yatex.el ends here
