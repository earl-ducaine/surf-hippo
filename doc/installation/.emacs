;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Sample emacs configuration, to be added (or to create) the .emacs file
;; in your home directory.
;;
;; This file assumes that Surf-Hippo is installed in the directory
;; "/usr/local/surf-hippo/".
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package) ;; You might already have this line
;; To use the stable Melpa package repository
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(desktop-save-mode 1)

;; Slime and Surf-Hippo
;;(require 'slime-autoloads)

(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

(setq inferior-lisp-program "/usr/local/surf-hippo/cmucl-20d/bin/lisp -core  /usr/local/surf-hippo/lib/images/cmucl-20d-image")

(setq lisp-indent-function 'common-lisp-indent-function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      common-lisp-hyperspec-root "file:/usr/local/surf-hippo/doc/outside_docs/CLHS6/HyperSpec/" ;; "file:/usr/local/lisp/CLHS6/HyperSpec/"
      slime-lisp-implementations '((surf-hippo ("/usr/local/surf-hippo/cmucl-20d/bin/lisp" "-core" "/usr/local/surf-hippo/lib/images/cmucl-20d-image")) :init-function ext::print-herald)
      ;; slime-lisp-implementations '((surf-hippo ("/usr/local/cmucl/cmucl-20d/bin/lisp" "-core" "/usr/local/surf-hippo/lib/images/cmucl-20d-image")) :init-function ext::print-herald)
      slime-contribs '(slime-fancy))

(global-set-key "\C-z" 'slime-selector) ; Control-z r will go to REPL buffer
(slime-setup '(slime-fancy slime-asdf))
(put 'upcase-region 'disabled nil)
;; Arglists of some functions are quite long and they may not fit on the screen in one line, there are two ways to display them.
;; This will make the minbuffer change its height automatically to accommodate several lines of text.
(setq slime-autodoc-use-multiline-p t)


;; Suggested in http://stackoverflow.com/questions/24565068/emacs-text-is-read-only
(defun set-region-writeable (begin end)
  "Removes the read-only text property from the marked region."
  ;; See http://stackoverflow.com/questions/7410125
  (interactive "r")
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t))
    (remove-text-properties begin end '(read-only t))
    (set-buffer-modified-p modified)))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((Package . COMMON-LISP-USER)
     (Lowercase . Yes)
     (Package . XLIB)
     (Syntax . Common-lisp)
     (Package . USER)
     (Log . C\.Log)
     (Package . C)
     (Package . SON-OF-PLOT-HACK)
     (Package . INTERACTORS)
     (Package . WINDOWS-HACK)
     (Package . SURF)
     (Package . KR)
     (Package . GILT)
     (Package . OPAL)
     (Package . C32)
     (Package . GARNET-DEBUG)
     (Package . DEMO-LOGO)
     (Package . DEMO-UNISTROKES)
     (Base . 10)
     (Package . GARNET-GADGETS)
     (Syntax . Common-Lisp)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )






