;; -*- coding: utf-8 -*-
;;
;; Many things are happily stolen from https://github.com/purcell/emacs.d
;;

;; All lisp should be (at least linked to) in the following folder
(let ((default-directory "~/.emacs.d/site-lisp"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(setq *vi-emulation-support-enabled* nil) ; "viper-mode"
(setq *spell-check-support-enabled* nil)
(setq *macbook-pro-support-enabled* t)
(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))

;;----------------------------------------------------------------------------
;; Make elisp more civilised
;;----------------------------------------------------------------------------
(require 'cl)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
(require 'init-elpa)
(require 'init-exec-path) ;; Set up $PATH
(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-title-bar)
(require 'init-themes)
(require 'init-osx-keys)
(require 'init-keybindings)
(require 'init-gui-frames)
(require 'init-proxies)
(require 'init-dired)
(require 'init-isearch)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flymake)
(require 'init-yasnippet)
(require 'init-auto-complete)
(require 'init-recentf)
(require 'init-anything)
(require 'init-windows)
(require 'init-sessions)
(require 'init-fonts)
(require 'init-mmm)
(require 'init-growl)
(require 'init-erc)
(require 'init-email)
(require 'init-editing-utils)

(require 'init-gnuplot)
(require 'init-crontab)
(require 'init-markdown)
(require 'init-csv)
(require 'init-javascript)
(require 'init-sh)
(require 'init-org)
(require 'init-htmlize)
(require 'init-nxml)
(require 'init-css)
(require 'init-python-mode)
(require 'init-haskell)

(require 'init-lisp)
(require 'init-slime)
(require 'init-common-lisp)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'server)
(unless (server-running-p)
  (server-start))

(require 'init-locales)
