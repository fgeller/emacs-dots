;; bbdb
(require 'bbdb)
(bbdb-initialize 'message)
(setq bbdb-complete-mail-allow-cycling t)
(setq bbdb-completion-display-record nil)

;; sending mail via message-mode
(require 'message)
(setq user-full-name "Felix Geller")
(setq user-mail-address "fgeller@gmail.com")
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq message-kill-buffer-on-exit t)
(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)
(add-hook 'message-mode-hook 'turn-on-orgtbl)
(define-key message-mode-map "\C-c\C-b"
  (lambda ()
    (interactive)
    (message-goto-body)
    (if (re-search-forward "mode=sign>" (point-at-eol) t)
	(newline))))

;; notmuch
(require 'notmuch)
(require 'org-notmuch)
(setq notmuch-fcc-dirs nil
      notmuch-mua-user-agent-function 'notmuch-mua-user-agent-emacs
      notmuch-saved-searches '(("Sent Mail" . "from:fgeller")
			       ("inbox" . "tag:inbox")
			       ("unread" . "tag:unread"))
      notmuch-search-oldest-first nil
      notmuch-show-logo nil
      notmuch-crypto-process-mime t)

(defun fg/decrypt-inlined-messages-in-buffer ()
  (save-excursion
    (let ((prefix "-----BEGIN PGP MESSAGE-----")
	  (suffix "-----END PGP MESSAGE-----")
	  start end)
      (goto-char (point-max))
      (while (re-search-backward suffix (point-min) t)
	(setq end (point))
	(re-search-backward prefix)
	(setq start (point))
	(epa-decrypt-region start end)))))
(add-hook 'notmuch-show-hook 'fg/decrypt-inlined-messages-in-buffer)

(define-key notmuch-search-mode-map "Q"
  (lambda ()
    (interactive)
    (notmuch-search-operate-all "-unread -inbox")
    (notmuch-search-quit)))


;; w3m-emacs
(if window-system
    (require 'w3m-load))


;; scala
(require 'scala-mode-auto)
(add-hook 'scala-mode-hook 'scala-mode-feature-electric-mode)
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


;; tex
(load-library "auctex")
(load-library "preview-latex")
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

