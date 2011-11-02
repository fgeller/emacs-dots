; gnus
(setq gnus-select-method 
      '(nnimap "dove-fgeller"
	       (nnimap-address "localhost")
	       (nnimap-server-port 143)
	       (nnimap-stream network)
	       (nnimap-authenticator login)))
(setq gnus-permanently-visible-groups "nnimap.*")
(setq gnus-buttonized-mime-types
      '("multipart/alternative" "multipart/signed")
      mm-discouraged-alternatives
      '("text/html" "text/richtext"))

(copy-face 'font-lock-builtin-face 'my-gnus-green)
(setq gnus-face-2 'my-gnus-green)
(copy-face 'font-lock-variable-name-face 'my-gnus-blue)
(setq gnus-face-3 'my-gnus-blue)

(setq gnus-group-line-format "%P  %(%~(pad-right 50)G%) unread: %-4N\n"
      gnus-summary-line-format "%U%R %2{%d%}%I %-20,20f› %3{%s%}\n"
      gnus-topic-line-format "%i[ %0{%(%n (new: %a)%)%} ]\n")

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(setq gnus-treat-display-smileys nil)
