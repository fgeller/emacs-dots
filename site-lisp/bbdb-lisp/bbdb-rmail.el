;;; bbdb-rmail.el --- BBDB interface to Rmail

;; Copyright (C) 1991, 1992 Jamie Zawinski <jwz@netscape.com>.
;; Copyright (C) 2010, 2011 Roland Winkler <winkler@gnu.org>

;; This file is part of the Insidious Big Brother Database (aka BBDB),

;; BBDB is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; BBDB is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with BBDB.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; This file contains the BBDB interface to Rmail.
;;; See bbdb.texinfo for documentation.

(require 'bbdb)
(require 'bbdb-com)
(require 'bbdb-mua)
(require 'rmail)
(require 'rmailsum)
(require 'mailheader)

(defcustom bbdb/rmail-update-records-p
  (lambda ()
    (let ((bbdb-update-records-p
           (if (bbdb/rmail-new-flag) 'query 'search)))
      (bbdb-select-message)))
  "How `bbdb-mua-update-records' processes mail addresses in Rmail.
Allowed values are:
 nil          Do nothing.
 search       Search for existing records.
 query        Update existing records or query for creating new ones.
 create or t  Update existing records or create new ones.
A function which returns one of the above values."
  :group 'bbdb-mua-rmail
  :type '(choice (const :tag "do nothing" nil)
                 (const :tag "search for existing records"
                        (lambda () (let ((bbdb-update-records-p 'search))
                                     (bbdb-select-message))))
                 (const :tag "query annotation of all messages"
                        (lambda () (let ((bbdb-update-records-p 'query))
                                     (bbdb-select-message))))
                 (const :tag "annotate (query) only new messages"
                        (lambda ()
                          (let ((bbdb-update-records-p
                                 (if (bbdb/rmail-new-flag) 'query 'search)))
                            (bbdb-select-message))))
                 (const :tag "annotate all messages"
                        (lambda () (let ((bbdb-update-records-p 'create))
                                     (bbdb-select-message))))
                 (const :tag "accept messages" bbdb-accept-message)
                 (const :tag "ignore messages" bbdb-ignore-message)
                 (const :tag "select messages" bbdb-select-message)
                 (sexp  :tag "user defined function")))

(defun bbdb/rmail-new-flag ()
  "Returns t if the current message in buffer BUF is new."
  (rmail-message-labels-p rmail-current-message ", ?\\(unseen\\),"))

(defun bbdb/rmail-header (header)
  "Pull HEADER out of Rmail header."
  (with-current-buffer rmail-buffer
    (if (fboundp 'rmail-get-header)  ; Emacs 23
        (rmail-get-header header)
      (save-restriction
        (with-no-warnings (rmail-narrow-to-non-pruned-header))
        (mail-header (intern-soft (downcase header))
                     (mail-header-extract))))))

;;;###autoload
(defun bbdb-insinuate-rmail ()
  "Hook BBDB into RMAIL.
Do not call this in your init file.  Use `bbdb-initialize'."
  ;; Do we need keybindings for more commands?  Suggestions welcome.
  ;; (define-key rmail-mode-map ":" 'bbdb-mua-display-records)
  ;; (define-key rmail-mode-map "'" 'bbdb-mua-display-recipients)
  (define-key rmail-mode-map ":" 'bbdb-mua-display-sender)
  (define-key rmail-mode-map ";" 'bbdb-mua-edit-notes-sender)
  ;; (define-key rmail-mode-map ";" 'bbdb-mua-edit-notes-recipients)
  (define-key rmail-summary-mode-map ":" 'bbdb-mua-display-sender)
  (define-key rmail-summary-mode-map ";" 'bbdb-mua-edit-notes-sender))

(provide 'bbdb-rmail)
