;;; imp.el --- IRC Multiplayer Elisp                 -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Nicholas Vollmer

;; Author:  Nicholas Vollmer <nv@parenthetic.dev>
;; Keywords: lisp, games
;; URL: https://www.github.com/progfolio/imp
;; Version: 0.0.0
;; Package-Requires: ((emacs "29"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; IMP is a framework for building multi-player elisp games which use IRC to pass messages.

;;; Code:
(require 'erc)

(defgroup imp nil "IRC Multi-player elisp."
  :prefix "imp-"
  :group 'applications)

(defvar-local imp-channel-name nil)
(defvar-local imp-channel-key nil)
(defvar-local imp-name nil)

(defun imp--key ()
  "Return a key."
  (let ((digits))
    (dotimes (_ 6) (push (number-to-string (random 10)) digits))
    (string-join digits)))

;;@TEST
(defun imp--log-message (response)
  "Log RESPONSE."
  (when response
    (with-current-buffer (get-buffer-create "*IMP-TEST*")
      (goto-char (point-max))
      (insert (format "%s: %s\n"
                      (erc-response.sender response)
                      (erc-response.contents response))))))

(defcustom imp-message-functions '(imp--log-message)
  "Abnormal hook when imp server recives a message." :type 'hook)

(defcustom imp-server-hook nil
  "Hook when imp server initialized." :type 'hook)

(defun imp--broadcast ()
  "Broadcast message."
  (goto-char (point-min))
  (erc-find-parsed-property)
  (when-let ((response (erc-get-parsed-vector (point))))
    (run-hook-with-args 'imp-message-functions response)))

;;;###autoload
(defun imp-client (&optional channel key name interactive)
  "Join an imp server on CHANNEL with KEY and NAME.
If CHANNEL does not exist, it will be created.
If INTERACTIVE is non-nil, copy invite string to clipboard, else return it."
  (interactive (list (read-string "Server Channel: ")
                     (read-string "Password: ")
                     (read-string "Name: ")
                     t))
  (let* ((seed (random 1000000))
         (channel (if (or (null channel) (string-empty-p channel)) (format "#imp-server-%d" seed) channel))
         (key (if (or (null key) (string-empty-p key)) (imp--key) key))
         (name (if (or (null name) (string-empty-p name)) (format "imp-%d" seed) name))
         (invite (format "%s@%s" channel key)))
    (with-current-buffer (erc :server erc-default-server :port erc-default-port
                              :nick name)
      (add-hook 'erc-after-connect (lambda (&rest _) (erc-join-channel channel key)) nil t)
      (add-hook 'erc-join-hook
                (lambda () (when (string= (buffer-name) channel)
                             (setq-local imp-channel-name channel
                                         imp-channel-key key
                                         imp-name name)
                             (erc-set-channel-key key)
                             (run-hooks 'imp-server-hook)
                             (add-hook 'erc-insert-modify-hook #'imp--broadcast nil t)))))
    (if (not interactive)
        invite
      (let ((select-enable-clipboard t))
        (kill-new invite)
        (message "IMP Invite %s copied to clipboard" invite)))))

;;;###autoload
(defun imp-accept-invite (invite &optional name)
  "Accept INVITE to channel as NAME."
  (interactive "sInvite code: \nsName: ")
  (let* ((tokens (split-string (string-trim invite) "@"))
         (channel (car tokens))
         (key (cadr tokens)))
    (unless (and (string-prefix-p "#" channel) key) (user-error "Malformed invite"))
    (imp-client channel key name)))

(provide 'imp)
;;; imp.el ends here
