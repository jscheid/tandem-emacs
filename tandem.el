;;; tandem --- Decentralized, cross-editor, collaborative text-editing. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Julian Scheid

;; Author: Julian Scheid <julians37@gmail.com>
;; URL: https://github.com/jscheid/tandem-emacs
;; Keywords: tandem, interaction
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; This is not an official typeintandem project.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See README.md for detailed description.

;;; Code:

(require 'json)
(require 'seq)

(defconst tandem-log-level-trace 0)
(defconst tandem-log-level-debug 1)
(defconst tandem-log-level-info 2)
(defconst tandem-log-level-warn 3)
(defconst tandem-log-level-error 4)

(defconst tandem-log-level-names '(trace debug info warn error))

(defconst tandem-uuid-regexp
  (eval-when-compile
    (mapconcat (lambda (x) (format "[[:xdigit:]]\\{%s\\}" x))
               '(8 4 4 4 12) "-")))

(defvar tandem-log-level tandem-log-level-info)

(defvar tandem-keep-agent-log nil)

(defvar-local tandem-mode nil)
(defvar-local tandem-process nil)
(defvar-local tandem-positions nil)
(defvar-local tandem-session-id nil)
(defvar-local tandem-old-buffer-contents nil)
(defvar-local tandem-old-point nil)

(add-to-list 'minor-mode-alist '(tandem-mode " Tandem"))

(defun tandem-row-column (point)
  "Turn a point into a row/column plist.
POINT the point for which to return row and column."
  (save-excursion
    (widen)
    (goto-char point)
    (list :row (- (line-number-at-pos point) 1)
          :column (current-column))))

(defun tandem-kill-buffer-hook ()
  "Hook executed before a buffer is killed."
  (when (and (local-variable-p 'tandem-process)
             tandem-process)
    (ignore-errors
      (delete-process tandem-process))))

(add-hook 'kill-buffer-hook 'tandem-kill-buffer-hook)

(defun tandem-before-change-hook (begin end)
  "Hook executed before a change.
BEGIN begin of the new region.
END end of the new region."
  (when (and (local-variable-p 'tandem-process)
             tandem-process)
    (add-to-list 'tandem-positions
                 (cons begin (tandem-row-column begin)))
    (add-to-list 'tandem-positions
                 (cons end (tandem-row-column end)))))
(add-hook 'before-change-functions 'tandem-before-change-hook)

(defun tandem-after-change-hook (begin end old-text-len)
  "Hook executed after a change.
BEGIN begin of the new region.
END end of the new region.
OLD-TEXT-LEN length of the old region."
  (when (and (local-variable-p 'tandem-process)
             tandem-process)
    (let ((old-start (cdr (assoc begin tandem-positions)))
          (old-end
           (cdr (assoc (+ begin old-text-len) tandem-positions))))
      (unless
       (or
        (eq old-text-len 0) ; insertion
        (eq old-text-len (- end begin) ) ; replacement
        (eq begin end)) ; deletion
       (error "Cannot handle this change yet :-("))
      (tandem-send-message
       tandem-process
       'new-patches
       :patch_list
       (list
        (list
         (cons 'start old-start)
         (cons 'end (if (eq old-text-len 0)
                        (list :row 0 :column 0)
                      old-end))
         (cons 'text (buffer-substring-no-properties begin end))))))
    (setq tandem-positions nil)))
(add-hook 'after-change-functions 'tandem-after-change-hook)

(defun tandem--log (level &rest fmtargs)
  "Log a message.
LEVEL the level to log at.
FMTARGS passed to `format' as-is."
  (when (>= level tandem-log-level)
    (message "tandem: %s %s"
             (nth level tandem-log-level-names)
             (apply 'format fmtargs))))

(defun tandem--trace (&rest fmtargs)
  "Log a message at trace level.
LEVEL the level to log at.
FMTARGS passed to `format' as-is."
  (apply 'tandem--log tandem-log-level-trace fmtargs))

(defun tandem--error (&rest fmtargs)
  "Log a message at error level.
FMTARGS passed to `format' as-is."
  (apply 'tandem--log tandem-log-level-error fmtargs))

(defun tandem-rename-agent-log-buffer ()
  "Rename the log buffer."
  (let ((new-name (format "*Tandem Agent Log %s*" tandem-session-id)))
    (with-current-buffer (process-get tandem-process 'agent-log-buffer)
      (rename-buffer new-name t))))

(defun tandem-send-message (process type &rest payload)
  "Send a message to the tandem agent.
PROCESS is the tandem agent process.
TYPE is the message type.
PAYLOAD is the message payload."
  (let* ((message (list :type type :version 1 :payload payload))
         (packet (concat (json-encode message)
                         "\n")))
    (tandem--trace "sending %S" packet)
    (process-send-string process packet)))

(defun tandem-handle-message-write-request (process payload)
  "Handle the `write-request' message.
PROCESS is the process from which the message was received.
PAYLOAD is the deserialized message payload."
  (let ((seq (plist-get payload :seq)))
    (with-current-buffer (process-get process 'buffer)
      (setq tandem-old-buffer-contents (buffer-string))
      (setq tandem-old-point (point)))
    (tandem-send-message process
                         'write-request-ack
                         :seq seq)))

(defun tandem-save-session-id-to-kill-ring (session-id)
  "Save SESSION-ID to kill ring unless it's already there."
  (unless (string= (car kill-ring) session-id)
    (kill-new session-id))
  (message "Tandem session ID %s saved to kill ring" session-id))

(defun tandem-handle-message-session-info (process payload)
  "Handle the `session-info' message.
PROCESS is the process from which the message was received.
PAYLOAD is the deserialized message payload."
  (with-current-buffer (process-get process 'buffer)
    (let ((session-id (plist-get payload :session_id)))
      (tandem-save-session-id-to-kill-ring session-id)
      (setq tandem-session-id session-id)
      (tandem-rename-agent-log-buffer))))

(defun tandem-goto-location (location)
  "Go to a location.
LOCATION is a plist with :row and :column properties"
  (goto-char 0)
  (beginning-of-line (+ 1 (plist-get location :row)))
  (forward-char (plist-get location :column))
  (point))

(defun tandem-handle-message-apply-patches (process payload)
  "Handle the `apply-patches' message.
PROCESS is the process from which the message was received.
PAYLOAD is the deserialized message payload."
  (with-current-buffer (process-get process 'buffer)
    (let ((inhibit-modification-hooks t))
      (save-restriction
        (widen)
        (let* ((reg-active-p (region-active-p))
               (reg-beginning (and reg-active-p (region-beginning)))
               (reg-end (and reg-active-p (region-end))))
          (atomic-change-group
            ;; FIXME: undo any changes since the write request. This
            ;; is wonky and eats any edits made in the meantime, need
            ;; to find a better solution.
            (delete-region (point-min) (point-max))
            (insert tandem-old-buffer-contents)
            (when reg-active-p
              (set-mark reg-beginning)
              (goto-char reg-end)
              (activate-mark))
            (goto-char tandem-old-point)
            (save-mark-and-excursion
             (mapc
              (lambda (patch)
                (let ((old-start (tandem-goto-location
                                  (plist-get patch :oldStart))))
                  (delete-region
                   old-start
                   (tandem-goto-location (plist-get patch :oldEnd)))
                  (insert (plist-get patch :newText))))
              (plist-get payload :patch_list)))))))
    (setq tandem-old-buffer-contents nil)
    (setq tandem-old-point nil)))

(defun tandem-handle-message (process message)
  "Handle a message received from the tandem agent.
PROCESS is the agent process and MESSAGE is the full deserialized
JSON message."
  (let ((version (plist-get message :version))
        (type (plist-get message :type))
        (payload (plist-get message :payload)))
    (if (/= version 1)
        (tandem--error
         "Received message with unsupported version: %s"
         version)
      (let ((handler-func
             (intern (format "tandem-handle-message-%s" type))))
        (if (not (fboundp handler-func))
            (tandem--error
             "Received unsupported message type: %s (not found: %S)"
             type
             handler-func)
          (funcall handler-func process payload))))))

(defun tandem-create-process ()
  "Create a new tandem agent process."
  (let* ((tandem-agent-path
          (concat (file-name-directory
                   (symbol-file 'tandem-join-session))
                  "tandem/agent/"))
         (process-environment
          (cons (concat "PYTHONPATH=" tandem-agent-path)
                process-environment))
         (agent-log-buffer
          (generate-new-buffer
           (generate-new-buffer-name "*Tandem Agent Log*")))
         (process
          (make-process
           :connection-type 'pipe
           :name "tandem"
           :command (list
                     (locate-file "python3" exec-path)
                     (concat tandem-agent-path "main.py")
                     "--log-file"
                     "/dev/stderr"
                     "--port"
                     (number-to-string (+ 40000 (random 20000))))
           :coding 'utf-8
           :stderr agent-log-buffer
           :sentinel (lambda (process event)
                       (tandem--trace
                        "agent process status change, event %S, status %S"
                        event
                        (process-status process))
                       (unless (process-live-p process)
                         (with-current-buffer
                             (process-get process 'buffer)
                           (setq tandem-mode nil)
                           (setq tandem-session-id nil)
                           (setq tandem-process nil))
                         (unless tandem-keep-agent-log
                           (sleep-for 0.05)
                           (let ((kill-buffer-query-functions nil))
                             (kill-buffer
                              (process-get process 'agent-log-buffer))))))
           :filter (lambda (process string)
                     (tandem--trace "received chunk %S" string)
                     (let ((ndx 0)
                           (input-buffer
                            (process-get process 'input-buffer)))
                       (while
                           (let ((found (string-match "\n" string ndx)))
                             (when found
                               (tandem-handle-message
                                process
                                (let ((json-object-type 'plist))
                                  (json-read-from-string
                                   (concat
                                    input-buffer
                                    (substring string ndx found)))))
                               (setq input-buffer "")
                               (setq ndx (+ found 1)))))
                       (process-put
                        process
                        'input-buffer
                        (concat input-buffer
                                (substring string ndx))))))))
    (process-put process 'input-buffer "")
    (process-put process 'agent-log-buffer agent-log-buffer)
    (bury-buffer agent-log-buffer)
    process))

(defun tandem-buffer-for-session-id (session-id)
  "Return the buffer running a tandem session.
SESSION-ID is the session ID to look for."
  (seq-find
   (lambda (buf)
     (string= (buffer-local-value 'tandem-session-id buf)
              session-id))
   (buffer-list)))

(defun tandem-host-session ()
  "Host a Tandem session for the current buffer."
  (interactive)

  (if tandem-session-id
      (message
       "This buffer is already running a Tandem session with ID %s"
       tandem-session-id)
    (let ((process (tandem-create-process)))
      (process-put process 'buffer (current-buffer))
      (setq tandem-mode t)
      (setq tandem-process process)
      (tandem-send-message process 'host-session)
      (tandem-send-message
       process
       'new-patches
       :patch_list
       (list
        (list
         '(start :row 0 :column 0)
         '(end :row 0 :column 0)
         (cons 'text (save-excursion
                       (widen)
                       (buffer-substring-no-properties
                        (point-min)
                        (point-max))))))))))

(defun tandem-join-session ()
  "Join an existing Tandem session."
  (interactive)

  (let* ((session-id-raw
          (read-string "Tandem Session ID: "
                       nil
                       'tandem-session-id-history))
         (session-id (progn
                       (when (string-match
                              tandem-uuid-regexp
                              session-id-raw)
                         (match-string 0 session-id-raw))))
         (existing-buffer
          (and session-id
               (tandem-buffer-for-session-id session-id))))
    (when session-id
      (if existing-buffer
          (switch-to-buffer existing-buffer)
        (let ((process (tandem-create-process))
              (new-buffer (generate-new-buffer
                           (generate-new-buffer-name
                            (format "*Tandem Guest %s*" session-id)))))
          (process-put process 'buffer new-buffer)
          (switch-to-buffer new-buffer)
          (setq tandem-mode t)
          (setq tandem-process process)
          (setq tandem-session-id session-id)
          (tandem-rename-agent-log-buffer)
          (tandem-send-message process
                               'join-session
                               :session_id session-id))))))

(defun tandem-kill-session ()
  "Kill the Tandem session for the current buffer."
  (interactive)

  (if (and (local-variable-p 'tandem-process)
           tandem-process)
      (progn
        (message "Closed tandem session %s" tandem-session-id)
        (ignore-errors
          (delete-process tandem-process)))
    (message "No tandem session in current buffer")))

(defun tandem-show-session ()
  "Show the Tandem session for the current buffer.

Also copy it to the kill ring"
  (interactive)
  (if tandem-session-id
      (tandem-save-session-id-to-kill-ring tandem-session-id)
    (message "No tandem session in current buffer")))

(defun tandem-view-agent-log ()
  "Show the Tandem agent log for the current buffer.

Also copy it to the kill ring"
  (interactive)
  (let ((agent-log-buffer
         (and tandem-process
              (process-get tandem-process 'agent-log-buffer))))
    (if agent-log-buffer
        (switch-to-buffer agent-log-buffer)
      (message "No tandem session in current buffer"))))

(provide 'tandem)

;;; tandem.el ends here
