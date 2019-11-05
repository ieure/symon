;;; symon.el --- tiny graphical system monitor     -*- lexical-binding: t; -*-

;; Copyright (C) 2015 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 1.2.0

;;; Commentary:

;; Load this script
;;
;;   (require 'symon)
;;
;; and turn on `symon-mode'.
;;
;;   (symon-mode)
;;
;; then a tiny system monitor is displayed in minibuffer, during idle.

;;; Change Log:

;; 1.0.0 first release
;; 1.1.0 add option symon-sparkline-thickness
;; 1.1.1 add symon-windows-page-file-monitor
;; 1.1.2 add darwin support (mac os x)
;; 1.2.0 add paging feature

;;; Code:

(require 'ring)
(require 'symon-sparkline)

(defconst symon-version "2.0.0")

(defgroup symon nil
  "tiny graphical system monitor"
  :group 'emacs)

;; + customs

;; core

(defcustom symon-refresh-rate 4
  "refresh rate of symon display. *set this option BEFORE
  enabling `symon-mode'.*"
  :group 'symon)

(defcustom symon-delay 2
  "delay in seconds until symon is displayed. *set this option
BEFORE enabling `symon-mode'.*"
  :group 'symon)

(defcustom symon-monitors
  (list (cond ((memq system-type '(gnu/linux cygwin))
         '(symon-memory-linux
           symon-cpu-linux
           symon-network-rx-linux
           symon-network-tx-linux))
        ((memq system-type '(darwin))
         '(symon-memory-darwin
           symon-cpu-darwin
           symon-network-rx-darwin
           symon-network-tx-darwin))
        ((memq system-type '(windows-nt))
         '(symon-memory-windows
           symon-cpu-windows
           symon-network-rx-windows
           symon-network-tx-windows))))

  "List of list of monitors.

Each outer list is a page.  Symon rotates through pages as it redisplays.

Each inner list is a list of monitors.  Members of that list
are anything which `symon--instantiate' knows how to produce a
monitor from:

- A symbol which is bound to a monitor class.  The class will be
  instantiated with no arguments.
- A symbol which is bound to a monitor object.
- A monitor object itself (not recommended).
- An expression which evaluates to one of the above."

  :group 'symon
  :risky t
  :type '(repeat
          (repeat :tag "Page of monitors"
                  (choice
                   (symbol :tag "Class or object")
                   (sexp)))))

;; page-file monitor

(defcustom symon-windows-page-file-upper-bound 2000
  "upper-bound of sparkline for page file usage."
  :group 'symon)

;; + utilities
;;   + general

(defun symon--flatten (lst)
  "flatten LST"
  (if (consp lst)
      (apply 'nconc (mapcar 'symon--flatten lst))
    (list lst)))

;;   + symon monitor classes & helpers


;;   + process management

(defvar symon--process-buffer-name " *symon-process*")
(defvar symon--process-reference-count 0)

(defun symon--read-value-from-process-buffer (index)
  "Read a value from a specific buffer"
  (when (get-buffer symon--process-buffer-name)
    (with-current-buffer symon--process-buffer-name
      (when (save-excursion
              (search-backward-regexp (concat index ":\\([0-9]+\\)\\>") nil t))
        (read (match-string 1))))))

(defun symon--maybe-start-process (cmd)
  (setq symon--process-reference-count
        (1+ symon--process-reference-count))
  (unless (get-buffer symon--process-buffer-name)
    (let ((proc (start-process-shell-command
                 "symon-process" symon--process-buffer-name cmd))
          (filter (lambda (proc str)
                    (when (get-buffer symon--process-buffer-name)
                      (with-current-buffer symon--process-buffer-name
                        (when (and (string-match "-" str) (search-backward "----" nil t))
                          (delete-region 1 (point)))
                        (goto-char (1+ (buffer-size)))
                        (insert str))))))
      (set-process-query-on-exit-flag proc nil)
      (set-process-filter proc filter))))

(defun symon--maybe-kill-process ()
  (setq symon--process-reference-count
        (1- symon--process-reference-count))
  (when (and (zerop symon--process-reference-count)
             (get-buffer symon--process-buffer-name))
    (kill-buffer symon--process-buffer-name)))

;; + predefined monitors
;;   + linux monitors

(defun symon-linux--read-lines (file reader indices)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char 1)
    (mapcar (lambda (index)
              (save-excursion
                (when (search-forward-regexp (concat "^" index "\\(.*\\)$") nil t)
                  (if reader
                      (funcall reader (match-string 1))
                    (match-string 1)))))
            indices)))

(defun symon--slurp (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring (point-min) (line-end-position))))





;; FIXME this is broken
(defclass symon-linux-network-rx (symon-monitor-history)
  ((default-display-opts
     :type list
     :initform '(:index "RX:" :unit "KB/s" :sparkline t
                        :upper-bound 300
                        :lower-bound 0))))

(cl-defmethod symon-monitor-fetch ((this symon-linux-network-rx))
  (let ((last-value (symon-monitor-value this)))
    (with-temp-buffer
      (insert-file-contents "/proc/net/dev")
      (goto-char 1)
      (let ((rx 0))
        (while (search-forward-regexp "^[\s\t]*\\(.*\\):" nil t)
          (unless (string= (match-string 1) "lo")
            (setq rx (+ rx (read (current-buffer))))))
        (prog1 (when last-value
                 (/ (- rx last-value) symon-refresh-rate 1000))
          (setf last-value rx))))))

;; FIXME this is broken
(defclass symon-linux-network-tx (symon-monitor-history)
  ((default-display-opts
     :type list
     :initform '(:index "RX:" :unit "KB/s" :sparkline t
                        :upper-bound 100
                        :lower-bound 0))))

(cl-defmethod symon-monitor-fetch ((this symon-linux-network-tx))
  (let ((last-value (symon-monitor-value this)))
    (with-temp-buffer
      (insert-file-contents "/proc/net/dev")
      (goto-char 1)
      (let ((tx 0))
        (while (search-forward-regexp "^[\s\t]*\\(.*\\):" nil t)
          (unless (string= (match-string 1) "lo")
            (forward-word 8)
            (setq tx (+ tx (read (current-buffer))))))
        (prog1 (when last-value
                 (/ (- tx last-value) symon-refresh-rate 1000))
          (setf last-value tx))))))

;; + symon core

(defvar symon--active-monitors nil)
(defvar symon--display-active nil)
(defvar symon--active-page    nil)
(defvar symon--total-page-num nil)
(defvar symon--timer-objects  nil)
(defvar symon--faulty-monitors nil)

(defun symon--instantiate (monitor-or-symbol)
  "Create an instance."
  (cond
   ;; Instance of symon-monitor class.
   ((and (object-p monitor-or-symbol) (object-of-class-p monitor-or-symbol symon-monitor))
    monitor-or-symbol)

   ;; Symbol bound to a symon-monitor class.
   ((and (symbolp monitor-or-symbol)
         (class-p monitor-or-symbol)
         (child-of-class-p monitor-or-symbol symon-monitor))
    (make-instance monitor-or-symbol))

   ;; Expression which can evaluate to one of the above.
   ((consp monitor-or-symbol) (symon--instantiate (eval monitor-or-symbol)))))

(defun symon--setup (monitors)
  (thread-first
      (lambda (page)
        (remove-if-not #'identity (mapcar #'symon--instantiate page)))
    (mapcar symon-monitors)))

(defun symon--initialize ()
  (unless symon-monitors
    (warn "`symon-monitors' is empty."))
  (let ((monitors (symon--setup symon-monitors)))
    (mapc #'symon-monitor-setup (symon--flatten monitors))
    (setq symon--active-monitors monitors
          symon--display-active nil
          symon--faulty-monitors nil
          symon--total-page-num (length symon-monitors)
          symon--timer-objects
          (list (run-with-timer 0 symon-refresh-rate 'symon--redisplay)
                (run-with-idle-timer symon-delay t 'symon-display)))
    (add-hook 'pre-command-hook 'symon--display-end)
    (add-hook 'kill-emacs-hook 'symon--cleanup)))

(defun symon--cleanup ()
  (remove-hook 'kill-emacs-hook 'symon--cleanup)
  (remove-hook 'pre-command-hook 'symon--display-end)
  (mapc #'cancel-timer symon--timer-objects)
  (mapc #'symon-monitor-cleanup (symon--flatten symon--active-monitors)))

(defun symon--display-update ()
  "update symon display"
  (unless (or cursor-in-echo-area (active-minibuffer-window))
    (let ((message-log-max nil)  ; do not insert to *Messages* buffer
          (display-string nil))
      (message "%s" (substring
          (cl-loop for monitor in (elt symon--active-monitors symon--active-page)
                   for output = (symon-monitor-display monitor)
                   unless (or (null output) (string= "" output))
                   concat " "
                   concat output)
          1)))
    (setq symon--display-active t)))

(defun symon-display ()
  "Activate symon display."
  (interactive)
  (setq symon--active-page 0)
  (symon--display-update))

(defun symon--redisplay ()
  "Update symon display."
  (when symon--display-active
    (setq symon--active-page (% (1+ symon--active-page) symon--total-page-num))
    (symon--display-update)))

(defun symon--display-end ()
  "deactivate symon display."
  (setq symon--display-active nil))

;;;###autoload
(define-minor-mode symon-mode
  "Tiny graphical system monitor"
  :init-value nil
  :global t
  (if symon-mode (symon--initialize) (symon--cleanup)))

;; + provide

(provide 'symon)

;;; symon.el ends here
