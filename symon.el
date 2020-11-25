;;; symon.el --- tiny graphical system monitor     -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2020 zk_phi, 2019-2020 Ian Eure.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
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

;; Author: ieure
;; URL: http://githhub.com/ieure/symon
;; Version: 2.0.0

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

;; Xymon
;; 2.0.0 Largely rewritten, Windows and Darwin support dropped.
;;
;; Symon:
;; 1.0.0 first release
;; 1.1.0 add option symon-sparkline-thickness
;; 1.1.1 add symon-windows-page-file-monitor
;; 1.1.2 add darwin support (mac os x)
;; 1.2.0 add paging feature

;;; Code:

(defconst symon-version "2.0.0")

(defgroup symon nil
  "Tiny graphical system monitor"
  :group 'emacs)

;; + customs

;; core

(defun symon--set-and-restart (sym value)
  (set-default sym value)
  (when symon-mode
    (symon-mode -1)
    (symon-mode 1)))

(defcustom symon-refresh-rate 4
  "How often to obtain new values from the monitors."
  :group 'symon
  :type 'integer
  :set 'symon--set-and-restart)

(defcustom symon-delay 2
  "Delay in seconds until Symon appears."
  :group 'symon
  :type 'integer
  :set 'symon--set-and-restart)

(defcustom symon-monitors
  (list '((symon-time :display-opts '(:format "%a %b %d %H:%M"))
          (symon-battery)
          (symon-cpu-linux :display-opts '(:sparkline (:type gridded)))
          (symon-memory-linux)))

  "List of list of monitors.

Each outer list is a page.  Symon rotates through pages as it
refreshes (every SYMON-REFRESH-RATE seconds).

Each inner list is a list of monitors.  Members of that list may
be the symbol of a monitor; a direct monitor value; or an
expression which evaluates to one of those things."

  :group 'symon
  :risky t
  :set 'symon--set-and-restart
  :type '(repeat
          (repeat :tag "Page of monitors"
                  (choice
                   (symbol :tag "Class or object")
                   (sexp)))))

;; + utilities
;;   + general

(defun symon--flatten (lst)
  "flatten LST"
  (if (consp lst)
      (apply 'nconc (mapcar 'symon--flatten lst))
    (list lst)))

;; + symon core

(defvar symon--active-monitors nil)
(defvar symon--display-active nil)
(defvar symon--active-page    nil)
(defvar symon--total-page-num nil)
(defvar symon--timer-objects  nil)
(defvar symon--faulty-monitors nil)

(defun symon--instantiate* (monitor-or-symbol)
  "Create an instance of a monitor, from MONITOR-OR-SYMBOL.

   MONITOR-OR-SYMBOL may be:

   - A symbol which is bound to a monitor class.  The class will be
     instantiated with no arguments.
   - A symbol which is bound to a monitor object.
   - A monitor object itself.
   - An expression which evaluates to one of the above."
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
   ((consp monitor-or-symbol) (symon--instantiate* (eval monitor-or-symbol)))
   ((null monitor-or-symbol) monitor-or-symbol)
   (t (error "Don't know how to instantiate type `%s' %s" (type-of monitor-or-symbol) monitor-or-symbol))))

(defun symon--instantiate (pages-of-monitors)
  "Instatiate Symon monitors in PAGES-OF-MONITORS."
  (thread-first
      (lambda (page-of-monitors)
        (cl-remove-if #'null (mapcar #'symon--instantiate* page-of-monitors)))
    (mapcar pages-of-monitors)))

(defun symon--initialize ()
  "Prepare Symon to monitor the system."
  (unless symon-monitors
    (warn "`symon-monitors' is empty."))
  (let ((monitors (symon--instantiate symon-monitors)))
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
  "Clean up monitors, disabling Symon."
  (remove-hook 'kill-emacs-hook 'symon--cleanup)
  (remove-hook 'pre-command-hook 'symon--display-end)
  (mapc #'cancel-timer symon--timer-objects)
  (mapc #'symon-monitor-cleanup (symon--flatten symon--active-monitors)))

(defun symon--display-catching-errors (monitor)
  "Display MONITOR, trapping errors, if they occur."
  (condition-case e
      (symon-monitor-display monitor)
    (error (symon-monitor--maybe-warn monitor e 'fetch-errors-warned "Fetch"))))

(defun symon--display-update ()
  "Display current page of monitors in the minibuffer."
  (unless (or cursor-in-echo-area (active-minibuffer-window))
    (let ((message-log-max nil)  ; do not insert to *Messages* buffer
          (display-string nil))
      (message "%s" (substring
          (cl-loop for monitor in (elt symon--active-monitors symon--active-page)
                   for output = (symon--display-catching-errors monitor)
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

(provide 'symon)

;;; symon.el ends here
