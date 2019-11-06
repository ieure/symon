;;; symon-monitor.el --- Monitor classes             -*- lexical-binding: t; -*-

;; Copyright (C) 2015 zk_phi
;; Copyright (C) 2019  Ian Eure

;; Author: zk_phi
;; Author: Ian Eure <ian@retrospec.tv>
;; Keywords: extensions, unix

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

;; This file contains the base code for Symon monitors.

;;; Code:

(require 'symon-sparkline)

 ;; I/O helpers

(defun symon-monitor--make-history-ring (size)
  "like `(make-ring size)' but filled with `nil'."
  (cons 0 (cons size (make-vector size nil))))

(defun symon-monitor--linux-read-lines (file reader indices)
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

(defun symon-monitor--slurp (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring (point-min) (line-end-position))))


 ;; Process management

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


 ;; Class definitions

(defclass symon-monitor ()
  ((interval :type integer
             :initform 4
             :initarg :interval
             :documentation "Fetch interval in seconds.")
   (display-opts :type list
                 :initform nil
                 :initarg :display-opts
                 :documentation "User-specified display options for this monitor.")
   (default-display-opts :type list
     :initform nil
     :type list
     :documentation "Default display options for this monitor.")

   ;; Internal slots

   (timer
    :documentation "Fires `symon-monitor-fetch' for this monitor.")
   (value
    :accessor symon-monitor-value
    :documentation "Most recent value"))

  :abstract t
  :documentation "Base (default) Symon monitor class.")

(cl-defmethod symon-monitor-update ((this symon-monitor))
  "Update THIS, storing the latest value."
  (oset this value (symon-monitor-fetch this)))

(defun symon-monitor--plist-merge (defaults user)
  (let ((opts (copy-list defaults))
        (user (copy-list user)))
    (while user
      (setq opts (plist-put opts (pop user) (pop user))))
    opts))

(cl-defmethod symon-monitor-setup ((this symon-monitor))
  "Setup this monitor.

This method is called when activating `symon-mode'."

  ;; Merge display opts
  (with-slots (display-opts default-display-opts) this
    (setq display-opts (symon-monitor--plist-merge
                        default-display-opts
                        display-opts)))

  (oset this timer
        (run-with-timer 0 (oref this interval)
                        (apply-partially #'symon-monitor-update this))))

(cl-defmethod symon-monitor-cleanup ((this symon-monitor))
  "Cleanup the monitor.

   This method is called when deactivating `symon-mode'."
  (when (slot-boundp this 'timer)
    (cancel-timer (oref this timer))
    (oset this timer nil)))

(cl-defmethod symon-monitor-fetch ((this symon-monitor))
  "Fetch the current monitor value.")

(cl-defmethod symon-monitor-display ((this symon-monitor))
  "Default display method for Symon monitors."
  (let* ((val (car (ring-elements (oref this history))))
         (plist (oref this display-opts))
         (index (plist-get plist :index))
         (unit (plist-get plist :unit)))
    (concat index
            (if (not (numberp val)) "N/A"
              (format "%d%s" val unit)))))


 ;; History monitor

(defclass symon-monitor-history (symon-monitor)
  ((history-size :type integer :custom integer
                 :initform 50
                 :initarg :history-size)

   ;; Internal slots

   (history
    :accessor symon-monitor-history
    :documentation "Ring of historical monitor values")
   (sparkline
    :initform nil
    :documentation "Sparkline rendering instance for this monitor."))

  :abstract t
  :documentation "Monitor class which stores a history of values.")

(cl-defmethod symon-monitor-setup ((this symon-monitor-history))
  (cl-call-next-method)

  (with-slots (history-size display-opts history sparkline) this
    (setf history (symon-monitor--make-history-ring history-size))
    (when-let ((sparkline-opts (plist-get display-opts :sparkline)))
      (setf sparkline (apply #'symon-sparkline sparkline-opts)))))

(cl-defmethod symon-monitor-history ((this symon-monitor-history))
  (oref this history))

(cl-defmethod symon-monitor-value ((this symon-monitor-history))
  (oref this value))

(cl-defmethod symon-monitor-update :before ((this symon-monitor-history))
  (ring-insert (oref this history) (symon-monitor-fetch this)))

(cl-defmethod symon-monitor-display ((this symon-monitor-history))
  "Default display method for Symon monitors."
  (with-slots (sparkline) this
      (concat
       (cl-call-next-method)
       (when (and sparkline (window-system))
         (propertize " " 'display (symon-sparkline-graph (ring-elements (symon-monitor-history this))))))))



(provide 'symon-monitor)
;;; symon-monitor.el ends here
