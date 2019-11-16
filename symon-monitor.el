;;; symon-monitor.el --- Monitor classes             -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Ian Eure
;; Copyright (C) 2015 zk_phi

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
(require 'ert)

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
     :documentation "Default display options for this monitor.")

   ;; Internal slots

   (timer
    :documentation "Fires `symon-monitor-fetch' for this monitor.")
   (value
    :accessor symon-monitor-value
    :documentation "Most recent fetched value.")
   (fetch-errors-warned
    :initform nil
    :documentation "List of errors from fetching.

This is a list of symbols of errors signaled when calling
`symon-monitor-fetch' on this monitor.  When fetching a monitor's
value signals an error, it's displayed as a warning.  Subsequent
errors of the same type are suppressed.")
   (display-errors-warned
    :initform nil
    :documentation "List of errors from displaying.

This is a list of symbols of errors signaled when calling
`symon-monitor-display' on this monitor.  When displaying a
monitor signals an error, it's displayed as a warning.
Subsequent errors of the same type are suppressed."))

  :abstract t
  :documentation "Base (default) Symon monitor class.")

(cl-defmethod symon-monitor--maybe-warn ((this symon-monitor) error errors-warned type)
  (cl-destructuring-bind (error-symbol &rest _) error
    (unless (memq error-symbol (slot-value this errors-warned))
      (push error-symbol (slot-value this errors-warned))
      (warn "%s of %s failed: %s: %s"
            type (eieio-object-class this) error-symbol data)
      ;; Evaluate to nil -- fetch failed, so there's no value.
      nil)))

(cl-defmethod symon-monitor-update ((this symon-monitor))
  "Update THIS, storing the latest value."
  (oset this value
        (condition-case error
            (symon-monitor-fetch this)
          (error (symon-monitor--maybe-warn this error 'fetch-errors-warned "Update")))))

(defun symon-monitor--plist-merge (defaults user)
  (let ((opts (copy-list defaults))
        (user (copy-list user)))
    (while user
      (let ((k (pop user))
            (v (pop user)))
        (setq opts
              (plist-put opts k
                         (if (consp v)
                             ;; If user options are a list, recursively merge.
                             (symon-monitor--plist-merge (plist-get opts k) v)
                           v)))))
    opts))

(cl-defmethod initialize-instance :after ((this symon-monitor) &rest _)
  ;; Merge display opts
  (when (slot-boundp this 'default-display-opts)
    (with-slots (display-opts default-display-opts) this
      (setf display-opts (symon-monitor--plist-merge
                          default-display-opts
                          display-opts)))))

(cl-defmethod symon-monitor-setup ((this symon-monitor))
  "Setup this monitor.

This method is called when activating `symon-mode'."

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
  "Fetch the current monitor value."
  (error "No `symon-monitor-fetch' defined for monitor %s" (eieio-object-class this)))

(cl-defmethod symon-monitor-display ((this symon-monitor))
  "Default display method for Symon monitors."
  (with-slots (display-opts) this
    (let ((val (symon-monitor-value this))
          (index (or (plist-get display-opts :index) ""))
          (unit (or (plist-get display-opts :unit) "")))
      (concat index
              (if (not (numberp val)) "N/A"
                (format "%d%s" val unit))))))


 ;; History monitor

(defclass symon-monitor-history (symon-monitor)
  ((history-size :type integer :custom integer
                 :initform 50
                 :initarg :history-size)

   (default-display-opts :initform nil)

   ;; Internal slots

   (history
    :accessor symon-monitor-history
    :documentation "Ring of historical monitor values")
   (sparkline
    :initform nil
    :documentation "Sparkline rendering instance for this monitor."))

  :abstract t
  :documentation "Monitor class which stores a history of values.")

(cl-defmethod initialize-instance :before ((this symon-monitor-history) &rest)
  "Initialize default display options."
  (plist-put (oref this default-display-opts) :sparkline '(:type gridded)))

(cl-defmethod initialize-instance :after ((this symon-monitor-history) &rest _)
  (with-slots (history-size display-opts history sparkline) this
    (setf history (symon-monitor--make-history-ring history-size))
    (when-let ((sparkline-opts (plist-get display-opts :sparkline)))
      (setf sparkline (apply #'symon-sparkline sparkline-opts)))))

(cl-defmethod symon-monitor-history ((this symon-monitor-history))
  (oref this history))

(cl-defmethod symon-monitor-value ((this symon-monitor-history))
  (car (ring-elements (symon-monitor-history this))))

(cl-defmethod symon-monitor-update :around ((this symon-monitor-history))
  (ring-insert (oref this history) (symon-monitor-fetch this)))

(cl-defmethod symon-monitor-display ((this symon-monitor-history))
  "Default display method for Symon monitors."
  (with-slots (sparkline) this
      (concat
       (cl-call-next-method)
       (when (and sparkline (window-system))
         (concat " "
                 (propertize " " 'display (symon-sparkline-graph sparkline (ring-elements (symon-monitor-history this)))))))))



(ert-deftest symon-monitor--test-plist-merge ()
  ;; Unique keys are added
  (should (equal '(:a 1 :b 2) (symon-monitor--plist-merge '(:a 1) '(:b 2))))

  ;; Duplicate keys are replaced
  (should (equal '(:a 2) (symon-monitor--plist-merge '(:a 1) '(:a 2))))

  ;; Recursive lists are merged
  (should (equal '(:a (:foo 1 :bar 2)) (symon-monitor--plist-merge '(:a (:foo 1))
                                                                   '(:a (:bar 2))))))



(ert-deftest symon-monitor--test-fetch-error ()
  (defclass symon-monitor--test-fetch-error (symon-monitor) nil)
  (cl-defmethod symon-monitor-fetch ((this symon-monitor--test-fetch-error))
    (error "Testing"))

  (let ((m (symon-monitor--test-fetch-error)))
    (should (null (memq 'err (oref m fetch-errors-warned))))
    (should (null (symon-monitor-update m)))
    (should (memq 'error (oref m fetch-errors-warned)))))

(ert-deftest symon-monitor--test-display-opts ()
  (defclass symon-monitor--test-display-opts (symon-monitor) nil)

  (let ((m (symon-monitor--test-display-opts)))
    (should (slot-boundp m 'display-opts))
    (should (null (oref m display-opts))))

  (let ((m (symon-monitor--test-display-opts :display-opts '(:foo 1))))
    (should (slot-boundp m 'display-opts))
    (should (equal '(:foo 1) (oref m display-opts)))))

(ert-deftest symon-monitor--test-default-display-opts ()
  (defclass symon-monitor--test-default-display-opts (symon-monitor)
    ((default-display-opts :initform '(:foo 1))))

  (let ((m (symon-monitor--test-default-display-opts)))
    (should (slot-boundp m 'display-opts))
    (should (equal '(:foo 1) (oref m display-opts))))

  (let ((m (symon-monitor--test-default-display-opts :display-opts '(:bar 2))))
    (should (slot-boundp m 'display-opts))
    (should (equal '(:foo 1 :bar 2) (oref m display-opts)))))



(ert-deftest symon-monitor-history--test-update ()
  (defclass symon-monitor-history--test-update (symon-monitor-history)
    ((n :initform 0)))

  (cl-defmethod symon-monitor-fetch ((this symon-monitor-history--test-update))
    (incf (slot-value this 'n)))

  (let ((m (symon-monitor-history--test-update)))
    (should (= 1 (symon-monitor-update m)))
    (should (= 2 (symon-monitor-update m)))
    (should (= (symon-monitor-update m) (symon-monitor-value m)))
    (should (equal '(3 2 1) (seq-take (ring-elements (symon-monitor-history m)) 3)))))

(provide 'symon-monitor)
;;; symon-monitor.el ends here
