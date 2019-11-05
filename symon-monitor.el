(require 'symon-sparkline)

(defun symon-monitor--make-history-ring (size)
  "like `(make-ring size)' but filled with `nil'."
  (cons 0 (cons size (make-vector size nil))))

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
                        display-opts))))

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

(defclass symon-monitor-history (symon-monitor)
  ((history-size :type integer :custom integer
                 :initform 50
                 :initarg :history-size)
   (history
    :accessor symon-monitor-history
    :documentation "Ring of historical monitor values"))

  :abstract t
  :documentation "Monitor class which stores a history of values.")

(cl-defmethod symon-monitor-setup ((this symon-monitor-history))
  (oset this history (symon-monitor--make-history-ring
                      (oref this history-size)))
  (cl-call-next-method))

(cl-defmethod symon-monitor-history ((this symon-monitor-history))
  (oref this history))

(cl-defmethod symon-monitor-value ((this symon-monitor-history))
  (car (symon-monitor-history this)))

(cl-defmethod symon-monitor-update :before ((this symon-monitor-history))
  (ring-insert (oref this history) (symon-monitor-fetch this)))

(cl-defmethod symon-monitor-display ((this symon-monitor-history))
  "Default display method for Symon monitors."
  (let* ((lst (ring-elements (oref this history)))
         (plist (oref this display-opts))
         (sparkline (plist-get plist :sparkline))
         (upper-bound (plist-get plist :upper-bound))
         (lower-bound (plist-get plist :lower-bound)))

    (concat (cl-call-next-method)
            (when (and sparkline (window-system))
              (let ((sparkline (symon--make-sparkline
                                lst lower-bound upper-bound)))
                (when symon-sparkline-use-xpm
                  (setq sparkline
                        (symon--convert-sparkline-to-xpm sparkline)))
                (propertize " " 'display sparkline))))))

(provide 'symon-monitor)
