(require 'symon-monitor)
(require 'symon-darwin)
(require 'symon-windows)

;; FIXME this is broken
(defclass symon-linux-network-rx (symon-monitor-history)
  ((default-display-opts
     :type list
     :initform '(:index "RX:" 
                        :unit "KB/s"
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
     :initform '(:index "RX:" 
                        :unit "KB/s"
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

(provide 'symon-network)
