(require 'symon-monitor)
(require 'symon-windows)
(require 'symon-darwin)

 ;; Linux

;;;###autoload
(defclass symon-memory-linux (symon-monitor-history)
  ((default-display-opts
     :initform '(:index "MEM:" :unit "%"))))

(cl-defmethod symon-monitor-fetch ((this symon-memory-linux))
  (cl-destructuring-bind (memtotal memavailable memfree buffers cached)
      (symon-monitor--linux-read-lines
       "/proc/meminfo" (lambda (str) (and str (read str)))
       '("MemTotal:" "MemAvailable:" "MemFree:" "Buffers:" "Cached:"))
    (if memavailable
        (/ (* (- memtotal memavailable) 100) memtotal)
      (/ (* (- memtotal (+ memfree buffers cached)) 100) memtotal))))

 ;; macOS

(defclass symon-memory-darwin (symon-monitor-darwin symon-monitor-history)
  nil)

(cl-defmethod symon-monitor-fetch ((this symon-memory-darwin))
  (symon--read-value-from-process-buffer "mem"))

 ;; Windows

(defclass symon-memory-windows (symon-monitor-windows symon-monitor-history)
  nil)

(cl-defmethod symon-monitor-fetch ((this symon-memory-windows))
  (symon--read-value-from-process-buffer "mem"))

(provide 'symon-memory)
