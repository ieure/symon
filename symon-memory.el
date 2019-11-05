(require 'symon)
(require 'symon-windows)
(require 'symon-darwin)

 ;; Linux

(defclass symon-memory-linux (symon-monitor-history)
  ((default-display-opts
     :initform '(:index "MEM:" :unit "%" :sparkline t))))

(cl-defmethod symon-monitor-fetch ((this symon-memory-linux))
  (cl-destructuring-bind (memtotal memavailable memfree buffers cached)
      (symon-linux--read-lines
       "/proc/meminfo" (lambda (str) (and str (read str)))
       '("MemTotal:" "MemAvailable:" "MemFree:" "Buffers:" "Cached:"))
    (if memavailable
        (/ (* (- memtotal memavailable) 100) memtotal)
      (/ (* (- memtotal (+ memfree buffers cached)) 100) memtotal))))

 ;; macOS

(defclass symon-memory-darwin (symon-monitor-darwin symon-monitor-history))

(cl-defmethod symon-monitor-fetch ((this symon-memory-darwin))
  (symon--read-value-from-process-buffer "mem"))

 ;; Windows

(defclass symon-memory-windows (symon-monitor-windows symon-monitor-history))

(cl-defmethod symon-monitor-fetch ((this symon-memory-windows))
  (symon--read-value-from-process-buffer "mem"))

(provide 'symon-memory)
