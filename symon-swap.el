(require 'symon-monitor)
(require 'symon-windows)

 ;; Linux

(defclass symon-swap-linux (symon-monitor)
  ((default-display-opts :initform '(:index "SWAP:" :style 'megabytes :sparkline nil)))
  :documentation "Monitor for Linux system swap.

The following display-opts are supported:

:style - May be 'MEGABYTES (mb swapped) or 'PERCENT (% swap used).
:hide  - When non-NIL, only show monitor if swap is being used.")

(cl-defmethod symon-monitor-fetch ((this symon-swap-linux))
  (cl-destructuring-bind (swaptotal swapfree)
      (symon-linux--read-lines
       "/proc/meminfo" 'read '("SwapTotal:" "SwapFree:"))
    (let ((swapped-bytes (- swaptotal swapfree)))
      (pcase (plist-get (oref this display-opts) :style)
        ('megabytes (/ swapped-bytes 1000))
        (_ (round (* 100 (/ swapped-bytes (float swaptotal)))))))))

(cl-defmethod symon-monitor-display ((this symon-swap-linux))
  (when (>= (symon-monitor-value this) 0)
    (cl-call-next-method)))

(cl-defmethod symon-monitor-setup ((this symon-swap-linux))
  (plist-put (oref this display-opts) :unit
             (if (equal 'megabytes (plist-get (oref this display-opts) :style))
                 "mb" "%"))
  (cl-call-next-method))


 ;; Windows

(defclass symon-swap-windows (symon-monitor-windows symon-monitor-history)
  ((default-display-opts
     :initform `(:index "PF:" 
                        :unit "MB"
                        :upper-bound ,symon-windows-page-file-upper-bound))))

(cl-defmethod symon-monitor-fetch ((this symon-swap-windows))
  (symon--read-value-from-process-buffer "swap"))

(provide 'symon-swap)
