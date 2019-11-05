(require 'symon-monitor)

(defun symon-darwin--maybe-start-process ()
  (symon--maybe-start-process (format "
while true; do
    echo \"----\"

    interface=`route get 0.0.0.0 | grep interface | awk '{print $2}'`
    s=`netstat -bi -I $interface | tail -1`;
    echo $s | awk '{print \"rx:\"$7}'
    echo $s | awk '{print \"tx:\"$8}'

    s=`hostinfo  | grep 'Load average' | awk '{print \"cpu:\"$3}' | sed 's/,//'`
    echo $s

    m1=`sysctl hw.memsize | sed 's/.*:\s*//'`
    m_active=`vm_stat | grep 'Pages active' | sed 's/.*: *//'`
    m_wired=`vm_stat | grep 'Pages wired' | sed 's/.*: *//'`

    s=`echo \"scale=2; (($m_active+$m_wired)*4096*100 / $m1)\"| bc -l`
    echo \"mem:$s\"

    sleep %d
done" symon-refresh-rate)))

(defclass symon-monitor-darwin ())

(cl-defmethod symon-monitor-setup ((this symon-monitor-darwin))
  (symon-darwin--maybe-start-process))

(cl-defmethod symon-monitor-cleanup ((this symon-monitor-darwin))
  (symon-darwin--maybe-kill-process))

(defclass symon-darwin-network-rx (symon-monitor-darwin symon-linux-network-rx))

(cl-defmethod symon-monitor-fetch ((this symon-darwin-network-rx))
  (with-slots (last-value) this
    (let ((rx (symon--read-value-from-process-buffer "rx")))
      (prog1 (when last-value
               (/ (- rx last-value) symon-refresh-rate 1000))
        (setf last-value rx)))))

(defclass symon-darwin-network-tx (symon-monitor-darwin symon-linux-network-tx))

(cl-defmethod symon-monitor-fetch ((this symon-darwin-network-tx))
  (with-slots (last-value) this
    (let ((tx (symon--read-value-from-process-buffer "tx")))
      (prog1 (when last-value
               (/ (- tx last-value) symon-refresh-rate 1000))
        (setf last-value tx)))))

(provide 'symon-darwin)
