
(require 'symon)
(require 'symon-cpu)

 ;; Linux

(defun piemon-cpu--freq (cpu)
  "Return the clock frequency of CPU."
  (cl-loop for stat in '("cpuinfo_min_freq" "cpuinfo_max_freq" "scaling_cur_freq")
           collect (read (piemon--slurp-cpu cpu "cpufreq" stat))))

(defun piemon-cpu--freqs (cpus)
  "Return the clock frequency information for CPUS."
  (mapcar #'piemon-cpu--freq cpus))

(defun piemon-cpu--minfreq (&optional cpus)
  (/ (reduce #'min (mapcar #'car (piemon-cpu--freqs (car (piemon-cpu--cpus))))) 1000.0))

(defun piemon-cpu--maxfreq (&optional cpus)
  (/ (reduce #'max (mapcar #'cadr (piemon-cpu--freqs (car (piemon-cpu--cpus))))) 1000.0))

(defclass symon-cpufreq-linux (symon-monitor-history)
  ((cpus :initform (car (symon-cpu--cpus)))))

(cl-defmethod symon-monitor-setup ((this symon-cpu-freq-linux))
  (with-slots (display-opts cpus) this
    (unless cpus
      (setf cpus (piemon-cpu--cpus)))

    (unless (null (plist-get display-opts :sparkline))
      
    (plist-put display-opts :))
    
    )
  
  (cl-call-next-method))

(cl-defmethod symon-monitor-fetch ((this symon-cpufreq-linux))
  "Return average of all cores in MHz."
  (with-slots (cpus) this
    (/ (reduce #'+ (mapcar #'caddr cpus)) (length cpu) 1000.0)))

(cl-defmethod symon-monitor-display ((this symon-cpufreq-linux))
  (with-slots (cpus) this
    (let ((mhz (symon-monitor-value this))))
    (if (> mhz 1000)
        (format "%.1fGHz" (/ mhz 1000.0))
      (format "%dMHz" mhz))))

(provide 'symon-cpufreq)
