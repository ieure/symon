
(require 'symon-monitor)
(require 'symon-darwin)
(require 'symon-cpu)

 ;; Linux

(defun symon-cpu--freq (cpu)
  "Return the clock frequency of CPU."
  (cl-loop for stat in '("cpuinfo_min_freq" "cpuinfo_max_freq" "scaling_cur_freq")
           collect (read (symon--slurp-cpu cpu "cpufreq" stat))))

(defun symon-cpu--freqs (cpus)
  "Return the clock frequency information for CPUS."
  (mapcar #'symon-cpu--freq cpus))

(defun symon-cpu--minfreq (&optional cpus)
  (/ (reduce #'min (mapcar #'car (symon-cpu--freqs (car (symon-cpu--cpus))))) 1000.0))

(defun symon-cpu--maxfreq (&optional cpus)
  (/ (reduce #'max (mapcar #'cadr (symon-cpu--freqs (car (symon-cpu--cpus))))) 1000.0))

;;;###autoload
(defclass symon-cpufreq-linux (symon-monitor-history)
  ((cpus :type list :initarg :cpus)
   (default-display-opts :initform nil)))

(cl-defmethod initialize-instance :around ((this symon-cpufreq-linux) &rest _)
  (unless (slot-boundp this 'cpus)
    (oset this cpus (car (symon-cpu--cpus))))

  (with-slots (cpus default-display-opts) this
    (plist-put default-display-opts :sparkline 
               `(:type gridded
                       :lower-bound ,(symon-cpu--minfreq cpus)
                       :upper-bound ,(symon-cpu--maxfreq cpus))))
  (cl-call-next-method))

(cl-defmethod symon-monitor-fetch ((this symon-cpufreq-linux))
  "Return average of all cores in MHz."
  (with-slots (cpus) this
    (/ (reduce #'+ (mapcar #'caddr (symon-cpu--freqs cpus))) (length cpus) 1000.0)))

(cl-defmethod symon-monitor-display ((this symon-cpufreq-linux))
  (with-slots (cpus) this
    (let ((mhz (symon-monitor-value this)))
      (if (> mhz 1000)
          (format "%.1fGHz" (/ mhz 1000.0))
        (format "%dMHz" mhz)))))

(provide 'symon-cpufreq)
