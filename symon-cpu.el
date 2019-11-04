
(require 'symon)

 ;; Linux

(defconst piemon-cpu--linux-path
  "/sys/devices/system/cpu/")

(defun piemon-cpu--path (cpu-num &rest stats)
  "Return the path for stat STAT on CPU CPU-NUM."
  (concat (format "%scpu%d" piemon-cpu-path cpu-num)
          (when stats (concat "/" (c-concat-separated stats "/")))))

(defun piemon--slurp-cpu (cpu-num &rest stats)
  "Return value of stat STAT on CPU CPU-NUM."
  (piemon--slurp-line (apply #'piemon-cpu--path cpu-num stats)))

(defun piemon-cpu--range (high-maybe-low)
  "Turn range HIGH-MAYBE-LOW into a sequence of numbers."
  (if (not (string-match-p "-" high-maybe-low))
      (list (read high-maybe-low))
    (apply #'number-sequence (mapcar #'read (split-string high-maybe-low "-")))))

(defun piemon-cpu--ranges (ranges)
  "Turn RANGES into a list of discrete numbers.

   RANGES is a comma-separated string of numbers or N-M,
   representing that sequence."
  (save-match-data
    (cl-loop for range in (split-string ranges ",")
             append (piemon-cpu--range range))))

(defun piemon-cpu--cpus ()
  "Return (ONLINE-CPU-IDS . TOTAL-NUM-CPUS)."
  (cons
   (thread-first (concat piemon-cpu-path "/online")
     (piemon--slurp-line)
     (piemon-cpu--ranges))
   (thread-first (concat piemon-cpu-path "/possible")
     (piemon--slurp-line)
     (piemon-cpu--ranges)
     (length))))

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

(defclass symon-cpu-linux (symon-monitor-history)
  ((last-total-ticks :type integer :initform 0)
   (last-idle-ticks :type integer :initform 0)
   (default-display-opts :type list
     :initform '(:index "CPU:" :unit "%" :sparkline t))))

(cl-defmethod symon-monitor-fetch ((this symon-cpu-linux))
  (cl-destructuring-bind (cpu)
      (symon-linux--read-lines
       "/proc/stat" (lambda (str) (mapcar 'read (split-string str nil t))) '("cpu"))
    (with-slots (last-total-ticks last-idle-ticks) this
      (let ((total (apply '+ cpu)) (idle (nth 3 cpu)))
        (prog1 (let ((total-diff (- total last-total-ticks))
                     (idle-diff (- idle last-idle-ticks)))
                 (unless (zerop total-diff)
                   (/ (* (- total-diff idle-diff) 100) total-diff)))
          (setf last-total-ticks total
                last-idle-ticks idle))))))

 ;; macOS

(defclass symon-cpu-darwin (symon-monitor-darwin symon-monitor-history))

(cl-defmethod symon-monitor-fetch ((this symon-cpu-darwin))
  (symon--read-value-from-process-buffer "cpu"))


 ;; Windows

(defclass symon-cpu-windows (symon-monitor-windows symon-monitor-history))

(cl-defmethod symon-monitor-fetch ((this symon-cpu-windows))
  (symon--read-value-from-process-buffer "cpu"))

(provide 'symon-cpu)
