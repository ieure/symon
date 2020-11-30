;;; symon-cpu.el --- CPU monitor for Symon           -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; Keywords: hardware

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

;;

;;; Code:

(require 'symon-monitor)

(defconst symon-cpu--linux-path
  "/sys/devices/system/cpu/")

(defun symon-cpu--path (cpu-num &rest stats)
  "Return the path for stat STAT on CPU CPU-NUM."
  (concat (format "%scpu%d" symon-cpu--linux-path cpu-num)
          (when stats (concat "/" (c-concat-separated stats "/")))))

(defun symon--slurp-cpu (cpu-num &rest stats)
  "Return value of stat STAT on CPU CPU-NUM."
  (symon-monitor--slurp (apply #'symon-cpu--path cpu-num stats)))

(defun symon-cpu--range (high-maybe-low)
  "Turn range HIGH-MAYBE-LOW into a sequence of numbers."
  (if (not (string-match-p "-" high-maybe-low))
      (list (read high-maybe-low))
    (apply #'number-sequence (mapcar #'read (split-string high-maybe-low "-")))))

(defun symon-cpu--ranges (ranges)
  "Turn RANGES into a list of discrete numbers.

   RANGES is a comma-separated string of numbers or N-M,
   representing that sequence."
  (save-match-data
    (cl-loop for range in (split-string ranges ",")
             append (symon-cpu--range range))))

(defun symon-cpu--cpus ()
  "Return (ONLINE-CPU-IDS . TOTAL-NUM-CPUS)."
  (cons
   (thread-first (concat symon-cpu--linux-path "/online")
     (symon-monitor--slurp)
     (symon-cpu--ranges))
   (thread-first (concat symon-cpu--linux-path "/possible")
     (symon-monitor--slurp)
     (symon-cpu--ranges)
     (length))))

(defun symon-cpu--freq (cpu)
  "Return the clock frequency of CPU."
  (cl-loop for stat in '("cpuinfo_min_freq" "cpuinfo_max_freq" "scaling_cur_freq")
           collect (read (symon--slurp-cpu cpu "cpufreq" stat))))

(defun symon-cpu--freqs (cpus)
  "Return the clock frequency information for CPUS."
  (mapcar #'symon-cpu--freq cpus))

(defun symon-cpu--minfreq (&optional cpus)
  (/ (cl-reduce #'min (mapcar #'car (symon-cpu--freqs (car (symon-cpu--cpus))))) 1000.0))

(defun symon-cpu--maxfreq (&optional cpus)
  (/ (cl-reduce #'max (mapcar #'cadr (symon-cpu--freqs (car (symon-cpu--cpus))))) 1000.0))

(defclass symon-cpu-linux (symon-monitor-history)
  ((last-total-ticks :type integer :initform 0)
   (last-idle-ticks :type integer :initform 0)
   (default-display-opts :type list
     :initform '(:index "CPU:" :unit "%"))))

(cl-defmethod symon-monitor-fetch ((this symon-cpu-linux))
  (cl-destructuring-bind (cpu)
      (symon-monitor--linux-read-lines
       "/proc/stat" (lambda (str) (mapcar 'read (split-string str nil t))) '("cpu"))
    (with-slots (last-total-ticks last-idle-ticks) this
      (let* ((total (apply '+ cpu))
             (total-diff (- total last-total-ticks))
             (idle (nth 3 cpu))
             (idle-diff (- idle last-idle-ticks)))
        (if (zerop total-diff)
            ;; If ticks haven't increased since last fetch, return the last value.
            (symon-monitor-value this)
          (prog1 (/ (* (- total-diff idle-diff) 100) total-diff)
            (setf last-total-ticks total
                  last-idle-ticks idle)))))))

(provide 'symon-cpu)
;;; symon-cpu.el ends here
