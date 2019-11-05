;;; symon-temp.el --- Temperature monitor using hwmon  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ian Eure

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

;; Monitor system temperature(s) from hwmon.
;;
;; Core temperature example:
;;
;; (define-symon-temperature my/coretemp (symon-temp-coretemp-device) :index "Core:")
;; (add-to-list 'symon-monitors 'my/coretemp)
;;
;; Wifi chipset example:
;;
;; (define-symon-temperature my/wifitemp (symon-temp-find-name "iwlwifi") :index "WiFi:")
;; (add-to-list 'symon-monitors 'my/wifitemp)

;;; Code:

(require 'symon-monitor)
(require 's)

(defun symon-temp--sensors (device)
  "Return a list of all sensors under DEVICE."
  (let ((default-directory device))
    (cl-loop for sensor in (file-expand-wildcards "temp*_label")
             collect (cons (car (s-split "_" sensor))
                           (symon--slurp-line sensor)))))

(defun symon-temp--max (device sensor)
  "Return the maximum temperature dev DEVICE sensor SENSOR can report."
  (let ((default-directory device))
    (/ (read (symon--slurp-line (concat sensor "_max"))) 1000.0)))

(defun symon-temp--min (device sensor)
  "Return the minumum temperature dev DEVICE sensor SENSOR can report."
  (let ((default-directory device))
    (or (ignore-errors (/ (read (symon--slurp-line (concat sensor "_min"))) 1000.0))
        0.0)))

(defun symon-temp--current (device sensor)
  "Return the current temperature of SENSOR under DEVICE."
  (let ((default-directory device))
    (or (ignore-errors (/ (read (symon--slurp-line (concat sensor "_input"))) 1000.0))
        0.0)))

(defun symon-temp--average (device sensors)
  "Return the average of all SENSORS under DEVICE."
  (cl-loop for sensor in sensors
           count 1 into n
           sum (symon-temp--current device sensor) into total
           finally return (/ total n)))

(defun symon-temp--total-max (device sensors)
  "Return the maximum possible value of all sensors SENSORS under DEVICE."
  (seq-max
   (cl-loop for sensor in sensors
            collect (symon-temp--max device sensor))))

(defun symon-temp--total-min (device sensors)
  "Return the minimum possible value of all sensors SENSORS under DEVICE."
  (seq-min
   (cl-loop for sensor in sensors
            collect (symon-temp--min device sensor))))

(defun symon-temp-coretemp-device ()
  "Find the coretemp device, if there is one."
  (symon-temp-find-name "coretemp"))

(defun symon-temp-find-name (name)
  "Find hwmon named NAME if there is one."
  (cl-loop for device in (file-expand-wildcards "/sys/class/hwmon/hwmon*")
           for mon-name = (symon--slurp-line (concat device "/name"))
           when (string= name mon-name)
           return device))

(defun symon-temp--index-name (device sensors)
  (concat (if (> (length sensors) 1)
              ;; Use the device name
              (symon--slurp-line (concat device "/name"))
            ;; Use the sensor name
            (concat (symon--slurp-line (concat device "/name")) " "
                    (symon--slurp-line (concat device "/" (car sensors) "_label")))) ":"))

;;;###autoload
(defclass symon-temp (symon-monitor-history)
  ;; (name device &key (sensors nil) (index 'default) (sparkline nil))

  ((device :type string
           :documentation "Sysfs path containing temp sensor
outputs, ex. /sys/class/hwmon/hwmon2")
   (sensors :type list
            :documentation
            "The specific sensors of device to monitor.  When empty,
   uses all available temperature sensors for that device.  When
   multiple sensors are specified, the average is displayed.")

   (default-display-options :type list
     '(:sparkline t :unit "℃")))

  :documentation
  "A Symon monitor for hwmon temperatures.")

(cl-defmethod symon-monitor-setup ((this symon-temp))
  (with-slots (sensors default-display-opts) this
    (unless sensors
      (setq sensors(mapcar #'car (symon-temp--sensors device))))
    (plist-put default-display-opts
               :index (symon-temp--index-name device sensors))))

(cl-defmethod symon-monitor-fetch ((this symon-temp))
  (with-slots (device sensors) this
    (symon-temp--average device sensors)))

(provide 'symon-temp)
;;; symon-temp.el ends here
