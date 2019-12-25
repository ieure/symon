;;; symon-battery.el --- Battery monitor for symon   -*- lexical-binding: t; -*-

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

;; Battery monitor for Symon, using built-in battery.el.

;;; Code:

(require 'symon)
(require 'battery)

(defgroup symon-battery nil
  "Customization for Symon battery monitor."
  :group 'symon)

(defface symon-battery-low-face
  '((t :background "Red1"
       :foreground "white"
       :weight bold))
  "Face for when the battery is discharging, and has less than 30
minutes left before depletion."
  :group 'symon-battery)

(defface symon-battery-medium-face
  '((t :foreground "DarkOrange"
       :weight bold))
  "Face for when the battery is discharging, and has between 30
and 59 minutes left before depletion."
  :group 'symon-battery)

(defface symon-battery-full-face
  '((t :foreground "ForestGreen"))
  "Face for when the battery is discharging, and holds more than
an hour's charge."
  :group 'symon-battery)

(defface symon-battery-charging-face
  '((t :inherit symon-battery-full-face))
  "Face for when the battery is being charged."
  :group 'symon-battery)

(defun symon-battery--capacity-face (charging percent)
  (if charging 'symon-battery-charging-face
    (cond
     ((>= percent 50) 'symon-battery-full-face)
     ((>= percent 25) 'symon-battery-medium-face)
     (t 'symon-battery-low-face))))

(defun symon-battery--time-face (charging time-left)
  "Return monitor face based on CHARGING and TIME-LEFT.

If the battery is being charged, returns
`symon-battery-charging-face'

If the battery is discharging, and there's between 59-30 minutes
estimated time to depletion, returns `symon-battery-medium-face'.

If the battery is discharging, and there's between 0-29 minutes
estimated time to depletion, returns `symon-battery-low-face'."

  (if charging 'symon-battery-charging-face
    (cl-destructuring-bind (hh mm) (mapcar #'read (split-string time-left ":"))
      (cond
       ((and (= hh 0) (> mm 30)) 'symon-battery-medium-face)
       ((= hh 0) 'symon-battery-low-face)
       (t 'symon-battery-full-face)))))

(defun symon-battery--face (charging percent time-left)
  (if (string= "N/A" time-left)
      (symon-battery--capacity-face charging percent)
    (symon-battery--time-face charging time-left)))

;;;###autoload
(defclass symon-battery (symon-monitor)
  ((interval :initform 10)
   (default-display-opts
     :initform '(:charging-indicator "^"
                                     :discharging-indicator "v")))
  :documentation "Battery monitor for Symon.")

(cl-defmethod symon-monitor-fetch ((this symon-battery))
  "Return battery status."
  (when battery-status-function
    (funcall battery-status-function)))

(cl-defmethod symon-battery--indicator ((this symon-battery) charging)
  "Return the battery charging or discharging indicator."
  (with-slots (display-opts) this
    (plist-get display-opts
               (if charging :charging-indicator
                 :discharging-indicator))))

(cl-defmethod symon-monitor-display ((this symon-battery))
  "Return the display text for the battery monitor."
  (when-let ((status (symon-monitor-value this)))
    (let ((charging (string= (downcase (cdr (assoc ?L status))) "ac"))
          (percent (read (cdr (assoc ?p status))))
          (time-left (cdr (assoc ?t status))))
      (thread-first
          (format "%d%%%s"
                  percent
                  (if (string= time-left "N/A")
                      "" (concat (symon-battery--indicator this charging) time-left)))
        (propertize 'face (symon-battery--face charging percent time-left))))))

(provide 'symon-battery)
;;; symon-battery.el ends here
