;;; symon-fan.el --- Symon fan monitor              -*- lexical-binding: t; -*-

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

(defgroup symon-fan nil
  "Display fan speed in Symon."
  :group 'symon)

(defface symon-fan-face
  '((t :inherit default))
  "Face for Symon fan display."
  :group 'symon-fan)

;;;###autoload
(defun symon-fan-fans ()
  "Return all known fans."
  (file-expand-wildcards "/sys/class/hwmon/hwmon*/fan*_input"))

(defun symon-fan--name (fan-device)
  "Return the name of FAN-DEVICE."
  (symon-monitor--slurp (concat (file-name-directory fan-device) "/name")))

;;;###autoload
(defclass symon-fan (symon-monitor-history)
  ((fan :type string
        :initarg :fan
        :documentation "The fan to monitor")

   (default-display-opts
     :type list
     :initform '(:unit "rpm"
                       :hide t))))

(cl-defmethod symon-monitor-fetch ((this symon-fan))
  (with-slots (fan) this
    (read (symon-monitor--slurp fan))))

(cl-defmethod symon-monitor-display ((this symon-fan))
  (let ((rpm (symon-monitor-value this))
        (hide (plist-get (oref this display-opts) :hide)))
    (unless (and hide rpm (= rpm 0))
      (propertize (cl-call-next-method) 'face 'symon-fan-face))))

(provide 'symon-fan)
;;; symon-fan.el ends here
