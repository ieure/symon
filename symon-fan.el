;;; symon-fan.el --- Symon fan monitor              -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'symon)

(defun symon-fan--fans ()
  "Return all known fans."
  (file-expand-wildcards "/sys/class/hwmon/hwmon*/fan*_input"))

(defun symon-fan--name (fan-device)
  "Return the name of FAN-DEVICE."
  (symon--slurp-line (concat (file-name-directory fan-device) "/name")))

;;;###autoload
(defclass symon-fan (symon-monitor-history)
  ((fan :type string
        :documentation "The fan to monitor")
   (default-display-opts '(:unit "rpm"
                                 :hide t
                                 :sparkline t))))

(cl-defmethod symon-monitor-fetch ((this symon-fan))
  (read (symon--slurp-line ,fan-device)))

(cl-defmethod symon-monitor-display ((this symon-fan))
  (let ((rpm (symon-monitor-value this))
        (hide (plist-get (oref this display-opts) :hide))))
  (unless (and hide (= rpm 0))
    (cl-call-next-method)))

(provide 'symon-fan)
;;; symon-fan.el ends here