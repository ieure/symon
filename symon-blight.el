;;; symon-blight.el --- Backlight monitor            -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Ian Eure

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

;; Monitor to display backlight brightness.

;;; Code:

(require 'symon-monitor)
(require 'blight)

(defgroup symon-blight nil
  "Display screen brightness in Symon."
  :group 'symon)

(defface symon-blight-face
  '((t :foreground "black"
       :background "gold3"))
  "Face for Symon screen brightness display."
  :group 'symon-blight)

;;;###autoload
(defclass symon-blight (symon-monitor)
  ((blight :initarg :blight
           :documentation "Blight instance to monitor.")
   (display-for :initarg :display-for :initform 15
                :documentation "Display the monitor for this many seconds after the brightness changes.  When set to `t', shows unconditionally.")))

(cl-defmethod symon-monitor-fetch ((this symon-blight)) nil)

(cl-defmethod symon-monitor-display ((this symon-blight))
  (with-slots (blight display-for) this
    (with-slots (last-changed) blight
      (when (or (eq t display-for)
                (<= (- (float-time) last-changed) display-for))
        (let ((b (blight-get blight)))
          (thread-first
              (format "%s %d%%" (if (>= b 50) "🔆" "🔅") b)
            (propertize 'face 'symon-blight-face)))))))

(provide 'symon-blight)
;;; symon-blight.el ends here
