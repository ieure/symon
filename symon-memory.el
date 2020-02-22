;;; symon-memory.el --- Memory monitor for Symon     -*- lexical-binding: t; -*-

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

 ;; Linux

;;;###autoload
(defclass symon-memory-linux (symon-monitor-history)
  ((default-display-opts
     :initform '(:index "MEM:" :unit "%"))))

(cl-defmethod symon-monitor-fetch ((this symon-memory-linux))
  (cl-destructuring-bind (memtotal memavailable memfree buffers cached)
      (symon-monitor--linux-read-lines
       "/proc/meminfo" (lambda (str) (and str (read str)))
       '("MemTotal:" "MemAvailable:" "MemFree:" "Buffers:" "Cached:"))
    (if memavailable
        (/ (* (- memtotal memavailable) 100) memtotal)
      (/ (* (- memtotal (+ memfree buffers cached)) 100) memtotal))))

(provide 'symon-memory)
;;; symon-memory.el ends here
