;;; symon-swap.el --- Swap monitor for Symon         -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2021  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; Keywords: data

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

(defclass symon-swap-linux (symon-monitor)
  ((default-display-opts :initform '(:index "SWAP:" :style 'megabytes :sparkline nil)))
  :documentation "Monitor for Linux system swap.

The following display-opts are supported:

:style - May be 'MEGABYTES (mb swapped) or 'PERCENT (% swap used).
:hide  - When non-NIL, only show monitor if swap is being used.")

(cl-defmethod symon-monitor-fetch ((this symon-swap-linux))
  (cl-destructuring-bind (swaptotal swapfree)
      (symon-linux--read-lines
       "/proc/meminfo" 'read '("SwapTotal:" "SwapFree:"))
    (let ((swapped-bytes (- swaptotal swapfree)))
      (pcase (plist-get (oref this display-opts) :style)
        ('megabytes (/ swapped-bytes 1000))
        (_ (round (* 100 (/ swapped-bytes (float swaptotal)))))))))

(cl-defmethod symon-monitor-display ((this symon-swap-linux))
  (when (>= (symon-monitor-value this) 0)
    (cl-call-next-method)))

(cl-defmethod symon-monitor-setup ((this symon-swap-linux))
  (plist-put (oref this display-opts) :unit
             (if (equal 'megabytes (plist-get (oref this display-opts) :style))
                 "mb" "%"))
  (cl-call-next-method))


 ;; Windows

(defclass symon-swap-windows (symon-monitor-windows symon-monitor-history)
  ((default-display-opts
     :initform `(:index "PF:"
                        :unit "MB"
                        :upper-bound ,symon-windows-page-file-upper-bound))))

(cl-defmethod symon-monitor-fetch ((this symon-swap-windows))
  (symon--read-value-from-process-buffer "swap"))

(provide 'symon-swap)
;;; symon-swap.el ends here
