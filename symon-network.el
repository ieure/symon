;;; symon-network.el --- Network monitor for Symon   -*- lexical-binding: t; -*-

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

;; FIXME this is broken
(defclass symon-linux-network-rx (symon-monitor-history)
  ((default-display-opts
     :type list
     :initform '(:index "RX:"
                        :unit "KB/s"
                        :upper-bound 300
                        :lower-bound 0))))

(cl-defmethod symon-monitor-fetch ((this symon-linux-network-rx))
  (let ((last-value (symon-monitor-value this)))
    (with-temp-buffer
      (insert-file-contents "/proc/net/dev")
      (goto-char 1)
      (let ((rx 0))
        (while (search-forward-regexp "^[\s\t]*\\(.*\\):" nil t)
          (unless (string= (match-string 1) "lo")
            (setq rx (+ rx (read (current-buffer))))))
        (prog1 (when last-value
                 (/ (- rx last-value) symon-refresh-rate 1000))
          (setf last-value rx))))))

;; FIXME this is broken
(defclass symon-linux-network-tx (symon-monitor-history)
  ((default-display-opts
     :type list
     :initform '(:index "RX:"
                        :unit "KB/s"
                        :upper-bound 100
                        :lower-bound 0))))

(cl-defmethod symon-monitor-fetch ((this symon-linux-network-tx))
  (let ((last-value (symon-monitor-value this)))
    (with-temp-buffer
      (insert-file-contents "/proc/net/dev")
      (goto-char 1)
      (let ((tx 0))
        (while (search-forward-regexp "^[\s\t]*\\(.*\\):" nil t)
          (unless (string= (match-string 1) "lo")
            (forward-word 8)
            (setq tx (+ tx (read (current-buffer))))))
        (prog1 (when last-value
                 (/ (- tx last-value) symon-refresh-rate 1000))
          (setf last-value tx))))))

(provide 'symon-network)
;;; symon-network.el ends here
