;;; symon-windows.el --- Windows support for Symon   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; Keywords: emulations

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

;;   + windows monitors

(defun symon-windows--maybe-start-wmi-process ()
  (symon--maybe-start-process (format "powershell -command          \
$last = 0;                                                          \
while(1)                                                            \
{                                                                   \
    echo ----;                                                      \
                                                                    \
    $t = (gwmi Win32_ComputerSystem).TotalPhysicalMemory / 1000;    \
    $f = (gwmi Win32_OperatingSystem).FreePhysicalMemory;           \
    echo mem:$(($t - $f) * 100 / $t);                               \
                                                                    \
    echo swap:$((gwmi Win32_PageFileUsage).CurrentUsage);           \
                                                                    \
    echo bat:$((gwmi Win32_Battery).EstimatedChargeRemaining);      \
                                                                    \
    $r = 0;                                                         \
    $t = 0;                                                         \
    $w = gwmi Win32_PerfRawData_Tcpip_NetworkInterface;             \
    foreach($x in $w){                                              \
        $r = $r + $x.BytesReceivedPersec;                           \
        $t = $t + $x.BytesSentPersec                                \
    }                                                               \
    echo rx:$($r / 1000);                                           \
    echo tx:$($t / 1000);                                           \
                                                                    \
    $p = (gwmi Win32_PerfRawData_Counters_ProcessorInformation)[0]; \
    if($last)                                                       \
    {                                                               \
        $dt = $p.Timestamp_Sys100NS - $last.Timestamp_Sys100NS;     \
        $dp = $p.PercentProcessorTime - $last.PercentProcessorTime; \
        echo cpu:$((1 - ($dp / $dt)) * 100);                        \
    }                                                               \
    $last = $p;                                                     \
                                                                    \
    sleep %d                                                        \
}" symon-refresh-rate)))

(defclass symon-monitor-windows () nil)

(cl-defmethod symon-monitor-setup ((this symon-monitor-windows))
  (symon-windows--maybe-start-process))

(cl-defmethod symon-monitor-cleanup ((this symon-monitor-windows))
  (symon-windows--maybe-kill-process))

;; (defclass symon-windows-battery (symon-monitor-windows symon-monitor-history)
;;   ((default-display-opts :initform '(:index "BAT:" :unit "%"))))

;; (cl-defmethod symon-windows-battery ((this symon-windows-battery))
;;   (symon--read-value-from-process-buffer "bat"))

;; (defclass symon-windows-network-rx (symon-monitor-windows symon-linux-network-rx)
;;   ((default-display-opts
;;      :initform `(:index "RX:" :unit "KB/s"
;;                         :upper-bound ,symon-network-rx-upper-bound
;;                         :lower-bound ,symon-network-rx-lower-bound)))

;;   :fetch )


;; (cl-defmethod symon-monitor-fetch ((this symon-windows-network-rx))
;;   (with
;;    (let ((rx (symon--read-value-from-process-buffer "rx")))
;;      (prog1 (when symon-windows--last-network-rx
;;               (/ (- rx symon-windows--last-network-rx) symon-refresh-rate))
;;        (setq symon-windows--last-network-rx rx)))))

;; (define-symon-monitor symon-windows-network-rx-monitor
;;   :index "RX:" :unit "KB/s" :sparkline t
;;   :upper-bound symon-network-rx-upper-bound
;;   :lower-bound symon-network-rx-lower-bound
;;   :setup (progn
;;            (symon-windows--maybe-start-wmi-process)
;;            (setq symon-windows--last-network-rx nil))
;;   :cleanup (symon--maybe-kill-process)
;;   :fetch (let ((rx (symon--read-value-from-process-buffer "rx")))
;;            (prog1 (when symon-windows--last-network-rx
;;                     (/ (- rx symon-windows--last-network-rx) symon-refresh-rate))
;;              (setq symon-windows--last-network-rx rx))))

;; (defvar symon-windows--last-network-tx nil)

;; (define-symon-monitor symon-windows-network-tx-monitor
;;   :index "TX:" :unit "KB/s" :sparkline t
;;   :upper-bound symon-network-tx-upper-bound
;;   :lower-bound symon-network-tx-lower-bound
;;   :setup (progn
;;            (symon-windows--maybe-start-wmi-process)
;;            (setq symon-windows--last-network-tx nil))
;;   :cleanup (symon--maybe-kill-process)
;;   :fetch (let ((tx (symon--read-value-from-process-buffer "tx")))
;;            (prog1 (when symon-windows--last-network-tx
;;                     (/ (- tx symon-windows--last-network-tx) symon-refresh-rate))
;;              (setq symon-windows--last-network-tx tx))))

(provide 'symon-windows)
;;; symon-windows.el ends here
