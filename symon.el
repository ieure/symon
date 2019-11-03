;;; symon.el --- tiny graphical system monitor     -*- lexical-binding: t; -*-

;; Copyright (C) 2015 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 1.2.0

;;; Commentary:

;; Load this script
;;
;;   (require 'symon)
;;
;; and turn on `symon-mode'.
;;
;;   (symon-mode)
;;
;; then a tiny system monitor is displayed in minibuffer, during idle.

;;; Change Log:

;; 1.0.0 first release
;; 1.1.0 add option symon-sparkline-thickness
;; 1.1.1 add symon-windows-page-file-monitor
;; 1.1.2 add darwin support (mac os x)
;; 1.2.0 add paging feature

;;; Code:

(require 'battery)
(require 'ring)

(defconst symon-version "1.2.0")

(defgroup symon nil
  "tiny graphical system monitor"
  :group 'emacs)

;; + customs

;; core

(defcustom symon-refresh-rate 4
  "refresh rate of symon display. *set this option BEFORE
  enabling `symon-mode'.*"
  :group 'symon)

(defcustom symon-delay 2
  "delay in seconds until symon is displayed. *set this option
BEFORE enabling `symon-mode'.*"
  :group 'symon)

(defcustom symon-monitors
  (list (cond ((memq system-type '(gnu/linux cygwin))
         '(symon-linux-memory
           symon-linux-cpu
           symon-linux-network-rx
           symon-linux-network-tx))
        ((memq system-type '(darwin))
         '(symon-darwin-memory
           symon-darwin-cpu
           symon-darwin-network-rx
           symon-darwin-network-tx))
        ((memq system-type '(windows-nt))
         '(symon-windows-memory
           symon-windows-cpu
           symon-windows-network-rx
           symon-windows-network-tx))))

  "List of list of monitors.

Each outer list is a page.  Symon rotates through pages as it redisplays.

Each inner list is a list of monitors.  Members of that list
are anything which `symon--instantiate' knows how to produce a
monitor from:

- A symbol which is bound to a monitor class.  The class will be
  instantiated with no arguments.
- A symbol which is bound to a monitor object.
- A monitor object itself (not recommended).
- An expression which evaluates to one of the above."

  :group 'symon
  :risky t
  :type '(repeat
          (repeat :tag "Page of monitors"
                  (choice
                   (symbol :tag "Class or object")
                   (sexp)))))

;; sparkline

(defcustom symon-sparkline-height 11
  "height of sparklines."
  :group 'symon)

(defcustom symon-sparkline-width 80
  "width of sparklines."
  :group 'symon)

(defcustom symon-sparkline-ascent 100
  "`:ascent' property for sparklines."
  :group 'symon)

(defcustom symon-sparkline-thickness 2
  "line width of sparklines."
  :group 'symon)

(defcustom symon-sparkline-type 'gridded
  "type of sparklines."
  :group 'symon)

;; some darwin builds cannot render xbm images (foreground color is
;; always black), so convert to xpm before rendering.
(defcustom symon-sparkline-use-xpm (eq system-type 'darwin)
  "when non-nil, convert sparklines to xpm from xbm before
rendering."
  :group 'symon)

;; network monitor

(defcustom symon-network-rx-upper-bound 300
  "upper-bound of sparkline for network RX status."
  :group 'symon)

(defcustom symon-network-tx-upper-bound 100
  "upper-bound of sparkline for network TX status."
  :group 'symon)

(defcustom symon-network-rx-lower-bound 0
  "lower-bound of sparkline for network RX status."
  :group 'symon)

(defcustom symon-network-tx-lower-bound 0
  "lower-bound of sparkline for network TX status."
  :group 'symon)

;; page-file monitor

(defcustom symon-windows-page-file-upper-bound 2000
  "upper-bound of sparkline for page file usage."
  :group 'symon)

;; + utilities
;;   + general

(defun symon--flatten (lst)
  "flatten LST"
  (if (consp lst)
      (apply 'nconc (mapcar 'symon--flatten lst))
    (list lst)))

;;   + sparkline generator

;; sparkline-types are internally a symbol with property
;; 'symon-sparkline-type associated to a function that generates a
;; 2d-bool-vector.

(defvar symon--sparkline-base-cache
  [nil symon-sparkline-width symon-sparkline-height nil])
(defun symon--get-sparkline-base ()
  (unless (and (eq (aref symon--sparkline-base-cache 0) symon-sparkline-type)
               (= (aref symon--sparkline-base-cache 1) symon-sparkline-width)
               (= (aref symon--sparkline-base-cache 2) symon-sparkline-height))
    (aset symon--sparkline-base-cache 0 symon-sparkline-type)
    (aset symon--sparkline-base-cache 1 symon-sparkline-width)
    (aset symon--sparkline-base-cache 2 symon-sparkline-height)
    (aset symon--sparkline-base-cache 3
          (funcall (get symon-sparkline-type 'symon-sparkline-type))))
  (copy-sequence (aref symon--sparkline-base-cache 3)))

(defun symon--make-sparkline (list &optional minimum maximum)
  "make sparkline image from LIST."
  (let ((num-samples (length list)))
    (unless (zerop num-samples)
      (let* ((image-data (symon--get-sparkline-base))
             (maximum (if maximum (float maximum) 100.0))
             (minimum (if minimum (float minimum) 0.0))
             (topmargin (1- symon-sparkline-thickness))
             (height (- symon-sparkline-height topmargin))
             (height-per-point (/ height (1+ (- maximum minimum))))
             (width-per-sample (/ symon-sparkline-width (float num-samples)))
             (samples (apply 'vector list))
             sample y ix)
        (dotimes (x symon-sparkline-width)
          (setq sample (aref samples (floor (/ x width-per-sample))))
          (when (numberp sample)
            (setq y (floor (* (- sample minimum) height-per-point)))
            (when (and (<= 0 y) (< y height))
              (dotimes (dy symon-sparkline-thickness)
                (aset image-data
                      (+ (* (- symon-sparkline-height (+ y dy) 1) symon-sparkline-width) x)
                      t)))))
        `(image :type xbm :data ,image-data :ascent ,symon-sparkline-ascent
                :height ,symon-sparkline-height :width ,symon-sparkline-width)))))

(defun symon--convert-sparkline-to-xpm (sparkline)
  "convert sparkline to an xpm image."
  (let ((data (plist-get (cdr sparkline) :data)))
    (with-temp-buffer
      (insert (format "/* XPM */
static char * sparkline_xpm[] = { \"%d %d 2 1\", \"@ c %s\", \". c none\""
                      symon-sparkline-width symon-sparkline-height
                      (face-foreground 'default)))
      (let ((ix 0))
        (dotimes (x symon-sparkline-height)
          (insert ",\n\"")
          (dotimes (y symon-sparkline-width)
            (insert (if (aref data ix) ?@ ?.))
            (setq ix (1+ ix)))
          (insert "\"")))
      (insert "};")
      `(image :type xpm :data ,(buffer-string) :ascent ,symon-sparkline-ascent
              :height ,symon-sparkline-height :width ,symon-sparkline-width))))

;;   + symon monitor classes & helpers

(defun symon--make-history-ring (size)
  "like `(make-ring size)' but filled with `nil'."
  (cons 0 (cons size (make-vector size nil))))

(defclass symon-monitor ()
  ((interval :type integer
             :initform 4
             :initarg :interval
             :documentation "Fetch interval in seconds.")
   (display-opts :type list
                 :initform nil
                 :initarg :display-opts
                 :documentation "User-specified display options for this monitor.")
   (default-display-opts :type list
     :initform nil
     :type list
     :documentation "Default display options for this monitor.")

   ;; Internal slots

   (timer
    :documentation "Fires `symon-monitor-fetch' for this monitor.")
   (value
    :accessor symon-monitor-value
    :documentation "Most recent value"))

  :abstract t
  :documentation "Base (default) Symon monitor class.")

(cl-defmethod symon-monitor-update ((this symon-monitor))
  "Update THIS, storing the latest value."
  (oset this value (symon-monitor-fetch this)))

(cl-defmethod symon-monitor-setup ((this symon-monitor))
  "Setup this monitor.

This method is called when activating `symon-mode'."

  ;; Merge display opts
  (let ((opts (copy-list (oref this default-display-opts)))
        (user-opts (copy-list (oref this display-opts))))
    (while user-opts
      (plist-put opts (pop user-opts) (pop user-opts)))
    (oset this display-opts opts))

  (oset this timer
        (run-with-timer 0 (oref this interval)
                        (apply-partially #'symon-monitor-update this))))

(cl-defmethod symon-monitor-cleanup ((this symon-monitor))
  "Cleanup the monitor.

   This method is called when deactivating `symon-mode'."
  (cancel-timer (oref this timer))
  (oset this timer nil))

(cl-defmethod symon-monitor-fetch ((this symon-monitor))
  "Fetch the current monitor value.")

(cl-defmethod symon-monitor-display ((this symon-monitor))
  "Default display method for Symon monitors."
  (let* ((val (car (ring-elements (oref this history))))
         (plist (oref this display-opts))
         (index (plist-get plist :index))
         (unit (plist-get plist :unit)))
    (concat index
            (if (not (numberp val)) "N/A"
              (format "%d%s" val unit)))))

(defclass symon-monitor-history (symon-monitor)
  ((history-size :type integer :custom integer
                 :initform 50
                 :initarg :history-size)
   (history
    :accessor symon-monitor-history
    :documentation "Ring of historical monitor values"))

  :abstract t
  :documentation "Monitor class which stores a history of values.")

(cl-defmethod symon-monitor-setup ((this symon-monitor-history))
  (oset this history (symon--make-history-ring (oref this history-size)))
  (cl-call-next-method))

(cl-defmethod symon-monitor-history ((this symon-monitor-history))
  (oref this history))

(cl-defmethod symon-monitor-value ((this symon-monitor-history))
  (car (symon-monitor-history this)))

(cl-defmethod symon-monitor-update :before ((this symon-monitor-history))
  (ring-insert (oref this history) (symon-monitor-fetch this)))

(cl-defmethod symon-monitor-display ((this symon-monitor-history))
  "Default display method for Symon monitors."
  (let* ((lst (ring-elements (oref this history)))
         (plist (oref this display-opts))
         (sparkline (plist-get plist :sparkline))
         (upper-bound (plist-get plist :upper-bound))
         (lower-bound (plist-get plist :lower-bound)))

    (concat (cl-call-next-method)
            (when (and sparkline (window-system))
              (let ((sparkline (symon--make-sparkline
                                lst lower-bound upper-bound)))
                (when symon-sparkline-use-xpm
                  (setq sparkline
                        (symon--convert-sparkline-to-xpm sparkline)))
                (propertize " " 'display sparkline))))))

;;   + process management

(defvar symon--process-buffer-name " *symon-process*")
(defvar symon--process-reference-count 0)

(defun symon--read-value-from-process-buffer (index)
  "Read a value from a specific buffer"
  (when (get-buffer symon--process-buffer-name)
    (with-current-buffer symon--process-buffer-name
      (when (save-excursion
              (search-backward-regexp (concat index ":\\([0-9]+\\)\\>") nil t))
        (read (match-string 1))))))

(defun symon--maybe-start-process (cmd)
  (setq symon--process-reference-count
        (1+ symon--process-reference-count))
  (unless (get-buffer symon--process-buffer-name)
    (let ((proc (start-process-shell-command
                 "symon-process" symon--process-buffer-name cmd))
          (filter (lambda (proc str)
                    (when (get-buffer symon--process-buffer-name)
                      (with-current-buffer symon--process-buffer-name
                        (when (and (string-match "-" str) (search-backward "----" nil t))
                          (delete-region 1 (point)))
                        (goto-char (1+ (buffer-size)))
                        (insert str))))))
      (set-process-query-on-exit-flag proc nil)
      (set-process-filter proc filter))))

(defun symon--maybe-kill-process ()
  (setq symon--process-reference-count
        (1- symon--process-reference-count))
  (when (and (zerop symon--process-reference-count)
             (get-buffer symon--process-buffer-name))
    (kill-buffer symon--process-buffer-name)))

;; + predefined monitors
;;   + linux monitors

(defun symon-linux--read-lines (file reader indices)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char 1)
    (mapcar (lambda (index)
              (save-excursion
                (when (search-forward-regexp (concat "^" index "\\(.*\\)$") nil t)
                  (if reader
                      (funcall reader (match-string 1))
                    (match-string 1)))))
            indices)))

(defclass symon-linux-cpu (symon-monitor-history)
  ((last-total-ticks :type integer :initform 0)
   (last-idle-ticks :type integer :initform 0)
   (default-display-opts :type list
     :initform '(:index "CPU:" :unit "%" :sparkline t))))

(cl-defmethod symon-monitor-fetch ((this symon-linux-cpu))
  (cl-destructuring-bind (cpu)
      (symon-linux--read-lines
       "/proc/stat" (lambda (str) (mapcar 'read (split-string str nil t))) '("cpu"))
    (with-slots (last-total-ticks last-idle-ticks) this
      (let ((total (apply '+ cpu)) (idle (nth 3 cpu)))
        (prog1 (let ((total-diff (- total last-total-ticks))
                     (idle-diff (- idle last-idle-ticks)))
                 (unless (zerop total-diff)
                   (/ (* (- total-diff idle-diff) 100) total-diff)))
          (setf last-total-ticks total
                last-idle-ticks idle))))))

(define-symon-monitor symon-linux-memory-monitor
  :index "MEM:" :unit "%" :sparkline t
  :fetch (cl-destructuring-bind (memtotal memavailable memfree buffers cached)
             (symon-linux--read-lines
              "/proc/meminfo" (lambda (str) (and str (read str)))
              '("MemTotal:" "MemAvailable:" "MemFree:" "Buffers:" "Cached:"))
           (if memavailable
               (/ (* (- memtotal memavailable) 100) memtotal)
             (/ (* (- memtotal (+ memfree buffers cached)) 100) memtotal)))
  :annotation (cl-destructuring-bind (swaptotal swapfree)
                  (symon-linux--read-lines
                   "/proc/meminfo" 'read '("SwapTotal:" "SwapFree:"))
                (let ((swapped (/ (- swaptotal swapfree) 1000)))
                  (unless (zerop swapped) (format "%dMB Swapped" swapped)))))

(define-symon-monitor symon-linux-battery-monitor
  :index "BAT:" :unit "%" :sparkline t
  :fetch (when battery-status-function
           (read (cdr (assoc ?p (funcall battery-status-function))))))

(defvar symon-linux--last-network-rx nil)

(define-symon-monitor symon-linux-network-rx-monitor
  :index "RX:" :unit "KB/s" :sparkline t
  :upper-bound symon-network-rx-upper-bound
  :lower-bound symon-network-rx-lower-bound
  :setup (setq symon-linux--last-network-rx nil)
  :fetch (with-temp-buffer
           (insert-file-contents "/proc/net/dev")
           (goto-char 1)
           (let ((rx 0))
             (while (search-forward-regexp "^[\s\t]*\\(.*\\):" nil t)
               (unless (string= (match-string 1) "lo")
                 (setq rx (+ rx (read (current-buffer))))))
             (prog1 (when symon-linux--last-network-rx
                      (/ (- rx symon-linux--last-network-rx) symon-refresh-rate 1000))
               (setq symon-linux--last-network-rx rx)))))

(defvar symon-linux--last-network-tx nil)

(define-symon-monitor symon-linux-network-tx-monitor
  :index "TX:" :unit "KB/s" :sparkline t
  :upper-bound symon-network-tx-upper-bound
  :lower-bound symon-network-tx-lower-bound
  :setup (setq symon-linux--last-network-tx nil)
  :fetch (with-temp-buffer
           (insert-file-contents "/proc/net/dev")
           (goto-char 1)
           (let ((tx 0))
             (while (search-forward-regexp "^[\s\t]*\\(.*\\):" nil t)
               (unless (string= (match-string 1) "lo")
                 (forward-word 8)
                 (setq tx (+ tx (read (current-buffer))))))
             (prog1 (when symon-linux--last-network-tx
                      (/ (- tx symon-linux--last-network-tx) symon-refresh-rate 1000))
               (setq symon-linux--last-network-tx tx)))))

;;   + darwin monitors

(defun symon-darwin--maybe-start-process ()
  (symon--maybe-start-process (format "
while true; do
    echo \"----\"

    interface=`route get 0.0.0.0 | grep interface | awk '{print $2}'`
    s=`netstat -bi -I $interface | tail -1`;
    echo $s | awk '{print \"rx:\"$7}'
    echo $s | awk '{print \"tx:\"$8}'

    s=`hostinfo  | grep 'Load average' | awk '{print \"cpu:\"$3}' | sed 's/,//'`
    echo $s

    m1=`sysctl hw.memsize | sed 's/.*:\s*//'`
    m_active=`vm_stat | grep 'Pages active' | sed 's/.*: *//'`
    m_wired=`vm_stat | grep 'Pages wired' | sed 's/.*: *//'`

    s=`echo \"scale=2; (($m_active+$m_wired)*4096*100 / $m1)\"| bc -l`
    echo \"mem:$s\"

    sleep %d
done" symon-refresh-rate)))

(define-symon-monitor symon-darwin-cpu-monitor
  :index "CPU:" :unit "%" :sparkline t
  :setup (symon-darwin--maybe-start-process)
  :cleanup (symon--maybe-kill-process)
  :fetch (symon--read-value-from-process-buffer "cpu"))

(define-symon-monitor symon-darwin-memory-monitor
  :index "MEM:" :unit "%" :sparkline t
  :setup (symon-darwin--maybe-start-process)
  :cleanup (symon--maybe-kill-process)
  :fetch (symon--read-value-from-process-buffer "mem"))

(defvar symon-darwin--last-network-rx nil)

(define-symon-monitor symon-darwin-network-rx-monitor
  :index "RX:" :unit "KB/s" :sparkline t
  :upper-bound symon-network-rx-upper-bound
  :lower-bound symon-network-rx-lower-bound
  :setup (progn
           (symon-darwin--maybe-start-process)
           (setq symon-darwin--last-network-rx nil))
  :cleanup (symon--maybe-kill-process)
  :fetch (let ((rx (symon--read-value-from-process-buffer "rx")))
           (prog1 (when symon-darwin--last-network-rx
                    (/ (- rx symon-darwin--last-network-rx) symon-refresh-rate 1000))
             (setq symon-darwin--last-network-rx rx))))

(defvar symon-darwin--last-network-tx nil)

(define-symon-monitor symon-darwin-network-tx-monitor
  :index "TX:" :unit "KB/s" :sparkline t
  :upper-bound symon-network-tx-upper-bound
  :lower-bound symon-network-tx-lower-bound
  :setup (progn
           (symon-darwin--maybe-start-process)
           (setq symon-darwin--last-network-tx nil))
  :cleanup (symon--maybe-kill-process)
  :fetch (let ((tx (symon--read-value-from-process-buffer "tx")))
           (prog1 (when symon-darwin--last-network-tx
                    (/ (- tx symon-darwin--last-network-tx) symon-refresh-rate 1000))
             (setq symon-darwin--last-network-tx tx))))

(define-symon-monitor symon-darwin-battery-monitor
  :index "BAT:" :unit "%" :sparkline t
  :fetch (when battery-status-function
           (read (cdr (assoc ?p (funcall battery-status-function))))))

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

(define-symon-monitor symon-windows-cpu-monitor
  :index "CPU:" :unit "%" :sparkline t
  :setup (symon-windows--maybe-start-wmi-process)
  :cleanup (symon--maybe-kill-process)
  :fetch (symon--read-value-from-process-buffer "cpu"))

(define-symon-monitor symon-windows-memory-monitor
  :index "MEM:" :unit "%" :sparkline t
  :setup (symon-windows--maybe-start-wmi-process)
  :cleanup (symon--maybe-kill-process)
  :fetch (symon--read-value-from-process-buffer "mem"))

(define-symon-monitor symon-windows-page-file-monitor
  :index "PF:" :unit "MB" :sparkline t
  :upper-bound symon-windows-page-file-upper-bound
  :setup (symon-windows--maybe-start-wmi-process)
  :cleanup (symon--maybe-kill-process)
  :fetch (symon--read-value-from-process-buffer "swap"))

(define-symon-monitor symon-windows-battery-monitor
  :index "BAT:" :unit "%" :sparkline t
  :setup (symon-windows--maybe-start-wmi-process)
  :cleanup (symon--maybe-kill-process)
  :fetch (symon--read-value-from-process-buffer "bat"))

(defvar symon-windows--last-network-rx nil)

(define-symon-monitor symon-windows-network-rx-monitor
  :index "RX:" :unit "KB/s" :sparkline t
  :upper-bound symon-network-rx-upper-bound
  :lower-bound symon-network-rx-lower-bound
  :setup (progn
           (symon-windows--maybe-start-wmi-process)
           (setq symon-windows--last-network-rx nil))
  :cleanup (symon--maybe-kill-process)
  :fetch (let ((rx (symon--read-value-from-process-buffer "rx")))
           (prog1 (when symon-windows--last-network-rx
                    (/ (- rx symon-windows--last-network-rx) symon-refresh-rate))
             (setq symon-windows--last-network-rx rx))))

(defvar symon-windows--last-network-tx nil)

(define-symon-monitor symon-windows-network-tx-monitor
  :index "TX:" :unit "KB/s" :sparkline t
  :upper-bound symon-network-tx-upper-bound
  :lower-bound symon-network-tx-lower-bound
  :setup (progn
           (symon-windows--maybe-start-wmi-process)
           (setq symon-windows--last-network-tx nil))
  :cleanup (symon--maybe-kill-process)
  :fetch (let ((tx (symon--read-value-from-process-buffer "tx")))
           (prog1 (when symon-windows--last-network-tx
                    (/ (- tx symon-windows--last-network-tx) symon-refresh-rate))
             (setq symon-windows--last-network-tx tx))))

;;   + misc monitors

(define-symon-monitor symon-current-time-monitor
  :display (format-time-string "%H:%M"))

;; + predefined sparkline types

(defun symon--sparkline-draw-horizontal-grid (vec y)
  (dotimes (x/2 (/ symon-sparkline-width 2))
    (aset vec (+ (* y symon-sparkline-width) (* x/2 2)) t)))

(defun symon--sparkline-draw-vertical-grid (vec x)
  (dotimes (y/2 (/ symon-sparkline-height 2))
    (aset vec (+ (* (* y/2 2) symon-sparkline-width) x) t)))

(defun symon--make-plain-sparkline ()
  (make-bool-vector (* symon-sparkline-height symon-sparkline-width) nil))

(defun symon--make-bounded-sparkline ()
  (let ((vec (symon--make-plain-sparkline)))
    (symon--sparkline-draw-horizontal-grid vec 0)
    (symon--sparkline-draw-horizontal-grid vec (1- symon-sparkline-height))
    vec))

(defun symon--make-boxed-sparkline ()
  (let ((vec (symon--make-bounded-sparkline)))
    (symon--sparkline-draw-vertical-grid vec 0)
    (symon--sparkline-draw-vertical-grid vec (1- symon-sparkline-width))
    vec))

(defun symon--make-gridded-sparkline ()
  (let ((vec (symon--make-boxed-sparkline)))
    (symon--sparkline-draw-horizontal-grid vec (/ symon-sparkline-height 2))
    (symon--sparkline-draw-vertical-grid   vec (/ symon-sparkline-width 4))
    (symon--sparkline-draw-vertical-grid   vec (/ symon-sparkline-width 2))
    (symon--sparkline-draw-vertical-grid   vec (/ (* symon-sparkline-width 3) 4))
    vec))

(put 'plain 'symon-sparkline-type 'symon--make-plain-sparkline)
(put 'bounded 'symon-sparkline-type 'symon--make-bounded-sparkline)
(put 'boxed 'symon-sparkline-type 'symon--make-boxed-sparkline)
(put 'gridded 'symon-sparkline-type 'symon--make-gridded-sparkline)

;; + symon core

(defvar symon--active-monitors nil)
(defvar symon--display-active nil)
(defvar symon--active-page    nil)
(defvar symon--total-page-num nil)
(defvar symon--timer-objects  nil)

(defun symon--instantiate (monitor-or-symbol)
  "Create an instance."
  (cond
   ;; Instance of symon-monitor class.
   ((and (object-p monitor-or-symbol) (object-of-class-p monitor-or-symbol symon-monitor))
    monitor-or-symbol)

   ;; Symbol bound to a symon-monitor class.
   ((and (symbolp monitor-or-symbol)
         (class-p monitor-or-symbol)
         (child-of-class-p monitor-or-symbol symon-monitor))
    (make-instance monitor-or-symbol))

   ;; Expression which can evaluate to one of the above.
   ((consp monitor-or-symbol) (symon--instantiate (eval monitor-or-symbol)))))

(defun symon--setup (monitors)
  (thread-first
      (lambda (page)
        (remove-if-not #'identity (mapcar #'symon--instantiate page)))
    (mapcar symon-monitors)))

(defun symon--initialize ()
  (unless symon-monitors
    (warn "`symon-monitors' is empty."))
  (let ((monitors (symon--setup symon-monitors)))
    (mapc #'symon-monitor-setup (symon--flatten monitors))
    (setq symon--active-monitors monitors
          symon--display-active nil
          symon--total-page-num (length symon-monitors)
          symon--timer-objects
          (list (run-with-timer 0 symon-refresh-rate 'symon--redisplay)
                (run-with-idle-timer symon-delay t 'symon-display)))
    (add-hook 'pre-command-hook 'symon--display-end)
    (add-hook 'kill-emacs-hook 'symon--cleanup)))

(defun symon--cleanup ()
  (remove-hook 'kill-emacs-hook 'symon--cleanup)
  (remove-hook 'pre-command-hook 'symon--display-end)
  (mapc #'cancel-timer symon--timer-objects)
  (mapc #'symon-monitor-cleanup (symon--flatten symon--active-monitors)))

(defun symon--display-update ()
  "update symon display"
  (unless (or cursor-in-echo-area (active-minibuffer-window))
    (let ((message-log-max nil)  ; do not insert to *Messages* buffer
          (display-string nil))
      (message "%s" (substring
          (cl-loop for monitor in (elt symon--active-monitors symon--active-page)
                   for output = (symon-monitor-display monitor)
                   unless (or (null output) (string= "" output))
                   concat " "
                   concat output)
          1)))
    (setq symon--display-active t)))

(defun symon-display ()
  "activate symon display."
  (interactive)
  (setq symon--active-page 0)
  (symon--display-update))

(defun symon--redisplay ()
  "update symon display."
  (when symon--display-active
    (setq symon--active-page (% (1+ symon--active-page) symon--total-page-num))
    (symon--display-update)))

(defun symon--display-end ()
  "deactivate symon display."
  (setq symon--display-active nil))

;;;###autoload
(define-minor-mode symon-mode
  "tiny graphical system monitor"
  :init-value nil
  :global t
  (if symon-mode (symon--initialize) (symon--cleanup)))

;; + provide

(provide 'symon)

;;; symon.el ends here
