(require 'symon)

(require 'symon)

(defgroup symon-battery nil
  "Customization for Symon battery monitor."
  :group 'symon)

(defcustom symon-battery-charging-indicator "^"
  "Indicator used when the system is plugged in and/or charging."
  :type 'string
  :group 'symon-battery)

(defcustom symon-battery-discharging-indicator "v"
  "Indicator used when the system is discharging the battery"
  :type 'string
  :group 'symon-battery)

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
  "Face for when the battery is discharging, and holds more than an hour's charge."
  :group 'symon-battery)

(defface symon-battery-charging-face
  '((t :inherit symon-battery-full-face))
  "Face for when the battery is being charged."
  :group 'symon-battery)

(defun symon-battery--face (charging time-left)
  "Return the face to use for the battery monitor."
  (if charging 'symon-battery-charging-face
    (cl-destructuring-bind (hh mm) (mapcar #'read (split-string time-left ":"))
      (cond
       ((and (= hh 0) (> mm 30)) 'symon-battery-medium-face)
       ((= hh 0) 'symon-battery-low-face)
       (t 'symon-battery-full-face)))))

(defclass symon-battery (symon-monitor)
  ((interval :initform 10)
   (default-display-opts
     :initform '(:charging-indicator "^"
                                     :discharging-indicator "v"))))

(cl-defmethod symon-monitor-fetch ((this symon-battery))
  (when battery-status-function
    (funcall battery-status-function)))

(cl-defmethod symon-battery--indicator ((this symon-battery))
  (with-slots (display-opts) this
    (plist-get display-opts
               (if charging :charging-indicator
                 :discharging-indicator))))

(cl-defmethod symon-monitor-display ((this symon-battery))
  (when-let ((status (symon-monitor-value this)))
    (let ((charging (string= (downcase (cdr (assoc ?L status))) "ac"))
          (percent (read (cdr (assoc ?p status))))
          (time-left (cdr (assoc ?t status))))
      (thread-first
          (format "%d%%%s"
                  percent
                  (if (string= time-left "N/A")
                      "" (concat (symon-battery--indicator this) time-left)))
        (propertize 'face (symon-battery--face charging time-left))))))

(provide 'symon-battery)
