(require 'symon-monitor)

(defgroup symon-time nil
  "Display the current time in Symon."
  :group 'symon)

(defface symon-time-face
  '((t :foreground "blue"))
  "Face for Symon time display."
  :group 'symon-time)

(defun symon-time--symbol (hh mm)
  "Return Unicode clock face glyph for time HH MM."

  ;; Unicode clock face glyphs are arranged in two blocks.  Whole
  ;; hours, from 1:00 - 12:00 from #x1f550 - #x1f55b; then half-hours
  ;; 1:30 - 12:30 from #x1f55c - #x1f567.
  ;;
  ;; The approach is to start at 12:30 and subtract the correct number
  ;; of positions to represent the current time.
  ;;
  ;; Hours are simple: Normalize it to 12-hour time by taking the
  ;; modulo of the hour and 12, then subtract that from 12 to get the
  ;; offset from the end of the list of glyphs.
  ;;

  (let ((h-offset (- 12 (% hh 12)))
        (m-offset 0))                   ; Default to hh:30

    (when (>= mm 45)                    ; Round up to the next hour
      (setq h-offset (- 12 (% (1+ hh) 12))
            m-offset 12))               ; hh:00
    (when (<= mm 15)                    ; Round the current hour down
      (setq m-offset 12))               ; hh:00

    (string (- #x1f567 h-offset m-offset))))

(defun symon-time--format (time format)
  "Format TIME according to FORMAT.

FORMAT is a superset of `format-time-string':

%f is a Unicode clock face representing the approximate current
time."
  (thread-first
      (s-replace "%f"
                 (pcase (decode-time)
                   (`(,_ ,mm ,hh . ,_) (symon-time--symbol hh mm)))
                 format)
    (format-time-string time)))

(defclass symon-time (symon-monitor)
  ((default-display-opts '(:format "%H:%M")))
  :documentation "Display the time in Symon.")

(cl-defmethod symon-monitor-fetch ((this symon-time))
  "Fetch the current time."
  (current-time))

(cl-defmethod symon-monitor-display ((this symon-time))
  "Display the current time according to the configured format."
  (thread-first
      (symon-time--format ,format)
    (propertize 'face 'symon-time-face)))

(ert-deftest symon-time--symbol-test ()
  (string= "🕐" (symon-time--symbol 1 0))
  (string= "🕑" (symon-time--symbol 2 0))
  (string= "🕒" (symon-time--symbol 3 0))
  (string= "🕓" (symon-time--symbol 4 0))
  (string= "🕔" (symon-time--symbol 5 0))
  (string= "🕕" (symon-time--symbol 6 0))
  (string= "🕖" (symon-time--symbol 7 0))
  (string= "🕗" (symon-time--symbol 8 0))
  (string= "🕘" (symon-time--symbol 9 0))
  (string= "🕙" (symon-time--symbol 10 0))
  (string= "🕚" (symon-time--symbol 11 0))
  (string= "🕛" (symon-time--symbol 12 0))

  (string= "🕜" (symon-time--symbol 1 30))
  (string= "🕝" (symon-time--symbol 2 30))
  (string= "🕞" (symon-time--symbol 3 30))
  (string= "🕟" (symon-time--symbol 4 30))
  (string= "🕠" (symon-time--symbol 5 30))
  (string= "🕡" (symon-time--symbol 6 30))
  (string= "🕢" (symon-time--symbol 7 30))
  (string= "🕣" (symon-time--symbol 8 30))
  (string= "🕤" (symon-time--symbol 9 30))
  (string= "🕥" (symon-time--symbol 10 30))
  (string= "🕦" (symon-time--symbol 11 30))
  (string= "🕜" (symon-time--symbol 12 30)))

(provide 'symon-time)
