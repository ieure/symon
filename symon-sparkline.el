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

(cl-defun symon--make-sparkline (list &key (()))
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

(provide 'symon-sparkline)
