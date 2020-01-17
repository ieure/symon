;;; symon-emms.el --- Symon monitor for EMMS         -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; Keywords: multimedia

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

(defgroup symon-monitor-emms nil
  "Fixme"
  :group 'symon)

(defface symon-monitor-emms-artist-face
  '((t :inherit emms-browser-artist-face))
  "Face for artist names."
  :group 'symon-monitor-emms)


(defface symon-monitor-emms-title-face
  '((t :inherit emms-playlist-track-face))
  "Face for track titles."
  :group 'symon-monitor-emms)

(defface symon-monitor-emms-time-face
  '((t :inherit default))
  "Face for playing time."
  :group 'symon-monitor-emms)

(defclass symon-monitor-emms (symon-monitor) nil)

(cl-defmethod symon-monitor-fetch ((this symon-monitor-emms))
  (when (and (featurep 'emms) (not emms-player-stopped-p))
    (emms-playlist-current-selected-track)))

(defun symon-monitor-emms--merge (track)
  (unless (memq 'info-artist track)
    (push '(info-artist) track))
  (unless (memq 'info-title track)
    (push '(info-title) track))
  track)

(defun symon-monitor-emms--display-file (track)
  (let ((info-artist (cdr (assoc 'info-artist track)))
        (info-title (cdr (assoc 'info-title track)))
        (name (cdr (assoc 'name track))))
      (s-join " " (list
               (propertize (s-trim emms-playing-time-string) 'face 'symon-monitor-emms-time-face)
               (s-join " - "
                       (or (cl-remove-if 'null (list (when info-artist (propertize info-artist 'face 'symon-monitor-emms-artist-face))
                                                     (when info-title (propertize info-title 'face 'symon-monitor-emms-title-face))))
                           (list (propertize (file-name-base name) 'face 'symon-monitor-emms-title-face))))))))


(defun symon-monitor-emms--display-url (track)
  (let ((pu (url-generic-parse-url (cdr (assoc 'name track)))))
    (propertize (url-host pu) 'face 'symon-monitor-emms-title-face)))

(cl-defmethod symon-monitor-display ((this symon-monitor-emms))
  (if-let ((track (symon-monitor-value this)))
    (concat
     (cond
      (emms-player-playing-p "▶ ")
      (emms-player-paused-p "⏸ ")
      (t ""))

     (pcase (cdr (assoc 'type track))
      ('file (symon-monitor-emms--display-file track))
      ('url (symon-monitor-emms--display-url track))))))


(provide 'symon-emms)
;;; symon-emms.el ends here
