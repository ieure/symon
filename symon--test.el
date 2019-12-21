;;; symon--test.el --- Symon tests                   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; Keywords: internal

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
(require 'ert)

(ert-deftest symon--display-catching-errors--test ()
  (defclass symon--display-catching-errors--failer (symon-monitor)
    ((n :initform 0)))
  (cl-defmethod symon-monitor-display ((this symon--display-catching-errors--failer))
    (incf (oref this n))
    (/ 1 0))

  (let ((m (symon--display-catching-errors--failer)))
    (should (null (symon--display-catching-errors m)))
    (should (= 1 (oref m n)))))

(provide 'symon--test)
;;; symon--test.el ends here
