;;; symon-sparkline--test.el --- Tests for Symon sparklines  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; Keywords: multimedia, lisp

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

(ert-deftest symon-sparkline-gridded--test-failing ()
  (let ((m (symon-sparkline-gridded)))
    (symon-sparkline-graph m '(5 13 1 1 1 2 1 4 7 36 43 43 43 51 42 47
                                 12 9 20 54 30 49 46 47 62 100 100 42 57
                                 66 54 51 48 37 44 48 52 48 57 57 72 65
                                 68 47 53 45 54 46 47 42))))


(provide 'symon-sparkline--test)
;;; symon-sparkline--test.el ends here
