;;; symon-monitor--test.el --- Tests for symon-monitor  -*- lexical-binding: t; -*-

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

(require 'symon-monitor)
(require 'ert)

(ert-deftest symon-monitor--test-fetch-error ()
  (defclass symon-monitor--test-fetch-error (symon-monitor) nil)
  (cl-defmethod symon-monitor-fetch ((this symon-monitor--test-fetch-error))
    (error "Testing"))

  (let ((m (symon-monitor--test-fetch-error)))
    (with-slots (fetch-errors-warned) m
      (should (null (memq 'err fetch-errors-warned)))
      (should (null (symon-monitor-update m)))
      (should (memq 'error fetch-errors-warned)))))

(ert-deftest symon-monitor--test-display-opts ()
  (defclass symon-monitor--test-display-opts (symon-monitor) nil)

  (let ((m (symon-monitor--test-display-opts)))
    (should (slot-boundp m 'display-opts))
    (should (null (oref m display-opts))))

  (let ((m (symon-monitor--test-display-opts :display-opts '(:foo 1))))
    (should (slot-boundp m 'display-opts))
    (should (equal '(:foo 1) (oref m display-opts)))))

(ert-deftest symon-monitor--test-default-display-opts ()
  (defclass symon-monitor--test-default-display-opts (symon-monitor)
    ((default-display-opts :initform '(:foo 1))))

  (let ((m (symon-monitor--test-default-display-opts)))
    (should (slot-boundp m 'display-opts))
    (should (equal '(:foo 1) (oref m display-opts))))

  (let ((m (symon-monitor--test-default-display-opts :display-opts '(:bar 2))))
    (should (slot-boundp m 'display-opts))
    (should (equal '(:foo 1 :bar 2) (oref m display-opts)))))

(ert-deftest symon-monitor--test-plist-merge ()
  ;; Unique keys are added
  (should (equal '(:a 1 :b 2) (symon-monitor--plist-merge '(:a 1) '(:b 2))))

  ;; Duplicate keys are replaced
  (should (equal '(:a 2) (symon-monitor--plist-merge '(:a 1) '(:a 2))))

  ;; Recursive lists are merged
  (should (equal '(:a (:foo 1 :bar 2)) (symon-monitor--plist-merge '(:a (:foo 1))
                                                                   '(:a (:bar 2))))))

(ert-deftest symon-monitor-history--test-update ()
  (defclass symon-monitor-history--test-update (symon-monitor-history)
    ((n :initform 0)))

  (cl-defmethod symon-monitor-fetch ((this symon-monitor-history--test-update))
    (incf (slot-value this 'n)))

  (let ((m (symon-monitor-history--test-update)))
    (should (= 1 (symon-monitor-update m)))
    (should (= 2 (symon-monitor-update m)))
    (should (= (symon-monitor-update m) (symon-monitor-value m)))
    (should (equal '(3 2 1) (seq-take (ring-elements (symon-monitor-history m)) 3)))))

(provide 'symon-monitor--test)
;;; symon-monitor--test.el ends here
