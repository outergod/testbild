;;;; testbild - test.lisp
;;;; Copyright (C) 2010  Alexander Kahl <e-user@fsfe.org>
;;;; This file is part of testbild.
;;;; testbild is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; testbild is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :testbild-test)

(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)

;;; String output streams don't properly support BEGINNING-OF-LINE-P at least
;;; for SBCL so we just implement that ourselves :)
(defclass test-output-stream (fundamental-character-output-stream trivial-gray-stream-mixin)
   ((stream :initarg :stream
            :reader test-stream
            :initform (alexandria:required-argument :stream)
            :documentation "Wrapped output stream")
    (beginning-of-line :accessor beginning-of-line-p
                       :initform t
                       :documentation "Whether the current stream position
refers to the beginning of a line")
    (beginning-of-line-charbag :reader beginning-of-line-charbag
                               :allocation :class
                               :initform (list #\Newline #\Return))))

(defmethod close ((stream test-output-stream) &key abort)
  (close (test-stream stream) :abort abort))

(defmethod stream-start-line-p ((stream test-output-stream))
  (beginning-of-line-p stream))

(defmethod stream-write-char :before ((stream test-output-stream) character)
  (with-slots ((bol beginning-of-line)
               (bol-charbag beginning-of-line-charbag)) stream
    (setq bol (find character bol-charbag))))

(defmethod stream-write-char ((stream test-output-stream) character)
  (write-char character (test-stream stream)))


;;; We cannot rely on any test framework like stefil that we expect to support
;;; testbild itself, so testing is done manually
(defparameter *tests* nil)

(defmacro deftest (&body body)
  `(push #'(lambda ()
             ,@body)
         *tests*))

(let ((tests-run))
  (defun run ()
    (setq tests-run 0)
    (format t "~&1..~d~%" (length *tests*))
    (mapcar #'funcall *tests*))

  (defun ok (got expected &optional description)
    (format t "~&~:[not ~;~]ok ~d~@[ - ~a~]~%"
            (string= got expected)    
            (incf tests-run) description)))
