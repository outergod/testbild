;;;; testbild - producer.lisp
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

(in-package :testbild)

(defclass producer ()
  ((stream :initarg :stream
           :initform *standard-output*
           :accessor test-producer-stream
           :documentation "Stream to use for test output")
   (tests-run :initform 0
              :accessor tests-run
              :documentation "Recorded number of test results emitted using this
producer."))
  (:documentation "Base class for test output producers."))

(defmethod reinitialize-instance :after ((producer producer) &rest initargs &key &allow-other-keys)
  "reinitialize-instance :after producer &rest initargs &key &allow-other-keys => 0

Reset the recorded number of tests run to zero."
  (declare (ignore initargs))
  (setf (tests-run producer) 0))

(defgeneric init-test (producer)
  (:documentation "Test initialization / header output.")
  (:method (producer)))
(defgeneric emit-plan (producer &key plan plan-argument &allow-other-keys)
  (:documentation "A so-called test plan is used e.g. by TAP as a means of
cross-checking proper test suite execution.")
  (:method (producer &key plan plan-argument &allow-other-keys)))
(defgeneric emit-result (producer &key success description directive reason &allow-other-keys)
  (:documentation "Emit the result from running an test / an assertion.")
  (:method :before ((producer producer) &key success description directive reason &allow-other-keys)
    "emit-result :before producer &key success description directive reason &allow-other-keys => number

Increase the recorded number of tests by one."
    (incf (tests-run producer))))
(defgeneric emit-comment (producer comment)
  (:documentation "Emit a comment, if available for this kind of producer.")
  (:method (producer comment)))
(defgeneric emit-bailout (producer  &optional reason)
  (:documentation "Emit a bail-out, if available for this kind of producer.")
  (:method (producer &optional reason)))
(defgeneric finalize-test (producer)
  (:documentation "Emit final test output.")
  (:method (producer)))
