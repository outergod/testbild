;;;; testbild - tap.lisp
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

(defclass tap-producer (producer)
  ((version :initarg :version
            :initform 13
            :reader tap-version
            :documentation "TAP output version.")
   (supported-versions :allocation :class
                       :initform '(12 13)
                       :reader supported-tap-versions
                       :documentation "List of supported TAP versions."))
  (:documentation "Producer for TAP (Test Anything Protocol) output."))

(defmethod shared-initialize :after ((producer tap-producer) slot-names &rest initargs &key &allow-other-keys)
  "shared-initialize :after producer slot-names &rest initargs &key &allow-other-keys => nil

Ensure the chosen TAP version is actually supported by TAP-PRODUCER."
  (declare (ignore slot-names initargs))
  (with-accessors ((version tap-version)
                   (supported supported-tap-versions))
      producer
    (assert (find version supported) ()
            (format nil "TAP version ~d is not among the supported versions [~{~a~^, ~}]" version supported))))

(defmethod init-test ((producer tap-producer) stream)
  "init-test producer stream => nil

TAP version 13 and higher supports an optional version line in the header."
  (with-accessors ((version tap-version))
      producer
    (when (>= version 13)
      (format stream "~&TAP version ~d~%" (tap-version producer)))))

(defmethod emit-plan :before ((producer tap-producer) stream &key plan plan-argument)
  "emit-plan :before producer stream &key plan plan-argument => nil

TAP test plans must always begin at the beginning of a line and start with
\"1..\"."
  (declare (ignore plan plan-argument))
  (format stream "~&1.."))

(defmethod emit-plan ((producer tap-producer) stream &key (plan :simple) plan-argument &allow-other-keys)
  "emit-plan producer stream &key (plan :simple) plan-argument => nil

TAP plans can be of type :SIMPLE or :SKIP, the latter means to skip all tests
within the suite."
  (cond ((eql :simple plan)
         (assert (and (integerp plan-argument)
                      (> plan-argument 0))
                 (plan-argument)
                 ":PLAN-ARGUMENT must be a positive integer for simple plans")
         (format stream "~d~%" plan-argument))
        ((eql :skip plan)
         (princ "0 " stream)
         (emit-comment producer stream (format nil "SKIP~@[ ~a~]~%" plan-argument)))
        (t (error (format nil "~s is not a recognized plan type" plan)))))
  
(defmethod emit-result :before ((producer tap-producer) stream &key success description directive reason &allow-other-keys)
  "emit-result :before producer stream &key success description directive reason &allow-other-keys => nil

TAP test results must always begin at the beginning of a line."
  (declare (ignore producer success description directive reason))
  (fresh-line stream))

(defmethod emit-result ((producer tap-producer) stream &key (success t) description directive reason &allow-other-keys)
  "emit-result producer stream &key (success t) description directive reason &allow-other-keys => nil

TAP style test result emitter. DIRECTIVE types :TODO and :SKIP are supported."
  (format stream "~:[not ~;~]ok ~d ~@[- ~a ~]" success (tests-run producer) description)
  (cond ((null directive)
         (terpri stream))
        ((eql :todo directive)
         (emit-comment producer stream (format nil "TODO~@[ ~a~]~%" reason)))
        ((eql :skip directive)
         (emit-comment producer stream (format nil "SKIP~@[ ~a~]~%" reason)))
        (t (error (format nil "~s is not a recognized test directive" directive)))))

(defmethod emit-comment ((producer tap-producer) stream comment)
  "emit-comment producer stream comment => comment-list

TAP comments are delimited by hash marks and EOLs so COMMENT is split by all
known EOL delimiter combinations and output with a hash mark in front each."
  (mapc #'(lambda (string)
            (format stream "# ~a~%" string))
        (cl-ppcre:split "(\\r?\\n|\\r)" comment)))
