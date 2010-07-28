;;;; testbild - x-unit.lisp
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

(defclass x-unit-producer (producer)
  ((fill-column :initarg :fill-column
                :initform nil
                :reader producer-fill-column
                :documentation "Line feeds will be inserted after FILL-COLUMN
characters if set."))
  (:documentation "Producer for xUnit style test output."))

(defmethod init-test ((producer x-unit-producer) stream)
  "init-test producer stream => nil

xUnit output always starts on a fresh line."
  (fresh-line stream))

(defmethod emit-result :before ((producer x-unit-producer) stream &key success description directive reason &allow-other-keys)
  "emit-result :before producer stream &key success description directive reason &allow-other-keys => nil

Ensure output proceeds on a fresh line after FILL-COLUMN test assertions, if set."
  (declare (ignore success description directive reason))
  (with-accessors ((tests-run tests-run)
                   (fill-column producer-fill-column))
      producer
    (when (and fill-column
               (zerop (mod tests-run fill-column)))
      (fresh-line stream))))

(defmethod emit-result ((producer x-unit-producer) stream &key (success t) description directive reason &allow-other-keys)
  "emit-result producer stream &key (success t) description directive reason &allow-other-keys => nil

xUnit output consists of single characters per assertion."
  (declare (ignore description reason))
  (write-char (cond ((null directive)
                     (if success #\. #\F))
                    ((eql :error directive) #\E)
                    ((eql :todo directive) #\I)
                    ((eql :skip directive) #\S)
                    (t (error (format nil "~s is not a recognized test directive" directive))))
              stream))

(defmethod emit-comment ((producer x-unit-producer) stream comment)
  "emit-comment producer stream comment => nil

xUnit has no support for comments so we use STDERR."
  (declare (ignore stream))
  (format *error-output* "~a~%" comment))
  
(defmethod finalize-test ((producer x-unit-producer) stream)
  "finalize-test producer stream => nil

xUnit output always ends with a line feed."
  (terpri stream))
