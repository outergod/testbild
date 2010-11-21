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

(in-package :testbild-test)

(in-suite all)
(defsuite tap)
(in-suite tap)

(defmacro test-tap-sequence ((producer stream expected) &body body)
  (let ((string (gensym))
        (out (gensym)))
    `(let ((,string (make-array 0 :element-type 'character
                                  :adjustable t :fill-pointer 0))
           (,producer (make-instance 'tap-producer)))
       (with-output-to-string (,out ,string)
         (let ((,stream (make-instance 'test-output-stream :stream ,out)))
           ,@body
           (is (string= ,string ,expected)))))))

(defmacro deftaptest (name expected &body body)
  `(deftest ,name ()
     (test-tap-sequence (producer stream ,expected)
       ,@body)))

(deftaptest emit-nothing #>eof>TAP version 13
eof
  (init-test producer stream))

(deftaptest emit-ok-nodesc #>eof>TAP version 13
ok 1
eof
  (init-test producer stream)
  (emit-result producer stream))

(deftaptest emit-nok-nodesc #>eof>TAP version 13
not ok 1
eof
  (init-test producer stream)
  (emit-result producer stream :success nil))

(deftaptest emit-ok-desc #>eof>TAP version 13
ok 1 - Hello World!
eof
  (init-test producer stream)
  (emit-result producer stream :description "Hello World!"))

(deftaptest emit-nok-desc #>eof>TAP version 13
not ok 1 - Goodbye World!
eof
  (init-test producer stream)
  (emit-result producer stream :success nil :description "Goodbye World!"))

(deftaptest emit-simple-plan #>eof>TAP version 13
1..3
eof
  (init-test producer stream)
  (emit-plan producer stream :plan-argument 3))
