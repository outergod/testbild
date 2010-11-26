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

(defmacro test-tap-sequence ((producer stream expected &optional description) &body body)
  (let ((string (gensym))
        (out (gensym)))
    `(let ((,string (make-array 0 :element-type 'character
                                  :adjustable t :fill-pointer 0)))
       (with-output-to-string (,out ,string)
         (let* ((,stream (make-instance 'test-output-stream :stream ,out))
                (,producer (make-instance 'tap-producer :stream ,stream)))
           ,@body
           (ok ,string ,expected ,description))))))

(defmacro deftaptest (name expected (&optional description) &body body)
  `(deftest ,name
     (test-tap-sequence (producer stream ,expected ,description)
       ,@body)))

(deftaptest emit-nothing #>eof>TAP version 13
eof
  ("basic functionality")
  (init-test producer))

(deftaptest emit-nothing #>eof>TAP version 13
eof
  ("finalizing")
  (init-test producer)
  (finalize-test producer))

(deftaptest emit-ok-nodesc #>eof>ok 1
eof
  ("ok without description")
  (emit-result producer))

(deftaptest emit-nok-nodesc #>eof>not ok 1
eof
  ("not ok without description")
  (emit-result producer :success nil))

(deftaptest emit-ok-desc #>eof>ok 1 - Hello World!
eof
  ("ok with description")
  (emit-result producer :description "Hello World!"))

(deftaptest emit-nok-desc #>eof>not ok 1 - Goodbye World!
eof
  ("not ok with description")
  (emit-result producer :success nil :description "Goodbye World!"))

(deftaptest emit-simple-plan #>eof>1..3
eof
  ("simple plan emission")
  (emit-plan producer :plan-argument 3))

;;; Reproduces https://github.com/e-user/testbild/issues#issue/1
(deftaptest github-issue-1 #>eof>1..0 # SKIP
eof
  ("check for github-issue-1 regression")
  (emit-plan producer :plan :skip))

(deftaptest emit-ok-empty-desc #>eof>ok 1 - 
eof
  ("ok with empty description")
  (emit-result producer :description ""))

(deftaptest emit-mixed-set #>eof>TAP version 13
1..5
ok 1
not ok 2
ok 3
ok 4
not ok 5
eof
  ("mixed tests, sequence numbers")
  (init-test producer)
  (emit-plan producer :plan-argument 5)
  (emit-result producer)
  (emit-result producer :success nil)
  (emit-result producer)
  (emit-result producer)
  (emit-result producer :success nil)
  (finalize-test producer))
