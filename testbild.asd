;;;; testbild - testbild.asd
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

(in-package :cl-user)

(defpackage :testbild-system
  (:use :cl :asdf))

(in-package :testbild-system)

(asdf:defsystem :testbild
                :description "Universal test output production and consumption facility"
                :version "0.0.1"
                :author "Alexander Kahl <e-user@fsfe.org>"
                :license "GPLv3+"
                :depends-on (:graylex :cl-ppcre)
                :components
                ((:module "src"
                          :components
                          ((:file "package")
                           (:file "producer" :depends-on ("package"))
                           (:file "tap" :depends-on ("package" "producer"))))))
