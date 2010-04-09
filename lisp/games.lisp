;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This file is part of AIslash.
;
; AIslash is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; AIslash is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with AIslash.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; disable an irritating SBCL flag
;#+SBCL (DECLAIM (SB-EXT:MUFFLE-CONDITIONS CL:STYLE-WARNING))

(defun make (&rest files)
   (handler-bind 
      ((style-warning #'muffle-warning))
  	(dolist (f files)
	  (load f))))

(defun make-tricks ()
  "timm tricks"
  (make  "lisp101/deftest" ; must be first
	 "lisp101/caution"
	 "lisp101/strings"
	 "lisp101/hash"
	 "lisp101/list"
	 "lisp101/random"
	 "lisp101/any" 
	 "lisp101/reading"
	 "lisp101/normal"
	 "lisp101/number"
	 "lisp101/lispfuns"
	 "lisp101/debug" 
	 ))

(defun make-tables ()
  "timm's table tricks"
  (make-tricks)
  (make  "table/structs"
	 "table/header"
	 "table/data"
	 "table/table"
	 "table/xindex" 
	 ))

(defun make-game()
  "gaming code"
  (make-tricks)
  (make  "game/globals"
	 "game/macros"
	 "game/defstructs"
	 "game/roaming"
	 "game/types" 
	 "game/printing"
	 "game/creation"
	 "game/stagger" 
	 ))

(make-game)
