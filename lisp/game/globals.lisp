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

(defstruct fixed 
  "fixed stuff"
  (inf  most-positive-fixnum)
  (ninf (* -1 most-positive-fixnum))
  (version 0.01)
  (width   21) ; counting from zero
  (height  10)  ; counting from zero
  (chars   '((#\a . axe)
             (#\k . key)
             (#\d . dynamite)
             (#\g . gold)
             (#\B . bush)
             (#\- . door)
             (#\. . blank)
             (#\* . wall)
             (#\  . blank)
             (#\~ . water)
             (#\^ . player)
             (#\> . player)
             (#\v . player)
             (#\< . player )))
  (steps   '(     ;deltax deltay
             (#\^  0     -1)
             (#\>  1      0)
             (#\v  0      1)
             (#\< -1      0))))
                     
(defparameter *f* (make-fixed))

(defstruct wm "runtime working memory"
  (toolong 1000)
  board)

(defparameter *w* (make-wm))

(defun zap ()
  (setf *w* (make-wm)))
