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

(defmacro doarray ((one i a &optional out) &body body )
  (let ((max (gensym))
        (b   (gensym)))
    `(let* ((,b ,a)
            (,max (length ,b)))
       (dotimes (,i ,max ,out) 
         (let ((,one (aref ,b ,i)))
           ,@body)))))

(defmacro doarray2 ((one two i a1 a2 &optional out) &body body )
  (let ((max (gensym))
        (b1   (gensym))
        (b2   (gensym)))
    `(let* ((,b1 ,a1)
            (,b2 ,a2)
            (,max (length ,b1)))
       (dotimes (,i ,max ,out)
         (let ((,one (aref ,b1 ,i))
               (,two (aref ,b2 ,i)))
           ,@body)))))
