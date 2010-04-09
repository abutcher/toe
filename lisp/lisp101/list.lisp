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

(defmacro geta (key list)
  `(cdr (assoc ,key ,list)))

(defmacro last1 (l) `(first (last ,l)))

(defun accumulate (* first rest)
  (if (null rest)
      first
      (let ((first1 (first rest))
            (rest1  (rest rest)))
        (accumulate *
                    (funcall * first first1)
                    rest1))))

(deftest test-accumulate ()
  (accumulate #'min 10 '(11 12 9 13)))

(defun shuffle (l)
  (dotimes (i (length l) l)
    (rotatef
     (elt l i)
     (elt l (my-random-int (length l))))))

(defun allbut (l n)
  (if (zerop n)
      (values (rest l) (first l))
      (cons (first l) (allbut (rest l) (1- n)))))

(defun transpose (x)
   (apply #'mapcar (cons #'list x)))

(defmacro doitems ((one n list &optional out) &body body )
  `(let ((,n -1))
     (dolist (,one ,list ,out)  (incf ,n) ,@body)))
