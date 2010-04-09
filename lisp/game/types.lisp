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


(defun make-grid ()
  (make-array (list (fixed-height *f*) (fixed-width *f*))))

(defmethod aspiece ((x piece)) x)
(defmethod aspiece ((x symbol)) (char->piece x))

(defun atp (x)
  (geta x (fixed-directions *f*)))

(defmethod piece->char ((x piece))
  (first (rassoc (type-of x) (fixed-chars *f*))))
(defun char->type (x)
  (geta x (fixed-chars *f*)))
(defun char->piece (x)
  (let* ((type        (char->type x))
         (constructor (intern (string-upcase 
                               (format nil "make-~a" type)))))
    (unless type
      (error "~a unknown" x))
    (funcall constructor)))
