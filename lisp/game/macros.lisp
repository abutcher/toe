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

(defmacro piece-at (piece board)
  `(aref (board-contents ,board)
        (point-y ,piece)
        (point-x ,piece)))

(defmacro walk ((i j cell endp board &optional  out) &rest body)
  (let ((imax     (gensym))
	(jmax     (gensym))
	(contents (gensym)))
    `(let (,cell
	     (,imax     (1+ (board-width  ,board)))
	     (,jmax     (1+ (board-height ,board)))
	     (,contents (board-contents ,board)))
       (dotimes (,j ,jmax ,out)
	 (dotimes (,i ,imax)
           (let ((,endp (= ,i (1- ,imax))))
             (setf ,cell (aref ,contents ,j ,i))
             ,@body))))))
