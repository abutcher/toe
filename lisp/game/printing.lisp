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

(defun onstring (x)
  (with-output-to-string (str)
    (onstream x str)))

(defmethod onstream ((x blank) &optional (str *standard-output*))
   (princ " " str))

(defmethod onstream ((x piece) &optional (str *standard-output*))
  (princ (piece->char x) str))

(defmethod onstream ((x board) &optional (str *standard-output*))
  (let (sep 
        (max (1- (fixed-width *f*))))
    (format str "~%")
    (walk (i j cell endrowp x)
	  (princ (or sep "") str)
	  (onstream cell str)
          (if endrowp (princ #\Newline str)))))

(defmethod onstream ((x player) &optional (str *standard-output*))
  (princ (player-direction x) str))
