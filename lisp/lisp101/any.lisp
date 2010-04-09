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

(defmethod any ((x list))
  (nth (my-random-int (length x)) x))

(defmethod any ((x hash-table))
  (let ((count (hash-table-count x)))
    (unless (zerop count)
      (let  ((n (1+ (my-random-int count))))
        (dohash (key value x)
          (if (<= (decf n) 0)
              (return-from any value)))))))

(defun <~ (n1 n2)
  (if (= n1 n2)
      (< (my-random 100) 50)
      (< n1 n2)))
