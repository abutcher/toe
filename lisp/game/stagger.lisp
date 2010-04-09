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

(defun stagger (board &optional (n 10))
   (onstream board)
   (cond ((< n 0)  (onstream board))
         (t        (stagger1 board)
                   (stagger board (1- n)))))

(defun stagger1 (board)
  (move-to (any (empty-neighbors board)) board))

(deftest test-stagger ()
  (reset-seed)
  (let* ((board (read1))
         (some  (empty-neighbors board))
         (one   (any some)))
    (stagger (read1))))
