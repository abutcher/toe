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

(defmethod move-to ((new-pos point) board)
  (let* ((new        (piece-at new-pos board))
         (old-pos    (board-at board)))
    (setf (board-at board) new-pos)
    (rotatef 
     (piece-at new-pos board)
     (piece-at old-pos board))))

(defmethod neighbors ((p point))
  (mapcar #'(lambda (q) 
              (make-point :x (+ (point-x p) (second q))
                          :y (+ (point-y p) (third  q))))
          (fixed-steps *f*)))

(defmethod neighbors ((b board))
  (neighbors (board-at b)))

(defun empty-neighbors (board)
  (let (out)
    (dolist (point (neighbors board) out)
      (unless (typep point 'blocks)
        (if (inbounds-p board point)
          (push point out))))))

(defun inbounds-p (board point)
  (let ((xmin 0) (xmax (board-width board))
        (ymin 0) (ymax (board-height board)))
    (and (<= xmin (point-x point) xmax)
         (<= ymin (point-y point) ymax))))
