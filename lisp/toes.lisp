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

(defun make-data ()
  "load those data tables"
  (make-tricks)
  (make "data/additionalbolts.lisp"
	"data/anneal.lisp"
	"data/audiology.lisp"
	"data/auto93.lisp"
	"data/autoHorse.lisp"
	"data/autoMpg.lisp"
	"data/autoPrice.lisp"
	"data/autos.lisp"
	"data/basketball.lisp"
	"data/bodyfat.lisp"
	"data/bolts.lisp"
	"data/boston-housing.lisp"
	"data/breast-cancer.lisp"
	"data/breastTumor.lisp"
	"data/cholesterol.lisp"
	"data/cleveland-14-heart-disease.lisp"
	"data/cleveland.lisp"
	"data/cloud.lisp"
	"data/contactlens.lisp"
	"data/cpu.lisp"
	"data/credit-rating.lisp"
	"data/detroit.lisp"
	"data/echoMonths.lisp"
	"data/elusage.lisp"
	"data/fishcatch.lisp"
	"data/fruitfly.lisp"
	"data/gascons.lisp"
	"data/german_credit.lisp"
	"data/horse-colic.lisp"
	"data/housing-convert-from-arff.lisp"
	"data/housing.lisp"
	"data/hungarian-14-heart-disease.lisp"
	"data/hungarian.lisp"
	"data/hypothyroid.lisp"
	"data/ionosphere.lisp"
	"data/kr-vs-kp.lisp"
	"data/letter.lisp"
	"data/longley.lisp"
	"data/lowbwt.lisp"
	"data/mammal-sleep.lisp"
	"data/mbagrade.lisp"
	"data/meta.lisp"
	"data/mushroom.lisp"
	"data/pbc.lisp"
	"data/pharynx.lisp"
	"data/pima_diabetes.lisp"
	"data/pollution.lisp"
	"data/primary-tumor.lisp"
	"data/pwLinear.lisp"
	"data/quake.lisp"
	"data/schlvote.lisp"
	"data/segment.lisp"
	"data/sensory.lisp"
	"data/servo.lisp"
	"data/sick.lisp"
	"data/sonar.lisp"
	"data/soybean.lisp"
	"data/splice.lisp"
	"data/strike.lisp"
	"data/vehicle.lisp"
	"data/veteran.lisp"
	"data/vineyard.lisp"
	"data/vote.lisp"
	"data/vowel.lisp"
	"data/waveform.lisp"
	"data/weather.lisp"
	"data/weather.nominal.lisp"
	"data/weather2.lisp"
	"data/weathernumerics.lisp"
	))

(defun make-mini-data ()
  "just a few data sets"
  (make "data/weathernumerics.lisp"
	"data/weather/weather-no-abnormal-test.lisp"
	"data/weather/weather-no-train.lisp"
	"data/weather/weather-yes-normal-test.lisp"
	"data/weather/weather-no-normal-test.lisp"
	"data/weather/weather-yes-abnormal-test.lisp"
	"data/weather/weather-yes-train.lisp"
	))

(defun make-toe()
  "toe stuff"
  (make-tricks)
  (make-tables)
  (make-mini-data)
  (make "toe/distance"
	"toe/toe-base"
	"toe/randomizer"
	"toe/normalize"
	"toe/discretize"
	"toe/bore"
	"toe/era"
	"toe/median"
	"toe/gac"
	"toe/sampler"
	"toe/alienator"
	"toe/sub-sampling"
	"toe/over-sampling"
	"toe/micro-sampling"
	"toe/score"
	"toe/utility"
	"toe/csv"
	"toe/likelyhood"
	"toe/anomaly"
	"cat"
	))

(make-toe)