; Fungsi untuk bertanya kepada user
(deffunction ask-question (?question)
	(printout t ?question "? ")
	(bind ?answer (read))
	(while (not (numberp ?answer)) do
		(printout t ?question "? ")
		(bind ?answer (read))
	)
?answer)

; Rule untuk menanyakan mean_concave_points, ditanyakan pada awal program
(defrule qMeanConcavePoints
	?x <- (initial-fact)
	=>
	(retract ?x)
	(bind ?ans (ask-question "mean_concave_points" ))
	(assert (mean_concave_points ?ans))	
)

; Rule untuk menanyakan worst_radius atau worst_perimeter berdasarkan nilai mean_concave_points
(defrule qWorstRadiusOrWorstPerimeter
	?x <- (mean_concave_points ?mcp)
	=>
	(retract ?x)
  (if (<= ?mcp 0.05)
    then
      (bind ?wr (ask-question "worst_radius" ))
	    (assert (worst_radius ?wr))
    else 
      (bind ?wp (ask-question "worst_perimeter" ))
      (assert (worst_perimeter ?wp))
  )
)

; Rule untuk menanyakan radius_error atau mean_texture berdasarkan nilai worst_radius
(defrule qRadiusErrorOrMeanTexture
	?x <- (worst_radius ?wr)
	=>
	(retract ?x)
  (if (<= ?wr 16.83)
    then
      (bind ?re (ask-question "radius_error" ))
	    (assert (radius_error ?re))
    else 
      (bind ?mt (ask-question "mean_texture" ))
      (assert (mean_texture_wr ?mt))
  )
)

; Rule untuk menanyakan worst texture atau mean_smoothness berdasarkan nilai radius_error
(defrule qWorstTextureOrMeanSmoothness
	?x <- (radius_error ?re)
	=>
	(retract ?x)
	(if (<= ?re 0.63)
		then
			(bind ?wt (ask-question "worst_texture"))
			(assert (worst_texture_re ?wt))
		else
			(bind ?ms (ask-question "mean_smoothness"))
			(assert (mean_smoothness ?ms))
	)
)

; Rule untuk menentukan prediksi breast cancer berdasarkan nilai mean_smoothness
(defrule predictByMeanSmoothness
	?x <- (mean_smoothness ?ms)
	=>
	(retract ?x)
	(if (<= ?ms 0.09)
		then
			(assert (predict 1))
		else
			(assert (predict 0))
	)
)

; Rule untuk menentukan prediksi breast cancer berdasarkan nilai worst_texture_re
(defrule predictByWorstTextureFromRE
	?x <- (worst_texture_re ?wt)
	=>
	(retract ?x)
	(if (<= ?wt 30.15)
		then 
			(assert (predict 1))
		else
			(bind ?wa (ask-question "worst_area"))
			(assert (worst_area ?wa))
	)
)

; Rule untuk menentukan prediksi breast cancer berdasarkan nilai worst_area
(defrule predictByWorstArea
	?x <- (worst_area ?wa)
	=>
	(retract ?x)
	(if (<= ?wa 641.60)
		then
			(assert (predict 1))
		else
			(bind ?mr (ask-question "mean_radius"))
			(assert (mean_radius_wa ?mr))
	)
)

; Rule untuk menentukan prediksi breast cancer berdasarkan nilai mean_radius_wa
(defrule predictByMeanRadiusFromRE
	?x <- (mean_radius_wa ?mr)
	=>
	(retract ?x)
	(if (<= ?mr 13.45)
		then
			(bind ?mt (ask-question "mean_texture"))
			(assert (mean_texture_mr ?mt))
		else
			(assert (predict 1))
	)
)

; Rule untuk menentukan prediksi breast cancer berdasarkan nilai mean_texture_mr
(defrule predictByMeanTextureFromMR
	?x <- (mean_texture_mr ?mt)
	=>
	(retract ?x)
	(if (<= ?mt 28.79)
		then
			(assert (predict 0))
		else
			(assert (predict 1))
	)
)

; Rule untuk menentukan prediksi breast cancer berdasarkan nilai worst_perimeter
(defrule predictByWorstPerimeter
	?x <- (worst_perimeter ?wp)
	=>
	(retract ?x)
	(if(<= ?wp 114.45)
		then
			(bind ?wt (ask-question "worst_texture"))
			(assert (worst_texture ?wt))
		else
			(assert (predict 0))
	)
)

; Rule untuk menanyakan worst_concave_points atau perimeter_error berdasarkan nilai worst_texture
(defrule qWorstConcavePointsOrPerimeterError
	?x <- (worst_texture ?wt)
	=>
	(retract ?x)
  (if (<= ?wt 25.65)
    then
      (bind ?wcp (ask-question "worst_concave_points" ))
	    (assert (worst_concave_points ?wcp))
    else 
      (bind ?pe (ask-question "perimeter_error" ))
      (assert (perimeter_error ?pe))
  )
)

; Rule untuk menentukan prediksi breast cancer berdasarkan nilai worst_concave_points
(defrule predictByWorstConcavePoints
	?x <- (worst_concave_points ?wcp)
	=>
	(retract ?x)
	(if(<= ?wcp 0.17)
		then
			(assert (predict 1))
		else
			(assert (predict 0))
	)
)

; Rule untuk menentukan prediksi breast cancer berdasarkan nilai perimeter_error
(defrule predictByPerimeterError
	?x <- (perimeter_error ?pe)
	=>
	(retract ?x)
	(if(<= ?pe 1.56)
		then
			(bind ?mr (ask-question "mean_radius"))
			(assert (mean_radius_pe ?mr))
		else
			(assert (predict 0))
	)
)

; Rule untuk menentukan prediksi breast cancer berdasarkan nilai mean_radius setelah cek perimeter_error
(defrule predictByMeanRadiusFromPE
	?x <- (mean_radius_pe ?mr)
	=>
	(retract ?x)
	(if(<= ?mr 13.34)
		then
			(assert (predict 0))
		else
			(assert (predict 1))
	)
)

; Rule untuk menentukan prediksi breast cancer berdasarkan nilai mean_texture setelah cek worst_radius
(defrule predictByMeanTextureFromWR
	?x <- (mean_texture_wr ?mt)
	=>
	(retract ?x)
	(if(<= ?mt 16.19)
		then
			(assert (predict 1))
		else
			(bind ?cpe (ask-question "concave_points_error"))
			(assert (concave_points_error ?cpe))
	)
)

; Rule untuk menentukan prediksi breast cancer berdasarkan nilai concave_points_error
(defrule predictByConcavePointsError
	?x <- (concave_points_error ?cpe)
	=>
	(retract ?x)
	(if(<= ?cpe 0.01)
		then
			(assert (predict 0))
		else
			(assert (predict 1))
	)
)

; Rule untuk menampilkan pesan bahwa user terprediksi kanker payudara
(defrule predictTrue
	?p <- (predict 1)
	=>
	(retract ?p)
	(printout t "***********************************************************" crlf)
	(printout t "* Mohon maaf, Anda terprediksi terkena kanker payudara :( *" crlf)
	(printout t "***********************************************************" crlf)
)

; Rule untuk menampilkan pesan bahwa user tidak terprediksi kanker payudara
(defrule predictFalse
	?p <- (predict 0)
	=>
	(retract ?p)
	(printout t "**************************************************************" crlf)
	(printout t "* Selamat, Anda tidak terprediksi terkena kanker payudara :) *" crlf)
	(printout t "**************************************************************" crlf)
)