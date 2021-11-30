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
      (assert (mean_texture ?mt))
  )
)