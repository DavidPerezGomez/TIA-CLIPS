(deffacts hechos
  (hecho1 1)
)

; usa el primer hecho para crear el hecho que acumulará el resultado
(defrule recoger_primer_hecho
  (declare (salience 10))
  (hecho1 ?num)
  (not (actual ? ?))
  (test (>= ?num 0))
=>
  (assert (actual ?num 1))
)

; hace el factorial
(defrule factorial
  (declare (salience 0))
  ?f <- (actual ?num ?acc)
  (not(finalizado))
=>
  (if (> ?num 1) then
    (retract ?f)
    (assert (actual (- ?num 1) (* ?acc ?num)))
  else
    (assert (finalizado))
    (assert (resultado ?acc))
  )
)
