; 2) Crea un programa mediante reglas que calcule el hecho que contenga el número
; más grande.

(defrule def_maximo
    (declare (salience 20))
    (numero ?n)
    (not (maximo ?m))
    =>
    (assert (maximo ?n))
)

(defrule compare
    (declare (salience 10))
    (numero ?n)
    ?max <- (maximo ?m)
    (test (> ?n ?m))
    =>
    (retract ?max)
    (assert (maximo ?n))
)

(defrule end
    (declare (salience 0))
    (maximo ?m)
    =>
    (printout t "El máximo es " ?m crlf)
    (halt)
)

(deffacts init
    (numero 13)
    (numero 10)
    (numero 8)
    (numero 5)
)
