; 1) Crea un programa mediante reglas que haga la cuenta atrás e imprima por pantalla.
; Para ello crea un hecho que contenga un número para hacer la cuenta atrás.

(defrule decrease
    (declare (salience 0))
    ?f <- (numero ?n)
    =>
    (printout t ?n crlf)
    (retract ?f)
    (assert (numero (- ?n 1)))
)

(defrule stop
    (declare (salience 10))
    ?f <- (numero 0)
    =>
    (retract ?f)
)

(deffacts init
    (numero 5)
)
