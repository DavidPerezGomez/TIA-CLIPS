; 3) Crea un programa mediante reglas que calcule el hecho que contenga el número
; más grande.

(deffunction maximo($?array)
    (bind ?max (nth$ 1 $?array))
    (foreach ?v (rest$ $?array)
        (if (> ?v ?max)
        then (bind ?max ?v)
        )
    )
    (return ?max)
)

(defrule def_max
    (declare (salience 20))
    (array $?a)
    (not (maximo ?m))
    =>
    (assert (maximo (maximo $?a)))
)

(defrule compare
    (declare (salience 10))
    (array $?a)
    ?max_f <- (maximo ?m)
    (test (> (maximo $?a) ?m))
    =>
    (retract ?max_f)
    (assert (maximo (maximo $?a)))
)

(defrule end
    (declare (salience 0))
    (maximo ?m)
    =>
    (printout t "El máximo es " ?m crlf)
)

(deffacts init
    (array 1 2 3 4 5 6)
    (array 1 3 10 9 7 2)
    (array 3 1 5 3 9 6)
    (array 7 2 4 2 7 3)
)
