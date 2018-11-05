; 8) Crea un programa mediante reglas que resuelva la intersección de dos hechos
; multicampo.

(deffunction append(?a $?vector)
    (if (not(multifieldp $?vector)) then
        (printout t "La segunda variable tiene que ser un multicampo." crlf)
        (return)
    )
    (return (insert$ $?vector (+ 1 (length $?vector)) ?a))
)

(deffunction intersection(?a $?b)
    (bind $?res (create$))
    (foreach ?val_a $?a
        (if (member$ ?val_a $?b)
        then (bind $?res (append ?val_a $?res))
        )
    )
    (foreach ?val_b $?b
        (if (member$ ?val_b $?a)
        then (bind $?res (append ?val_b $?res))
        )
    )
    (return $?res)
)

(defrule intersect
    ?f1 <- (array $?a1)
    ?f2 <- (array $?a2)
    (test (not (eq ?f1 ?f2)))
    =>
    (assert (interseccion (intersection $?a1 $?a2)))
)

(deffacts init
    (array a b c d e)
    (array g f c b a)
)
