;0
(deffunction isValidNumber(?a)
    (or (integerp ?a) (floatp ?a))
)

(deffunction isNaturalNumber(?a)
    (and (integerp ?a) (>= ?a 0))
)

;1
(deffunction cuentaAtras(?a)
    (if (not (integerp ?a)) then
        (printout t "El parámetro no es correcto." crlf)
        (return)
    )

    (while (> ?a 0)
        (printout t ?a " ")
        (bind ?a (- ?a 1))
     )
     (printout t crlf)
     (return)
)

;2
(deffunction hasta(?a)
    ;funciona con negativos
    (if (not (integerp ?a)) then
        (printout t "El parámetro no es correcto." crlf)
        (return)
    )

    (bind ?cont 0)
    (if (> ?a 0)
    then (bind ?top ?a)
    else (bind ?top (- ?a 2))
    )
    (while (not (= ?cont ?top))
        (printout t ?cont " ")
        (if (> ?a 0)
        then (bind ?cont (+ ?cont 1))
        else (bind ?cont (- ?cont 1))
        )
     )
     (printout t crlf)
)

;3 iteración
(deffunction sumatorioIter(?a)
    (if (not (isNaturalNumber ?a)) then
        (printout t "El parámetro no es correcto." crlf)
        (return)
    )

    (bind ?acc 0)
    (loop-for-count (?i 1 ?a)
        (bind ?acc (+ ?acc ?i))
    )
    (return ?acc)
)

;3 recursión
(deffunction sumatorioRec(?a)
    (if (not (isNaturalNumber ?a)) then
        (printout t "El parámetro no es correcto." crlf)
        (return)
    )

    (if (= ?a 0)
    then (return 0)
    else (return (+ ?a (sumatorioRec(- ?a 1))))
    )
)

;4
(deffunction minimoMulti($?vector)
    ;utilizar nth$ en lugar de first$
    ;porque first$ devuelve un multicampo con un solo valor
    (bind ?min (nth$ 1 $?vector))
    (foreach ?i $?vector
        (if (isValidNumber ?i)
        then (if (< ?i ?min)
             then (bind ?min ?i)
             )
        else (printout t "Todos los valores deben ser números." crlf)
             (return)
        )
    )
    (return ?min)
)

;5
(deffunction maximoMulti($?vector)
    ;utilizar nth$ en lugar de first$
    ;porque first$ devuelve un multicampo con un solo valor
    (bind ?max (nth$ 1 $?vector))
    (foreach ?i $?vector
        (if (isValidNumber ?i)
        then (if (> ?i ?max)
             then (bind ?max ?i)
             )
        else (printout t "Todos los valores deben ser números." crlf)
             (return)
        )
    )
    (return ?max)
)

;6
(deffunction sumaMulti($?vector)
    ;utilizar nth$ en lugar de first$
    ;porque first$ devuelve un multicampo con un solo valor
    (bind ?acc 0)
    (foreach ?i $?vector
        (if (isValidNumber ?i)
        then (bind ?acc (+ ?acc ?i))
        else (printout t "Todos los valores deben ser números." crlf)
             (return)
        )
    )
    (return ?acc)
)

;7
(deffunction mediaMulti($?vector)
    ;utilizar nth$ en lugar de first$
    ;porque first$ devuelve un multicampo con un solo valor
    (bind ?sum (sumaMulti $?vector))
    (if (or (integerp ?sum) (floatp ?sum))
    then (return (/ ?sum (length$ $?vector)))
    else (return)
    )
)

;8
(deffunction switchin(?option ?vector)
    (switch ?option
        (case 1 then
            (return (maximoMulti ?vector)))
        (case 2 then
            (return (minimoMulti ?vector)))
        (default then
            (printout t "Input no válido." crlf)
            (return))
    )
)

;9
(deffunction multiplicarElemento(?mult ?vector)
    (bind ?result (create$))
    (foreach ?i $?vector
        (if (isValidNumber ?i)
        then (bind ?result (insert$ $?result (* ?mult ?i) (+ 1 (length $?result))))
        else (printout t "Todos los valores deben ser números." crlf)
             (return)
        )
    )
)
