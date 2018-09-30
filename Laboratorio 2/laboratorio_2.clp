;0
(deffunction isIntOrFloat (?n)
    (or (integerp ?n) (floatp ?n))
)

(deffunction isNaturalNumber(?a)
    (and (integerp ?a) (>= ?a 0))
)

;1)
(deffunction cuentaAtras (?n)
  (if (not (isNaturalNumber ?n))
    then (printout t "El numero no es valido." crlf)
    (return)
  )

  (while (not (= ?n 0))
    (printout t ?n)
    (bind ?n (- ?n 1))
  )
  (printout t crlf)
)


;2)
(deffunction hasta (?n)
  (if
    (or
      (not (integerp ?n))
      (<= ?n 0)
    )
    then (printout t "El numero no es valido." crlf)
    (return)
  )

  (bind ?contador 0)
  (while (not (= ?n ?contador))
    (printout t " " ?contador)
    (bind ?contador (+ ?contador 1))
  )
  (printout t crlf)
)

;3 iteración
(deffunction sumatorio(?a)
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
(deffunction minimoMulti ($?n)
  (if (= 0 (length$ ?n)) then
    (printout t "Necesita parametros" crlf)
    (return)
  )

  (bind ?minimo (nth$ 1 ?n))

  (foreach ?item ?n
    (if (not (isIntOrFloat ?item)) then
      (printout t "Todos los elementos deben ser numeros." crlf)
      (return)
    )

    (if (< ?item ?minimo) then
      (bind ?minimo ?item)
    )
  )
  (return ?minimo)
)

;5
(deffunction maximoMulti ($?n)
  (if (= 0 (length$ ?n)) then
    (printout t "Necesita parametros" crlf)
    (return)
  )

  (bind ?maximo (nth$ 1 ?n))

  (foreach ?item ?n
    (if (not (isIntOrFloat ?item)) then
      (printout t "Todos los elementos deben ser numeros." crlf)
      (return)
    )

    (if (> ?item ?maximo) then
      (bind ?maximo ?item)
    )
  )
  (printout t ?maximo crlf)
)

;6
(deffunction sumaMulti ($?n)
  (if (= 0 (length$ ?n)) then
    (printout t "Necesita parametros" crlf)
    (return)
  )

  (bind ?resultado 0)

  (foreach ?item ?n
    (if (not (isIntOrFloat ?item)) then
      (printout t "Todos los elementos deben ser numeros." crlf)
      (return)
    )

    (bind ?resultado (+ ?resultado ?item))
  )
  (return ?resultado)
)


;7
(deffunction mediaMulti ($?n)
  ;la comprobacion de si esta vacio el multicampo y
  ;si todos son numeros los hace sumaMulti
  (/ (sumaMulti ?n) (length$ ?n))
)


;8
(deffunction switchin(?option ?vector)
    (switch ?option
        (case 1 then
            (return (maximoMulti ?vector)))
        (case 2 then
            (return (minimoMulti ?vector)))
        (default then
            (printout t "Input no valido." crlf)
            (return))
    )
)

;9
(deffunction multiplicarElemento(?mult ?vector)
    (bind ?result (create$))
    (foreach ?i $?vector
        (if (isIntOrFloat ?i)
        then (bind ?result (insert$ $?result (+ 1 (length $?result)) (* ?mult ?i)))
        else (printout t "Todos los valores deben ser numeros." crlf)
             (return)
        )
    )
    (return $?result)
)


;10 iteración
(deffunction factorialIter(?a)
    (if (not (isNaturalNumber ?a)) then
        (printout t "El parametro no es correcto." crlf)
        (return)
    )

    (if (= ?a 0)
    then (return 1))

    (bind ?acc 1)
    (loop-for-count (?i 1 ?a)
        (bind ?acc (* ?acc ?i))
    )
    (return ?acc)
)

;10 recursión
(deffunction factorialRec(?a)
    (if (not (isNaturalNumber ?a)) then
        (printout t "El parametro no es correcto." crlf)
        (return)
    )

    (if (= ?a 0)
    then (return 1)
    else (return (* ?a (factorialRec(- ?a 1))))
    )
)

;11
(deffunction fibonacci()
    (printout t "Introduce cuantos terminos quieres calcular: ")
    (bind ?a (read))
    (if (not (isNaturalNumber ?a)) then
        (printout t "El parámetro no es correcto." crlf)
        (return)
    )

    (bind ?x_prev 1)
    (bind ?x 0)
    (bind ?tmp 0)

    (loop-for-count (?i 0 (- ?a 1))
        (printout t ?x " ")
        (bind ?tmp ?x_prev)
        (bind ?x_prev ?x)
        (bind ?x (+ ?x ?tmp))
    )
    (printout t "" crlf)
)
