;0
(deffunction isValidNumber(?a)
    (integerp ?a)
)

(deffunction isIntOrFloat (?n)
    (or (integerp ?n) (floatp ?n))
)

;1)
(deffunction cuentaAtras (?n)
  (if
    (or
      (not (isValidNumber ?n))
      (<= ?n 0)
    )
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
      (not (isValidNumber ?n))
      (<= ?n 0)
    )
    then (printout t "El numero no es valido." crlf)
    (return)
  )

  (bind ?contador 0)
  (while (not (= ?n ?contador))
    (printout t ?contador)
    (bind ?contador (+ ?contador 1))
  )
  (printout t crlf)
)

;3
(deffunction sumatorio (?n)
  (if
    (or
      (not (isValidNumber ?n))
      (<= ?n 0)
    )
    then (printout t "El numero no es valido." crlf)
    (return)
  )

  (bind ?resultado 0)
  (while (not(= ?n 0))
    (bind ?resultado (+ ?resultado ?n))
    (bind ?n (- ?n 1))
  )
  (printout t ?resultado crlf)
)

(deffunction sumatorioRec (?n)

  (if (or (not(integerp ?n)) (< ?n 0)) then
    then (printout t "El numero no es valido." crlf)
    (return)
  )

  (if (= ?n 0) then
  (return ?n)
  else (return
            (+ ?n (sumatorioRec (- ?n 1)) )
        )
  )
)

;4
(deffunction minimoMulti ($?n)
  (bind ?loop_iniciado FALSE)
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
  (printout t ?minimo crlf)
)

;5
(deffunction maximoMulti ($?n)
  (bind ?loop_iniciado FALSE)
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
  (bind ?loop_iniciado FALSE)
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
(deffunction allValidNums ($?n)
  (foreach ?item ?n
    (if (not (isIntOrFloat ?item)) then
      (return FALSE)
    )
  )
  (return TRUE)
)


(deffunction mediaMulti ($?n)
  (if (allValidNums ?n) then
    (/ (sumaMulti ?n) (length$ ?n))
  else (printout t "Todos los elementos deben ser numeros." crlf)
  )
)

;8
(deffunction switchin (?opt ?n)
  (switch ?opt
    (case 1 then
      (maximoMulti ?n)
    )
    (case 2 then
      (minimoMulti ?n)
    )
    (default then
      (printout t "La opcion indicada tiene que ser 1 o 2." crlf)
    )
  )
)

;9
(deffunction multiplicarElemento (?opt $?vector)

  (if (or
        (not (isIntOrFloat ?opt))
        (not (allValidNums ?vector))
      ) then
    (printout t "Los numeros no son validos." crlf)
    (return)
  )

  (bind ?resultado (create$))


  (foreach ?item ?vector
    (bind ?resultado (insert$ ?resultado (+ (length$ ?resultado) 1) (* ?opt ?item)))
  )
  (return ?resultado)
)

;10
(deffunction factorial (?n)
  (if
    (or
      (not (isValidNumber ?n))
      (< ?n 0)
    )
    then (printout t "El numero no es valido." crlf)
    (return)
  )

  (if (= ?n 0) then
    (return 1)
  )

  (bind ?resultado 1)
  (while (not(= ?n 0))
    (bind ?resultado (* ?resultado ?n))
    (bind ?n (- ?n 1))
  )
  (printout t ?resultado crlf)
)

(deffunction factorialRec (?n)

  (if (or (not(integerp ?n)) (< ?n 0)) then
    then (printout t "El numero no es valido." crlf)
    (return)
  )

  (if (or (= ?n 0) (= ?n 1)) then
  (return 1))

  (if (> ?n 1) then
    (return
      (* ?n (factorialRec (- ?n 1)) )
    )
  )
)


;11
(deffunction fibonacci (?n)
  (if (or (not(integerp ?n)) (< ?n 0)) then
    then (printout t "El numero no es valido." crlf)
    (return)
  )

  (if (= ?n 0) then
  (return 0))

  (if (= ?n 1) then
  (return 1))

  (if (> ?n 1) then
    (return
      ; Fibonacci(x – 2) + Fibonacci(x – 1)
      (+ (fibonacci (- ?n 2)) (fibonacci (- ?n 1)))
    )
  )

)
