; se activa si se ha introducido un input incorrecto
(defrule error_input
  (declare (salience 20))
  (error_input)
=>
  (printout t "No se ha introducido un numero entero positivo." crlf)
)

; pide el numero del que se va a hacer el sumatorio
(defrule pedir_num
  (declare (salience 10))
  (not(actual ? ?))
=>
  (printout t "Escribe el numero para el sumatorio: ")
  (bind ?obj (read))
  ; comprueba que el numero es correcto
  (if
    (or
      (not (integerp ?obj))
      (< ?obj 0)
    )
  then
    (assert (error_input)) ; indica que ha habido un error para que pare el programa
  else
    ; si el numero es correcto hay que diferenciar si se ha introducido un 0 o > 0
    (if (eq ?obj 0)
    then
      (assert (actual ?obj 0))
    else
      (assert (actual (- ?obj 1) 0))
    )

  )
)

; acumula el numero sumado y lo muestra
(defrule sumar_num
  (declare (salience 0))
  ?f <- (actual ?num ?acc)
  (not(finalizado))
=>
  (if (neq ?num 0) then
    (retract ?f)
    (assert (actual (- ?num 1) (+ ?acc ?num)))
  else
    (assert (finalizado))
    (assert (sumatorio ?acc))
  )
)
