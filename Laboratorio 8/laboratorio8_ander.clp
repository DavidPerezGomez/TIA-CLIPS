; Este problema represenda una lista del 0 al 10, empezamos en el estado inicial 3
; sus adyacentes serían el 2 y el 4

(defglobal
   ?*ID_COUNTER* = 1
   ?*CAMINO* = ""
)

(deftemplate contador
  (slot num (type INTEGER))
)

(deftemplate estado
  (slot op (type LEXEME))
  (slot id (type INTEGER))
  (slot id_padre (type INTEGER))
  (slot j4 (type INTEGER))
  (slot j3 (type INTEGER))
)

(deftemplate por_visitar
  (slot op (type LEXEME))
  (slot id_padre (type INTEGER))
  (slot j4 (type INTEGER))
  (slot j3 (type INTEGER))
)

(deftemplate visitado
  (slot j4 (type INTEGER))
  (slot j3 (type INTEGER))
)

(deffacts estado_inicial
  (por_visitar (op Z) (id_padre 0) (j4 0) (j3 0))
  (contador (num 0))
)

; inicializacion del programa
(defrule inicializacion
  (declare (salience 100))
  (not (init))
=>
  (printout t "Inicializacion hecha." crlf)
  (set-strategy breadth)
  ; (printout t ?*id_counter* crlf)
  (assert(init))
)

; Devuelve los estados adyacentes
(deffunction get_adyacentes (?j4 ?j3)
  (bind ?result "")
; 1. Llenar la jarra de 4 litros completamente (para ello, la jarra de 4 litros no debe
; estar completamente llena).
  (if (< ?j4 4) then
    (bind ?result (str-cat ?result "A" 4 ?j3 " "))
  )
; 2. Llenar la jarra de 3 litros completamente (para ello, la jarra de 3 litros no debe
; estar completamente llena).
  (if (< ?j3 3) then
    (bind ?result (str-cat ?result "B" ?j4 3 " "))
  )
; 3. Vaciar la jarra de 4 litros (para ello, la jarra debe contener algo de liquido).
  (if (> ?j4 0) then
    (bind ?result (str-cat ?result "C" 0 ?j3 " "))
  )
; 4. Vaciar la jarra de 3 litros (para ello, la jarra debe contener algo de liquido).
  (if (> ?j3 0) then
    (bind ?result (str-cat ?result "D" ?j4 0 " "))
  )
; 5. Verter el contenido de la jarra de 4 litros en la jarra de 3 litros (para ello, la
; jarra de 4 litros debe contener algo de liquido y la de 3 litros no estar
; completamente llena).
  (if (and (> ?j4 0) (< ?j3 3)) then
    (bind ?librej3 (- 3 ?j3))
    (if (>= ?j4 ?librej3) then
        (bind ?resultj4 (- ?j4 ?librej3))
        (bind ?resultj3 (+ ?j3 ?librej3))
    else
        (bind ?resultj4 0)
        (bind ?resultj3 (+ ?j3 ?j4))
    )
    (bind ?result (str-cat ?result "E" ?resultj4 ?resultj3 " "))
  )
; 6. Verter el contenido de la jarra de 3 litros en la jarra de 4 litros (para ello, la
; jarra de 3 litros debe contener algo de liquido y la de 4 litros no estar
; completamente llena).
  (if (and (> ?j3 0) (< ?j4 4)) then
    (bind ?librej4 (- 4 ?j4))
    (if (>= ?j3 ?librej4) then
        (bind ?resultj3 (- ?j3 ?librej4))
        (bind ?resultj4 (+ ?j4 ?librej4))
    else
        (bind ?resultj3 0)
        (bind ?resultj4 (+ ?j4 ?j3))
    )
    (bind ?result (str-cat ?result "F" ?resultj4 ?resultj3 " "))
  )

  (return (explode$ ?result))
)

(defrule hay_por_visitar
  ?f <- (por_visitar (op ?op_actual) (id_padre ?id_padre_aux) (j4 ?j4_aux) (j3 ?j3_aux))
  (not(solucion_encontrada))
  ?c <- (contador (num ?num_cont))
=>
  ; TODO el mismo id counter para el hecho inicial no se si ira bien
  (bind ?id_actual ?*ID_COUNTER*)
  (assert (estado (op ?op_actual) (id ?id_actual) (id_padre ?id_padre_aux) (j4 ?j4_aux) (j3 ?j3_aux)))
  (bind ?*ID_COUNTER* (+ ?*ID_COUNTER* 1))
  (bind ?num_cont (+ ?num_cont 1))
  (bind ?c (modify ?c (num ?num_cont)))
  (if (eq ?j4_aux 2) then
    (assert (solucion_encontrada))
    (return)
  )

  (assert (visitado (j4 ?j4_aux) (j3 ?j3_aux)))
  (bind ?adyacentes (get_adyacentes ?j4_aux ?j3_aux))
  (foreach ?item ?adyacentes
    (bind ?letter (sub-string 1 1 ?item))
    (bind ?j4 (string-to-field (sub-string 2 2 ?item)))
    (bind ?j3 (string-to-field (sub-string 3 3 ?item)))
    (if (not(eq (assert (visitado (j4 ?j4) (j3 ?j3))) FALSE)) then
      (assert (por_visitar (op ?letter) (id_padre ?id_actual) (j4 ?j4) (j3 ?j3)))
      (assert (estado (op ?letter) (id ?*ID_COUNTER*) (id_padre ?id_actual) (j4 ?j4_aux) (j3 ?j3_aux)))
      (bind ?*ID_COUNTER* (+ ?*ID_COUNTER* 1))
      (bind ?num_cont (+ ?num_cont 1))
      (bind ?c (modify ?c (num ?num_cont)))
    )
    ; (bind ?*ID_COUNTER* (+ ?*ID_COUNTER* 1))
  )
  (retract ?f)
)

; si no hay solucion avisamos
(defrule no_hay_por_visitar
  (not (por_visitar (op ?) (j4 ?) (j3 ?)))
=>
  (printout t "No se ha encontrado la solucion" crlf)
)

(defrule construir_solucion
  (solucion_encontrada)
  ?f <- (estado (op ?op) (id ?id_actual) (id_padre ?id_padre) (j4 ?j4) (j3 ?j3))
  ?c <- (contador (num ?num_cont))
  (test (eq ?num_cont ?id_actual))
=>
  (if (eq ?id_padre 0) then
    (assert (mostrar_solucion))
    (printout t "#### SOLUCION ####" crlf)
  )
  ; (printout t "creando camino" crlf)
  (retract ?f)
  (modify ?c (num ?id_padre))
  (bind ?*CAMINO* (str-cat ?*CAMINO* ?op ?j4 ?j3 " "))
)

(deffunction traducir_letra_operacion (?letra)
    (bind ?result "")
    (switch (str-cat ?letra)
        ;str-cat el input para tener en cuenta símbolos también
        (case "A" then
          (bind ?result "Llenar j4"))
        (case "B" then
          (bind ?result "Llenar j3"))
        (case "C" then
          (bind ?result "Vaciar j4"))
        (case "D" then
          (bind ?result "Vaciar j3"))
        (case "E" then
          (bind ?result "Verter j4 en j3"))
        (case "F" then
          (bind ?result "Verter j3 en j4"))
        (case "Z" then
          (bind ?result "Estado inicial"))
    )
    (return ?result)
)

(defrule print_solucion
  (mostrar_solucion)
=>
  (bind ?camino (explode$ ?*CAMINO*))
  (bind ?i (length$ ?camino))
  (while (>= ?i 1)
    (bind ?item (nth ?i ?camino))
    (bind ?i (- ?i 1))
    (bind ?letra (sub-string 1 1 ?item))
    (bind ?j4 (string-to-field (sub-string 2 2 ?item)))
    (bind ?j3 (string-to-field (sub-string 3 3 ?item)))
    (bind ?op (traducir_letra_operacion ?letra))
    (printout t ?op ": " crlf)
    (printout t "j4:" ?j4 " j3:" ?j3 crlf)
    (printout t "" crlf)
  )
)
