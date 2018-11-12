; Este problema represenda una lista del 0 al 10, empezamos en el estado inicial 3
; sus adyacentes serían el 2 y el 4

(deffacts estado_inicial
  (por_visitar 3)
)

; inicializacion del programa
(defrule inicializacion
  (declare (salience 100))
  (not (init))
=>
  (printout t "Inicializacion hecha." crlf)
  (set-strategy breadth)
  (assert(init))
)

; Devuelve los estados adyacentes
(deffunction get_adyacentes (?n)
    (bind ?left (- ?n 1))
    (bind ?right (+ ?n 1))
    (bind ?result (create$))

    (if (and (>= ?left 0) (<= ?left 10))then
      (bind ?result (insert$ ?result 1 ?left))
     )
    (if (and (>= ?right 0) (<= ?right 10))then
      (bind ?result (insert$ ?result 1 ?right))
     )
    (return ?result)
)

; True si es estado final, si no False
(deffunction es_estado_final (?n)
  (bind ?objetivo -7)  ; TODO el objetivo igual en la inicializacion sería mejor
  (return (eq ?n ?objetivo))
)

(defrule hay_por_visitar
  ?f <- (por_visitar ?n)
  (not(solucion_encontrada ?))
=>
  (if (es_estado_final ?n) then
    (assert (solucion_encontrada ?n))
  else
    (bind ?adyacentes (get_adyacentes ?n))
    (foreach ?item ?adyacentes
      (if (not(eq (assert (visitado ?item)) FALSE)) then
        (assert (por_visitar ?item))
      )
    )
  )
  (retract ?f)
)

; si no hay solucion avisamos
(defrule no_hay_por_visitar
  (not (por_visitar ?))
=>
  (printout t "No se ha encontrado la solucion" crlf)
)
