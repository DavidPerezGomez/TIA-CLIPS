(defglobal ?*ultimo_id* = 0)
(defglobal ?*objetivo* = 2)
(defglobal ?*camino* = (create$))

; añade valor princpio de una variable multicampo
(deffunction prepend(?a $?vector)
    (if (not(multifieldp $?vector)) then
        (printout t "La segunda variable tiene que ser un multicampo." crlf)
        (return)
    )
    (return (insert$ $?vector 1 ?a))
)

; incrementa el contador global en 1 y devuelve el nuevo valor
(deffunction update_global_id()
    (bind ?*ultimo_id* (+ ?*ultimo_id* 1))
    (return ?*ultimo_id*)
)

; devuelve la cantidad de liquido desplazada al verter
; ?cant1 litros en una jarra de ?cap2 litros que ya tiene ?cant2 dentro
; ej.: (vertido 4 3 1) -> 2
; Verter 4 litros en una jarra de 3 que ya tiene 1 litro.
; Solo se desplazan 2 litros.
(deffunction vertido(?cant1 ?cap2 ?cant2)
    (return (min ?cant1 (- ?cap2 ?cant2)))
)

; representa un estado concreto (con identificador y estado anterior)
; de la dos jarras
(deftemplate estado
    (slot id (type INTEGER))
    (slot id_anterior (type INTEGER))
    (slot movimiento (type STRING))
    (slot capacidad_j1 (type INTEGER))
    (slot cantidad_j1 (type INTEGER))
    (slot capacidad_j2 (type INTEGER))
    (slot cantidad_j2 (type INTEGER))
)

; representa un estado genérico de las dos jarras para evitar duplicados
(deftemplate creado
    (slot capacidad_j1 (type INTEGER))
    (slot cantidad_j1 (type INTEGER))
    (slot capacidad_j2 (type INTEGER))
    (slot cantidad_j2 (type INTEGER))
)

; inicialización
(defrule init
    (declare (salience 50))
    (initial-fact)
    =>
    ; (set-strategy depth)
    (set-strategy breadth)
)

; recorrer el camino marcha atrás guardando los movimientos.
; al llegar a final, se imprimen los movimientos recogidos
(defrule backtrack
    (declare (salience 30))
    ?fin <- (fin ?id_fin)
    (estado
        (id ?id)
        (id_anterior ?id_ant)
        (cantidad_j1 ?cant1)
        (cantidad_j2 ?cant2)
        (movimiento ?mov)
    )
    (test (eq ?id_fin ?id))
    =>
    (if (not (eq ?id ?id_ant))
    ; queda más camino por recorrer
    then (bind ?*camino* (prepend (str-cat ?mov " (" ?cant1 " | " ?cant2 ")" ) ?*camino*))
         (retract ?fin)
         (assert (fin ?id_ant))
    ; final del camino
    else (foreach ?m ?*camino*
            (printout t ?m crlf)
        )
        (halt)
    )
)

; comprobar si algún estado se corresponde con un estado final
(defrule comprobar_fin
    (declare (salience 20))
    (estado
        (id ?id)
        (cantidad_j1 ?cant1)
    )
    (test (eq ?cant1 ?*objetivo*))
    =>
    (assert (fin ?id))
)

(defrule llenar_j1
    (declare (salience 10))
    (estado
        (id ?id)
        (capacidad_j1 ?cap1)
        (cantidad_j1 ?cant1)
        (capacidad_j2 ?cap2)
        (cantidad_j2 ?cant2)
    )
    (test (< ?cant1 ?cap1))
    =>
    (if (not (eq (assert
                    (creado
                        (capacidad_j1 ?cap1)
                        (cantidad_j1 ?cap1)
                        (capacidad_j2 ?cap2)
                        (cantidad_j2 ?cant2)
                    )) FALSE)) then
        (bind ?new_id (update_global_id))
        (assert
            (estado
                (id ?new_id)
                (id_anterior ?id)
                (movimiento "Llenar J1")
                (capacidad_j1 ?cap1)
                (cantidad_j1 ?cap1)
                (capacidad_j2 ?cap2)
                (cantidad_j2 ?cant2)
            )
        )
    )
)

(defrule llenar_j2
    (declare (salience 10))
    (estado
        (id ?id)
        (capacidad_j1 ?cap1)
        (cantidad_j1 ?cant1)
        (capacidad_j2 ?cap2)
        (cantidad_j2 ?cant2)
    )
    (test (< ?cant2 ?cap2))
    =>
    (if (not (eq (assert
                    (creado
                        (capacidad_j1 ?cap1)
                        (cantidad_j1 ?cant1)
                        (capacidad_j2 ?cap2)
                        (cantidad_j2 ?cap2)
                    )) FALSE)) then
        (bind ?new_id (update_global_id))
        (assert
            (estado
                (id ?new_id)
                (id_anterior ?id)
                (movimiento "Llenar J2")
                (capacidad_j1 ?cap1)
                (cantidad_j1 ?cant1)
                (capacidad_j2 ?cap2)
                (cantidad_j2 ?cap2)
            )
        )
    )
)

(defrule vaciar_j1
    (declare (salience 10))
    (estado
        (id ?id)
        (capacidad_j1 ?cap1)
        (cantidad_j1 ?cant1)
        (capacidad_j2 ?cap2)
        (cantidad_j2 ?cant2)
    )
    (test (> ?cant1 0))
    =>
    (if (not (eq (assert
                    (creado
                        (capacidad_j1 ?cap1)
                        (cantidad_j1 0)
                        (capacidad_j2 ?cap2)
                        (cantidad_j2 ?cant2)
                    )) FALSE)) then
        (bind ?new_id (update_global_id))
        (assert
            (estado
                (id ?new_id)
                (id_anterior ?id)
                (movimiento "Vaciar J1")
                (capacidad_j1 ?cap1)
                (cantidad_j1 0)
                (capacidad_j2 ?cap2)
                (cantidad_j2 ?cant2)
            )
        )
    )
)

(defrule vaciar_j2
    (declare (salience 10))
    (estado
        (id ?id)
        (capacidad_j1 ?cap1)
        (cantidad_j1 ?cant1)
        (capacidad_j2 ?cap2)
        (cantidad_j2 ?cant2)
    )
    (test (> ?cant2 0))
    =>
    (if (not (eq (assert
                    (creado
                        (capacidad_j1 ?cap1)
                        (cantidad_j1 ?cant1)
                        (capacidad_j2 ?cap2)
                        (cantidad_j2 0)
                    )) FALSE)) then
        (bind ?new_id (update_global_id))
        (assert
            (estado
                (id ?new_id)
                (id_anterior ?id)
                (movimiento "Vaciar J2")
                (capacidad_j1 ?cap1)
                (cantidad_j1 ?cant1)
                (capacidad_j2 ?cap2)
                (cantidad_j2 0)
            )
        )
    )
)

(defrule verter_j1_en_j2
    (declare (salience 10))
    (estado
        (id ?id)
        (capacidad_j1 ?cap1)
        (cantidad_j1 ?cant1)
        (capacidad_j2 ?cap2)
        (cantidad_j2 ?cant2)
    )
    (test (and (>    ?cant1 0) (< ?cant2 ?cap2)))
    =>
    (bind ?vertido (vertido ?cant1 ?cap2 ?cant2))
    (if (not (eq (assert
                    (creado
                        (capacidad_j1 ?cap1)
                        (cantidad_j1 (- ?cant1 ?vertido))
                        (capacidad_j2 ?cap2)
                        (cantidad_j2 (+ ?cant2 ?vertido))
                    )) FALSE)) then
        (bind ?new_id (update_global_id))
        (assert
            (estado
                (id ?new_id)
                (id_anterior ?id)
                (movimiento "Verter J1 en J2")
                (capacidad_j1 ?cap1)
                (cantidad_j1 (- ?cant1 ?vertido))
                (capacidad_j2 ?cap2)
                (cantidad_j2 (+ ?cant2 ?vertido))
            )
        )
    )
)

(defrule verter_j2_en_j1
    (declare (salience 10))
    (estado
        (id ?id)
        (capacidad_j1 ?cap1)
        (cantidad_j1 ?cant1)
        (capacidad_j2 ?cap2)
        (cantidad_j2 ?cant2)
    )
    (test (and (> ?cant2 0) (< ?cant1 ?cap1)))
    =>
    (bind ?vertido (vertido ?cant2 ?cap1 ?cant1))
    (if (not (eq (assert
                    (creado
                        (capacidad_j1 ?cap1)
                        (cantidad_j1 (+ ?cant1 ?vertido))
                        (capacidad_j2 ?cap2)
                        (cantidad_j2 (- ?cant2 ?vertido))
                    )) FALSE)) then
        (bind ?new_id (update_global_id))
        (assert
            (estado
                (id ?new_id)
                (id_anterior ?id)
                (movimiento "Verter J2 en J1")
                (capacidad_j1 ?cap1)
                (cantidad_j1 (+ ?cant1 ?vertido))
                (capacidad_j2 ?cap2)
                (cantidad_j2 (- ?cant2 ?vertido))
            )
        )
    )
)

(defrule no_solucion
    (declare (salience 0))
    =>
    (printout t "No hay solución" crlf)
)

(deffacts init
    (estado
        (id 0)
        (id_anterior 0)
        (capacidad_j1 34)
        (cantidad_j1 0)
        (capacidad_j2 13)
        (cantidad_j2 0)
    )
    (creado
        (capacidad_j1 34)
        (cantidad_j1 0)
        (capacidad_j2 13)
        (cantidad_j2 0)
    )
)
