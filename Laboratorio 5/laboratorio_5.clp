; Representación estado: glop
; g: 1 -> el granjero está en la primera orilla; 0 -> el granjero está en la segunda orilla
; l: 1 -> la lechuga está en la primera orilla; 0 -> la lechuga está en la segunda orilla
; o: 1 -> la oveja (cabra) está en la primera orilla; 0 -> la oveja (cabra) está en la segunda orilla
; p: 1 -> el puma está en la primera orilla; 0 -> el puma está en la segunda orilla
; Un estado es incorrecto si (l == o != g) ó (p == o != g)
; Un estado es incorrecto si la la oveja está en la misma orila que el puma o que la lechuga y en distinta que la del granjero
; Un estado es final si y solo si glop == 0000

; Los movimientos se representan mediante números de la siguiente manera:
; 0 -> "mover granjero"
; 1 -> "mover granjero y lechuga"
; 2 -> "mover granjero y oveja"
; 3 -> "mover granjero y puma"

; En el algoritmo de búsqueda, cada estado se almacena junto con sus estados anteriores
; "glop m2g2l2o2p2 m3g3l3o3p3 ..." donde g3l3o3p3 es padre de g2l2o2p2 que es padre de glop
; y m2 es el movimiento realizado para pasar de g2l2o2p2 a glop

; Método principal: (glop)

(defglobal ?*state_ln* = 4)

; appends element ?a to the end of multifield $?vector
(deffunction append(?a $?vector)
    (if (not(multifieldp $?vector)) then
        (printout t "La segunda variable tiene que ser un multicampo." crlf)
        (return)
    )
    (return (insert$ $?vector (+ 1 (length $?vector)) ?a))
)

; inserts element ?a at the start of multifield $?vector
(deffunction prepend(?a $?vector)
    (if (not(multifieldp $?vector)) then
        (printout t "La segunda variable tiene que ser un multicampo." crlf)
        (return)
    )
    (return (insert$ $?vector 1 ?a))
)

; returns TRUE if ?item is in $?vector
(deffunction in(?item $?vector)
    (if (not(multifieldp $?vector)) then
        (printout t "La segunda variable tiene que ser un multicampo." crlf)
        (return)
        )
    (if (member$ ?item $?vector) then
        (return TRUE)
        else (return FALSE)
    )
)

; devuelve TRUE si el estado es un fin del juego (el puma se come a la oveja o la oveja se come la lechuga)
(deffunction is_game_over(?state)
    (bind ?leaf (sub-string 1 ?*state_ln* ?state))
    (bind ?g (string-to-field(sub-string 1 1 ?leaf)))
    (bind ?l (string-to-field(sub-string 2 2 ?leaf)))
    (bind ?o (string-to-field(sub-string 3 3 ?leaf)))
    (bind ?p (string-to-field(sub-string 4 4 ?leaf)))
    (return (and (not (= ?g ?o))
                 (or (= ?o ?l)
                     (= ?o ?p)
                 )
            )
    )
)

; devuelve TRUE si el estado representa el final del juego.
; (todos los personajes han llegado a la otra orilla)
(deffunction is_final(?state)
    (bind ?leaf (sub-string 1 ?*state_ln* ?state))
    (return (= 0 (string-to-field ?leaf)))
)

; devuelve una string describiendo el movimiento correspondiente al número ?mov
(deffunction mov_to_string(?mov)
    (switch (string-to-field ?mov)
        (case 0 then
            (return "mover granjero"))
        (case 1 then
            (return "mover granjero y lechuga"))
        (case 2 then
            (return "mover granjero y oveja"))
        (case 3 then
            (return "mover granjero y puma"))
        (default then
            (return "movimiento no reconocido"))
    )
)


; dado un estado, excribe por pantalla cada uno de sus antecesores, de más
; antiguo a más nuevo, y por último, el propio estado.
; después de cada estado intermedio, escribe también la acción realizada
; para llegar al siguiente estado
; p.e.: (print_res "0000 21010 00010")
; > 0010 -> mover granjero
; > 1010 -> mover granjero y oveja
; > 0000
(deffunction print_res(?state)
    (bind ?i (length ?state))
    (bind $?vector (create$))
    (while (> ?i 0)
        (bind $?vector (append (sub-string (- ?i (+ ?*state_ln* 1) -1) ?i ?state) ?vector))
        (bind ?i (- ?i (+ ?*state_ln* 3) -1))
    )
    (bind ?state_aux_1 "")
    (bind ?state_aux_2 "")
    (bind ?i 1)
    (while (<= ?i (length$ $?vector))
        (bind ?state (nth$ ?i $?vector))
        (if (not (= ?i (length$ $?vector)))
        then (bind ?str (sub-string 2 5 ?state))
             (bind ?str (str-cat ?str " -> " (mov_to_string (sub-string 1 1 ?state))))
        else (bind ?str (sub-string 1 4 ?state))
        )
        (bind ?i (+ ?i 1))
        (printout t ?str crlf)
    )
    (return FALSE)
)

; aplica el movimiento ?mov al estado ?state y devuelve el estado resultante.
; en caso de intentar aplicar un movimiento imposible, devuelve FALSE
; p.e.: (apply_movement "0101" 0)
; > "1101"
(deffunction apply_movement(?state ?mov)
    (bind ?res "")
    (bind ?leaf (sub-string 1 ?*state_ln* ?state))
    (bind ?g (string-to-field(sub-string 1 1 ?leaf)))
    (bind ?l (string-to-field(sub-string 2 2 ?leaf)))
    (bind ?o (string-to-field(sub-string 3 3 ?leaf)))
    (bind ?p (string-to-field(sub-string 4 4 ?leaf)))
    (switch ?mov
        (case 0 then
            ; mover granjero
            (if (= 0 ?g)
            then (bind ?res (str-cat 1 ?l ?o ?p))
            else (bind ?res (str-cat 0 ?l ?o ?p))
            )
        )
        (case 1 then
            ; mover granjero y lechuga
            (if (not (= ?g ?l))
            then (return FALSE)
            )
            (if (= 0 ?g)
            then (bind ?res (str-cat 1 1 ?o ?p))
            else (bind ?res (str-cat 0 0 ?o ?p))
            )
        )
        (case 2 then
            ; mover granjero y oveja
            (if (not (= ?g ?o))
            then (return FALSE)
            )
            (if (= 0 ?g)
            then (bind ?res (str-cat 1 ?l 1 ?p))
            else (bind ?res (str-cat 0 ?l 0 ?p))
            )
        )
        (case 3 then
            ; mover granjero y puma
            (if (not (= ?g ?p))
            then (return FALSE)
            )
            (if (= 0 ?g)
            then (bind ?res (str-cat 1 ?l ?o 1))
            else (bind ?res (str-cat 0 ?l ?o 0))
            )
        )
        (default then
            (printout t "movimiento desconocido" crlf)
            (return FALSE)
        )
    )
    (return ?res)
)

; returns a multifield with all the possible children states
; children states returned have already have its parents history appended
; i.e.: (get_children "1100 1001")
; > ("1100 1100 1001" "1001 1100 1001" "0100 1100 1001")

; devuelve un multicampo con todos los posibles hijos del estado ?state
; p.e.: (get_children "0101 21111")
; > ("1101 00101 21111" "1111 20101 21111")
(deffunction get_children(?state)
    (bind ?leaf (sub-string 1 ?*state_ln* ?state))
    (bind $?res (create$))
    (loop-for-count (?i 0 3)
        (bind ?child (apply_movement ?state ?i))
        (if (and ?child (not (is_game_over ?child)))
        then (bind $?res (append (str-cat ?child " " ?i ?state) $?res))
        )
    )
    (return $?res)
)

(deffunction glop(?initial)
    ; set global variable
    (bind ?*state_ln* (length ?initial))
    (if (is_game_over ?initial)
    then (printout t "El estado no es válido." crlf)
         (return)
    )
    ; (if (is_final ?initial)
    ; then (print_res ?initial)
    ;      (return)
    ; )
    ; initialize lists of visited states and states witing to be inspected
    (bind ?visited (create$))
    (bind ?stack (create$ ?initial))
    (bind ?current ?initial)
    ; loop until finished
    (while (not (is_final ?current))
        ; add current state (independent of parents) to visited list
        (bind ?visited (append (sub-string 1 ?*state_ln* ?current) ?visited))
        ; pop current state from the stack
        (bind ?stack (rest$ ?stack))
        ; find children states
        (bind ?children (get_children ?current))
        (foreach ?child ?children
            ; add non-visited children states to the stack
            (if (not (in (sub-string 1 ?*state_ln* ?child) ?visited))
            then (bind ?stack (prepend ?child ?stack))
            )
        )
        ; advance to next state in the queue
        (bind ?current (nth$ 1 ?stack))
    )
    (print_res ?current)
    (return)
)
