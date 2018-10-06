; Representación estado (para 2 habiationes): axby
; a: 0 -> primera habitación limpia; 1 -> primera habitación sucia
; x: 0 -> primera habitación no tiene aspiradora; 1 -> primera habitación tiene aspiradora
; b: 0 -> segunda habitación limpia; 1 -> segunda habitación sucia
; y: 0 -> segunda habitación no tiene aspiradora; 1 -> segunda habitación tiene aspiradora
; Un estado es correcto si y solo si x + y = 1
; Un estado es final si y solo si a + b = 0

; En el algoritmo de búsqueda, cada estado se almacena junto con sus estados anteriores
; "axby a2x2b2y2 a3x3b3y3..." donde a3x3b3y3 es padre de a2x2b2y2 que es padre de axby

; Método principal: (aspiradora)

; global variable to save/check how long a single state string (ignoring parents) should be
; i.e.: with two rooms, states are 4 chars. long ("0110", "1001", "1110", etc.)
; added mostly in case we need to add more rooms
(defglobal ?*state_ln* = 0)

; appends element ?a to the end of multifield $?vector
(deffunction append(?a $?vector)
    (if (not(multifieldp $?vector)) then
        (printout t "La segunda variable tiene que ser un multicampo." crlf)
        (return)
    )
    (return (insert$ $?vector (+ 1 (length $?vector)) ?a))
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

; given a state, prints on screen each of the state's parents, oldest to newest
; and then the state itself
; i.e.: (print_res "0001 0011 0110 1110")
; > 1110
; > 0110
; > 0011
; > 0001
(deffunction print_res(?state)
    (bind ?i (length ?state))
    (while (> ?i 0)
        (printout t (sub-string (- ?i ?*state_ln* -1) ?i ?state) crlf)
        (bind ?i (- ?i ?*state_ln* 1))
    )
)

; returns TRUE if the given state is legal (correct). FALSE otherwise
(deffunction is_legal(?state)
    (bind ?leaf (sub-string 1 ?*state_ln* ?state))
    (bind ?x (string-to-field(sub-string 2 2 ?leaf)))
    (bind ?y (string-to-field(sub-string 4 4 ?leaf)))
    (return (= 1 (+ ?x ?y)))
)

; returns TRUE if the given state is final. FALSE otherwise
(deffunction is_final(?state)
    (bind ?leaf (sub-string 1 ?*state_ln* ?state))
    (bind ?a (string-to-field(sub-string 1 1 ?leaf)))
    (bind ?b (string-to-field(sub-string 3 3 ?leaf)))
    (return (= 0 (+ ?a ?b)))
)

; returns resulting state after moving left
; always leaves the vaccum clenaer on the left room
(deffunction move_left(?state)
    (bind ?res (sub-string 1 1 ?state))
    (bind ?res (str-cat ?res "1"))
    (bind ?res (str-cat ?res (sub-string 3 3 ?state)))
    (bind ?res (str-cat ?res "0"))
    (return ?res)
)

; returns resulting state after moving right
; always leaves the vaccum clenaer on the right room
(deffunction move_right(?state)
    (bind ?res (sub-string 1 1 ?state))
    (bind ?res (str-cat ?res "0"))
    (bind ?res (str-cat ?res (sub-string 3 3 ?state)))
    (bind ?res (str-cat ?res "1"))
    (return ?res)
)

; returns resulting state after using the vacuum creaner to clean the room it currently is in.
(deffunction clean(?state)
    (if (= 1 (string-to-field (sub-string 2 2 ?state)))
    then
    ; vaccum is in left room
        (bind ?res "01")
        (bind ?res (str-cat ?res (sub-string 3 3 ?state)))
        (bind ?res (str-cat ?res "0"))
        (return ?res)
    else
    ; vaccum is in right room
        (bind ?res (sub-string 1 1 ?state))
        (bind ?res (str-cat ?res "0"))
        (bind ?res (str-cat ?res "01"))
        (return ?res)
    )
)

; returns a multifield with all the possible children states
; children states returned have already have its parents history appended
; i.e.: (get_children "1100 1001")
; > ("1100 1100 1001" "1001 1100 1001" "0100 1100 1001")
(deffunction get_children(?state)
    (bind ?leaf (sub-string 1 ?*state_ln* ?state))
    (bind $?res (create$))
    (bind $?res (append (str-cat (move_left ?leaf) " " ?state) $?res))
    (bind $?res (append (str-cat (move_right ?leaf) " " ?state) $?res))
    (bind $?res (append (str-cat (clean ?leaf) " " ?state) $?res))
    (return $?res)
)

; main method
(deffunction aspiradora(?initial)
    ; set global variable
    (bind ?*state_ln* (length ?initial))
    (if (not (is_legal ?initial))
    then (printout t "El estado no es válido." crlf)
         (return)
    )
    (if (is_final ?initial)
    then (print_res ?initial)
         (return)
    )
    ; initialize lists of visited states and states witing to be inspected
    (bind ?visited (create$))
    (bind ?queue (create$ ?initial))
    (bind ?current ?initial)
    ; loop until finished
    (while (not (is_final ?current))
        ; add current state (independent of parents) to visited list
        (bind ?visited (append (sub-string 1 ?*state_ln* ?current) ?visited))
        ; pop current state from the queue
        (bind ?queue (rest$ ?queue))
        ; find children states
        (bind ?children (get_children ?current))
        (foreach ?child ?children
            ; add non-visited chil states to queue
            (if (not(in (sub-string 1 ?*state_ln* ?child) ?visited))
            then (bind ?queue (append ?child ?queue))
            )
        )
        ; advance to next state in the queue
        (bind ?current (nth$ 1 ?queue))
    )
    (print_res ?current)
    (return)
)
