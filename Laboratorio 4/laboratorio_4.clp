; Representación estado (para 2 habiationes): axby
; a: 0 -> primera habitación limpia; 1 -> primera habitación sucia
; x: 0 -> primera habitación no tiene aspiradora; 1 -> primera habitación tiene aspiradora
; b: 0 -> segunda habitación limpia; 1 -> segunda habitación sucia
; y: 0 -> segunda habitación no tiene aspiradora; 1 -> segunda habitación tiene aspiradora
; Un estado es correcto sí y solo sí x + y =
; Un estado es final sí y solo sí a + b = 0

; En el algotritmo de búsqueda, cada estado se almacena junto con sus estados anteriores
; "axby a2x2b2y2 a3x3b3y3..." donde a3x3b3y3 es padre de a2x2b2y2 que es padre de axby

; Nota: cargar el documento varias veces en caso de error (por si acaso)

; TODO document

(defglobal ?*state_ln* = 0)

(deffunction clean(?initial)
    (bind ?*state_ln* (length ?initial))
    ; variable global para guardar como de largo es el estado (depende del número de habitaciones)
    (if (not (is_legal ?initial))
    then (printout t "El estado no es válido." crlf)
         (return)
    )
    (if (is_final ?initial)
    then (print_res ?initial)
         (return)
    )
    (bind ?visited (create$))
    (bind ?queue (create$ ?initial))
    (bind ?current ?initial)
    (while (not (is_final ?current))
        (bind ?visited (append (sub-string 1 ?*state_ln* ?current) ?visited))
        (bind ?queue (rest$ ?queue))
        (bind ?children (get_children ?current))
        (foreach ?child ?children
            (if (not(in (sub-string 1 ?*state_ln* ?child) ?visited))
            then (bind ?queue (append ?child ?queue))
            )
        )
        (bind ?current (nth$ 1 ?queue))
    )
    (print_res ?current)
    (return)
)

(deffunction append(?a $?vector)
    (if (not(multifieldp $?vector)) then
        (printout t "La segunda variable tiene que ser un multicampo." crlf)
        (return)
    )
    (return (insert$ $?vector (+ 1 (length $?vector)) ?a))
)

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

(deffunction print_res(?state)
    (bind ?i (length ?state))
    (while (> ?i 0)
        (printout t (sub-string (- ?i ?*state_ln* -1) ?i ?state) crlf)
        (bind ?i (- ?i ?*state_ln* 1))
    )
)

(deffunction is_legal(?state)
    (bind ?leaf (sub-string 1 ?*state_ln* ?state))
    (bind ?x (string-to-field(sub-string 2 2 ?leaf)))
    (bind ?y (string-to-field(sub-string 4 4 ?leaf)))
    (return (= 1 (+ ?x ?y)))
)

(deffunction is_final(?state)
    (bind ?leaf (sub-string 1 ?*state_ln* ?state))
    (bind ?a (string-to-field(sub-string 1 1 ?leaf)))
    (bind ?b (string-to-field(sub-string 3 3 ?leaf)))
    (return (= 0 (+ ?a ?b)))
)

(deffunction get_children(?state)
; returns a multifield with all the possible children states
    (bind ?leaf (sub-string 1 ?*state_ln* ?state))
    (bind $?res (create$))
    (bind $?res (append (str-cat (move_left ?leaf) " " ?state) $?res))
    (bind $?res (append (str-cat (move_right ?leaf) " " ?state) $?res))
    (bind $?res (append (str-cat (succ ?leaf) " " ?state) $?res))
    (return $?res)
)

(deffunction move_left(?state)
; returns resulting state after moving left
; always leaves the vaccum on the left room
    (bind ?res (sub-string 1 1 ?state))
    (bind ?res (str-cat ?res "1"))
    (bind ?res (str-cat ?res (sub-string 3 3 ?state)))
    (bind ?res (str-cat ?res "0"))
    (return ?res)
)

(deffunction move_right(?state)
; returns resulting state after moving right
; always leaves the vaccum on the right room
    (bind ?res (sub-string 1 1 ?state))
    (bind ?res (str-cat ?res "0"))
    (bind ?res (str-cat ?res (sub-string 3 3 ?state)))
    (bind ?res (str-cat ?res "1"))
    (return ?res)
)

(deffunction succ(?state)
; returns resulting state after succking
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
