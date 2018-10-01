; Representación estado: axby
; a: 0 -> primera habitación limpia; 1 -> primera habitación sucia
; x: 0 -> primera habitación no tiene aspiradora; 1 -> primera habitación tiene aspiradora
; b: 0 -> segunda habitación limpia; 1 -> segunda habitación sucia
; y: 0 -> segunda habitación no tiene aspiradora; 1 -> segunda habitación tiene aspiradora
; Un estado es correcto sí y solo sí x + y =
; Un estado es final sí y solo sí a + b = 0

(deffunction test(?state)
    (printout t (get_children ?state) crlf)
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

(deffunction is_legal(?state)
    (bind ?x (string-to-field(sub-string 2 2 ?state)))
    (bind ?y (string-to-field(sub-string 4 4 ?state)))
    (return (= 1 (+ ?x ?y)))
)

(deffunction is_final(?state)
    (bind ?a (string-to-field(sub-string 1 1 ?state)))
    (bind ?b (string-to-field(sub-string 3 3 ?state)))
    (return (= 0 (+ ?a ?b)))
)

(deffunction get_children(?state)
; returns a multifield with all the possible children states
    (bind $?res (create$))
    (bind $?res (append (move_left ?state) $?res))
    (bind $?res (append (move_right ?state) $?res))
    (bind $?res (append (succ ?state) $?res))
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
