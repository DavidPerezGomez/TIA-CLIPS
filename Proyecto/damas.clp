; ==============================================================================
; JUEGO
; ==============================================================================
(defmodule JUEGO (export defglobal DIM COLOR_J))

(defglobal JUEGO
    ?*DIM* = 8 ; tamaño del tablero
    ?*TURNO* = TRUE ; turno actual. TRUE: blancas; FALSE: negras
    ?*COLOR_J* = TRUE ; color del jugador. TRUE: blancas; FALSE: negras
    ?*PIEZA_NORMAL* = "N"
    ?*DAMA* = "D"
    ?*MOV_FORZADO* = FALSE
    ?*SYM_B* = "o" ; simbolo para las blancas
    ?*SYM_N* = "x" ; simbolo para negras
    ?*SYM_V* = " " ; simbolo para el vacio
    ?*DEBUG* = TRUE
)

; estado actual del tablero
(deftemplate JUEGO::tablero
  (multislot blancas)
  (multislot negras)
)

(deffunction JUEGO::in(?item $?vector)
    (if (member$ ?item $?vector) then
        (return TRUE)
    else
        (return FALSE)
    )
)

(deffunction JUEGO::last($?vector)
    (return (nth$ (length$ $?vector) $?vector))
)

(deffunction JUEGO::append(?a $?vector)
    (return (insert$ $?vector (+ 1 (length $?vector)) ?a))
)

(deffunction JUEGO::prepend(?a $?vector)
    (return (insert$ $?vector 1 ?a))
)

(deffunction JUEGO::cambiar_turno()
    (bind ?*TURNO* (not ?*TURNO*))
    (return ?*TURNO*)
)

(deffunction JUEGO::is_in_board(?x ?y)
    (return (and (> ?x 0) (> ?y 0) (<= ?x ?*DIM*) (<= ?y ?*DIM*)))
)

; crea una linea de fichas donde ?x e ?y son las
; coordenadas de la primera ficha por la izquierda
(deffunction JUEGO::crear_linea (?x ?y)
    (bind ?result "")
    (bind ?result (str-cat ?*PIEZA_NORMAL* ?x ?y))
    (loop-for-count (?i ?x (- ?*DIM* 1))
        (if (eq 0 (mod ?i 2)) then
            (bind ?result (str-cat ?result " N" (+ ?x ?i) ?y))
        )
    )
    (return ?result)
)

(deffunction JUEGO::crear_tablero()
    (bind ?negras "")
    (bind ?blancas "")
    (bind ?lineas (- (/ ?*DIM* 2) 1))
    (loop-for-count (?i 1 ?lineas)
        (if (eq 0 (mod ?i 2)) then
            (bind ?blancas (str-cat ?blancas (crear_linea 2 ?i) " "))
            (bind ?negras (str-cat ?negras (crear_linea 1 (- ?*DIM* ?i -1)) " "))
        else
            (bind ?blancas (str-cat ?blancas (crear_linea 1 ?i) " "))
            (bind ?negras (str-cat ?negras (crear_linea 2 (- ?*DIM* ?i -1)) " "))
        )
    )
    ; Cambiar las fichas a multicampos
    (bind ?negras (explode$ ?negras))
    (bind ?blancas (explode$ ?blancas))

    (printout t "### POSICIONES INICIALES ### " crlf)
    (printout t "Negras: " ?negras crlf)
    (printout t "Blancas: " ?blancas crlf)

    (assert(tablero (blancas ?blancas) (negras ?negras)))
)

(deffunction JUEGO::print_tablero (?blancas ?negras)
    (printout t "Imprimiendo tablero: " crlf)
    (bind ?longnegras (length$ ?negras))
    (bind ?longblancas (length$ ?blancas))
    (printout t "longnegras: " ?longnegras crlf)
    (printout t "longblancas: " ?longblancas crlf)

    (bind ?v ?*DIM*)
    (while (> ?v 0)
        (loop-for-count (?i 1 ?*DIM*)
            (bind ?variable (nth ?i ?negras))
            (printout t ?variable "variable")
            (printout t "N" ?v ?i  " "))
            (bind ?v (- ?v 1))
        (printout t crlf)
    )
)

; devuelve los posibles movimientos de una pieza normal
; solamente se tiene en cuenta un salto, aunque sea posible hacer varios
; direccion -> 1 si sube (blancas); -1 si baja (negras)
; devuelve un multicampo en el que cada valor es un movimiento posible
; cada movimiento viene en forma de string
; si no se puede mover, el multicampo estará vacío
; si el movimiento es simple (no se come) la string son solo las coordenadas del destino
; > "24" (se mueve a (2,4))
; si en el movimiento se captura a otra pieza, la string contiene las coordenadas
; de la pieza capturada y del destino separadas por un espacio
; > "35 46" (captura la pieza en (3,5) y se mueve a (4,6))
(deffunction JUEGO::mov_pieza_normal(?x ?y ?direccion ?atacantes ?defendientes)
    (bind ?mov (create$))
    (bind ?posiciones (create$
        (sym-cat (- ?x 1) (+ ?y ?direccion))
        (sym-cat (+ ?x 1) (+ ?y ?direccion))))
    ; miramos en las posiciones básicas
    (foreach ?pos ?posiciones
        (bind ?pos_x (string-to-field (sub-string 1 1 ?pos)))
        (bind ?pos_y (string-to-field (sub-string 2 2 ?pos)))
        ; comprobamos que está dentro del tablero
        (if (is_in_board ?pos_x ?pos_y) then
            ; creamos las posibles piezas que podrían estar en esa posición
            (bind ?posibles_piezas (create$
                (sym-cat ?*PIEZA_NORMAL* ?pos_x ?pos_y)
                (sym-cat ?*DAMA* ?pos_x ?pos_y)))
            (bind ?ocupada FALSE)
            ; si está en las enemigas
            (foreach ?posible_pieza ?posibles_piezas
                (if (in ?posible_pieza ?defendientes) then
                    (bind ?ocupada TRUE)
                    (break)
                )
            )
            (if ?ocupada then
                ; se mira en la siguiente posición
                (bind ?dif_x (- ?pos_x ?x))
                (bind ?dif_y (- ?pos_y ?y))
                (bind ?sig_pos_x (+ ?pos_x ?dif_x))
                (bind ?sig_pos_y (+ ?pos_y ?dif_y))
                ; comprobamos que está dentro del tablero
                (if (is_in_board ?sig_pos_x ?sig_pos_y) then
                    ; creamos las posibles piezas que podrían estar en esa posición
                    (bind ?sig_posibles_piezas (create$
                        (sym-cat ?*PIEZA_NORMAL* ?sig_pos_x ?sig_pos_y)
                        (sym-cat ?*DAMA* ?sig_pos_x ?sig_pos_y)))
                    (bind ?sig_ocupada FALSE)
                    ; si no está en las aliadas o en las enemigas
                    (foreach ?sig_posible_pieza ?sig_posibles_piezas
                        (if (or (in ?sig_posible_pieza ?defendientes)
                                (in ?sig_posible_pieza ?atacantes)) then
                            (bind ?sig_ocupada TRUE)
                            (break)
                        )
                    )
                    (if (not ?sig_ocupada) then
                        ; la casilla está vacía
                        ; se captura la pieza intermedia
                        (if (not ?*MOV_FORZADO*) then
                            ; si los movimientos anteriores no están forzados
                            ; se vacía la lista de movimientos
                            (bind ?mov (create$))
                            (bind ?*MOV_FORZADO* TRUE)
                        )
                        (bind ?mov (append (str-cat
                        ?pos_x ?pos_y " " ?sig_pos_x ?sig_pos_y) ?mov))
                        ; else
                            ; la casilla está ocupada
                            ; no se puede mover; no se hace nada
                    )
                )
            ; si no está en las enemigas
            else
                ; ni en las aliadas
                (bind ?ocupada FALSE)
                (foreach ?posible_pieza ?posibles_piezas
                    (if (in ?posible_pieza ?atacantes) then
                        (bind ?ocupada TRUE)
                        (break)
                    )
                )
                (if (not ?ocupada) then
                    ; movimiento normal
                    ; se añade si no hay algún movimiento forzado
                    (if (not ?*MOV_FORZADO*) then
                        (bind ?mov (append (str-cat ?pos_x ?pos_y) ?mov))
                    )
                )
            )
        )
    )
    (return ?mov)
)

; hace que me quiera tirar por la ventana
; devuelve depresión
(deffunction JUEGO::mov_dama(?x ?y ?direccion ?atacantes ?defendientes)
    (return (create$))
)

; devuelve un multicampo en el que cada valor es una string representando
; el movimiento de una pieza
; en cada string, el primer valor son las coordenadas de la pieza
; el último valor son las cordenadas a las que se mueve
; y el valor intermedio (si lo hubiera) son las coordenadas de la
; pieza que captura
; solo se tiene en cuenta un salto en cada movimiento aunque pudiese
; haber más
; > ("13 24" "33 24" "33 44" "53 44" "53 64" "73 64" "73 84")
(deffunction JUEGO::movimientos(?blancas ?negras ?juegan_blancas)
    (bind ?*MOV_FORZADO* FALSE)
    (if ?juegan_blancas then
        (bind ?atacantes ?blancas)
        (bind ?defendientes ?negras)
        (bind ?direccion 1)
    else
        (bind ?atacantes ?negras)
        (bind ?defendientes ?blancas)
        (bind ?direccion -1)
    )
    (bind ?movimientos (create$))
    (foreach ?pieza ?atacantes
        (bind ?prev_forzado ?*MOV_FORZADO*)
        (bind ?tipo (sub-string 1 1 ?pieza))
        (bind ?x (string-to-field (sub-string 2 2 ?pieza)))
        (bind ?y (string-to-field (sub-string 3 3 ?pieza)))
        (if (eq ?tipo ?*PIEZA_NORMAL*) then
            (bind ?mov (mov_pieza_normal ?x ?y ?direccion ?atacantes ?defendientes))
            (if (and ?*MOV_FORZADO* (not ?prev_forzado)) then
                (bind ?movimientos (create$))
                (bind ?prev_forzado ?*MOV_FORZADO*)
            )
            (if (eq ?prev_forzado ?*MOV_FORZADO*) then
                (foreach ?m ?mov
                    (bind ?mov_completo (str-cat ?x ?y " " ?m))
                    (bind ?movimientos (append ?mov_completo ?movimientos))
                )
            )
        ; else (if (eq ?tipo ?*DAMA*) then
        );)
    )
)

(deffunction JUEGO::pedir_mov(?blancas ?negras)
    (bind ?pos_mov (movimientos ?blancas ?negras ?*TURNO*))
    (while TRUE
        (print_tablero ?blancas ?negras)
        (printout t "¿Qué pieza quieres mover? xy: ")
        (bind ?pieza (str-cat (read)))
        (printout t (length ?pieza))
        (if (eq (length ?pieza) 3) then
            (bind ?pieza (str-cat (sub-string 1 1 ?pieza) (sub-string 3 3 ?pieza)))
        )
        (bind ?pieza_correcta FALSE)
        (foreach ?mov ?pos_mov
            (if (eq (sub-string 1 2 ?mov) ?pieza) then
                (bind ?pieza_correcta TRUE)
                (break)
            )
        )
        (if ?pieza_correcta then
            (printout t "¿A que posición quieres moverla? xy: ")
            (bind ?posicion (str-cat (read)))
            (if (eq (length ?posicion) 3) then
                (bind ?posicion (str-cat (sub-string 1 1 ?posicion) (sub-string 3 3 ?posicion)))
            )
            (foreach ?mov ?pos_mov
                (bind ?long (length ?mov))
                (if (eq (sub-string (- ?long 1) ?long ?mov) ?posicion) then
                    (return ?mov)
                )
            )
        )
    )
)

(defrule JUEGO::iniciar_tablero
    (declare (salience 100))
    ?f <- (inicializacion)
=>
    (retract ?f)
    (crear_tablero)
)

(defrule JUEGO::turno
    (declare (salience 50))
    ?t <- (tablero (blancas $?b) (negras $?n))
    =>
    (bind ?mov (pedir_mov $?b $?n))
    (printout t ?mov crlf)
)

; (defrule JUEGO::test
;     (declare (salience 90))
;     (tablero (blancas $?b) (negras $?n))
;     =>
;     (print_tablero $?b $?n)
; )

(deffacts JUEGO::inicializacion
    (inicializacion)
)

; ==============================================================================
; INICIO
; ==============================================================================
(defmodule INICIO (import JUEGO defglobal DIM COLOR_J))

(deffunction INICIO::pedir_param()
    (bind ?tamaño -1)
    (while (or (< ?tamaño 4) (> ?tamaño 9) (not (= 0 (mod ?tamaño 2)))) do
        (printout t "Introduce el tamaño del tablero (> 3 & < 10 & par): ")
        (bind ?tamaño (read))
    )
    (bind ?*DIM* ?tamaño)
    (bind ?exit FALSE)
    (while TRUE
        (printout t "¿Juegas con blancas o negras? (b/n): ")
        (bind ?color (read))
        (if (= 0 (str-compare "b" (lowcase ?color))) then
            (bind ?*COLOR_J* TRUE)
            (return)
        )
        (if (= 0 (str-compare "n" (lowcase ?color))) then
            (bind ?*COLOR_J* FALSE)
            (return)
        )
    )
)

(defrule INICIO::pedir_param
    (inicio)
    =>
    (pedir_param)
    (focus JUEGO)
    (return)
)

(deffacts INICIO::inicializacion
    (inicio)
)

; ==============================================================================
; MAIN
; ==============================================================================
(defrule MAIN::main
    (initial-fact)
    =>
    (focus INICIO)
)
