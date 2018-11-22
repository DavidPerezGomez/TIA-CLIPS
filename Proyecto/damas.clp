; ==============================================================================
; JUEGO
; ==============================================================================
(defmodule JUEGO (export defglobal DIM COLOR_J))

(defglobal JUEGO
    ?*DIM* = 8 ; tamaño del tablero
    ?*TURNO* = TRUE ; turno actual. TRUE: blancas; FALSE: negras
    ?*COLOR_J* = TRUE ; color del jugador. TRUE: blancas; FALSE: negras
    ?*SYM_B* = "o" ; simbolo para las blancas
    ?*SYM_N* = "x" ; simbolo para negras
    ?*SYM_V* = " " ; simbolo para el vacio
    ?*DEBUG* = TRUE
)

(defrule JUEGO::test
    (declare (salience 10))
    =>
    (printout t "tamaño: " ?*DIM* " | turno: " ?*COLOR_J* crlf)
)

; ==============================================================================
; INICIO
; ==============================================================================
(defmodule INICIO (import JUEGO defglobal DIM COLOR_J))

(deffunction INICIO::pedir_param()
    (bind ?tamaño -1)
    (while (or (< ?tamaño 4) (not (= 0 (mod ?tamaño 2)))) do
        (printout t "Introduce el tamaño del tablero (> 3 y par): ")
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
    =>
    (pedir_param)
    (focus JUEGO)
    (return)
)

; ==============================================================================
; MAIN
; ==============================================================================
(defrule MAIN::main
    =>
    (focus INICIO)
)
