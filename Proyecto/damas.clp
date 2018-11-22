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

; estado actual del tablero
(deftemplate JUEGO::tablero
  (multislot blancas)
  (multislot negras)
)

(deffunction cambiar_turno()
    (bind ?*TURNO* (not ?*TURNO*))
    (return ?*TURNO*)
)

; crea una linea de fichas donde ?x e ?y son las
; coordenadas de la primera ficha por la izquierda
(deffunction JUEGO::crear_linea (?x ?y)
    (bind ?result "")
    (bind ?result (str-cat "N" ?x ?y))
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

(deffunction JUEGO::print_tablero (?x ?y)
    (printout t "Imprimiendo tablero: " crlf)
    (bind ?negras ?x)
    (bind ?blancas ?y)
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

(defrule JUEGO::iniciar_tablero
    (declare (salience 100))
    ?f <- (inicializacion)
=>
    (retract ?f)
    (crear_tablero)
)

(defrule JUEGO::test
    (declare (salience 10))
    ?f <- (foo)
    =>
    (retract ?f)
    (printout t "tamaño: " ?*DIM* " | turno: " ?*COLOR_J* crlf)
)

(deffacts JUEGO::inicializacion
    (foo)
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
