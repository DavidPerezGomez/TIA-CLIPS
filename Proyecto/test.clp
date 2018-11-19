(defmodule INICIO (export defglobal tamaño))

(defglobal ?*tamaño* = 8)

(deffunction INICIO::pedir_param()
    (bind ?tamaño -1)
    (while (< ?tamaño 4) do
        (printout t "Introduce el tamaño del tablero (> 3): ")
        (bind ?tamaño (read))
    )
    (bind ?*tamaño* ?tamaño)
)

(defrule INICIO::pedir_param
    (declare (salience 10))
    =>
    (pedir_param)
    (focus FOO)
    (return)
)

(defmodule FOO (import INICIO defglobal tamaño))

(defrule FOO::main
    (declare (salience 10))
    =>
    (printout t ?*tamaño* crlf)
    (return)
)

(defrule MAIN::inicio
    =>
    (focus INICIO)
)
