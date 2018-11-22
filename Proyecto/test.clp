(defmodule MAIN (export deftemplate foo))

(deftemplate foo
    (slot bar)
)

(defrule MAIN::inicio
    =>
    (assert (foo (bar 0)))
    (focus INICIO)
    )

(defmodule INICIO (export defglobal tamaño) (import MAIN deftemplate foo))
(defglobal INICIO ?*tamaño* = 8)

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
    (foo)
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
