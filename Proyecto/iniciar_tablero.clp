
(defglobal
   ?*SYM_B* = "o" ; simbolo para las blancas
   ?*SYM_N* = "x" ; simbolo para negras
   ?*SYM_V* = " " ; simbolo para el vacio
   ?*DIM* = 0 ; dimension del tablero
   ?*DEBUG* = TRUE ; dimension del tablero
)

(deftemplate tablero
  (multislot blancas)
  (multislot negras)
)

(deffacts inicializacion
  (inicializacion)
)

(deffunction crear_linea (?x ?y)
  (bind ?result "")
  (bind ?result (str-cat "N" ?x ?y))
  (loop-for-count (?i ?x (- ?*DIM* 1))
    (if (eq 0 (mod ?i 2)) then
      (bind ?result (str-cat ?result " N" (+ ?x ?i) ?y))
    )
  )
  (return ?result)
)

(deffunction crear_tablero ()
  (printout t "creando tablero" crlf)
  (printout t "Introduce la dimension del tablero: ")
  (bind ?*DIM* (read))
  (if ?*DEBUG* then
    (printout t "[DEBUG]Introduce las lineas de fichas: ")
    (bind ?lineas (read))
  else
    (bind ?lineas 3)
  )

  (bind ?negras "")
  (bind ?blancas "")
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

  (printout t "### POSICIONES INICIALES ### " ?negras crlf)
  (printout t "Negras: " ?negras crlf)
  (printout t "Blancas: " ?blancas crlf)

  (assert(tablero (blancas ?blancas) (negras ?negras)))
)


(defrule iniciar_tablero
  ?f <- (inicializacion)
=>
  (retract ?f)
  (crear_tablero)
)
