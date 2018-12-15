; TODO añadir luego que es modulo JUEGO
; (defglobal JUEGO
(defglobal
    ?*DIM* = 6 ; tamaño del tablero
    ?*TURNO* = TRUE ; turno actual. TRUE: blancas; FALSE: negras
    ?*COLOR_J* = TRUE ; color del jugador. TRUE: blancas; FALSE: negras
    ?*PIEZA_NORMAL* = "N" ; símbolo para representar las piezas normales (peones) en el programa
    ?*DAMA* = "D" ; símbolo para representar las damas en el programa
    ?*MOV_FORZADO* = FALSE ; flag para indicar que un movimiento está forzado (es una captura)
    ?*CORONADO* = FALSE ; flag para indicar que un movimiento termina en un peón siendo coronado
    ?*MOV_IA* = FALSE ; variable para guardar el movimiento realizado por la IA
    ?*SYMB_PEON_B* = "o" ; simbolo para los peones blancos
    ?*SYMB_PEON_N* = "x" ; simbolo para los peones negros
    ?*SYMB_DAMA_B* = "O" ; simbolo para las damas blancas
    ?*SYMB_DAMA_N* = "X" ; simbolo para las damas blancas
    ?*SYMB_G_PEON_B* = " o " ; simbolo para los peones blancos (en tablero grande)
    ?*SYMB_G_PEON_N* = " x " ; simbolo para los peones negros (en tablero grande)
    ?*SYMB_G_DAMA_B* = "oOo" ; simbolo para las damas blancas (en tablero grande)
    ?*SYMB_G_DAMA_N* = "xXx" ; simbolo para las damas blancas (en tablero grande)
    ?*SYM_V* = " " ; simbolo para casilla vacía
    ?*DEBUG* = TRUE


    ; ############### <HEURISTICOS> ###############
    ; VALORES DE PIEZAS
    ; esto sirve para calcular la diferencia de valores
    ?*HEU_VALOR_PIEZA_NORMAL* = 1
    ?*HEU_VALOR_PIEZA_DAMA* = 2

    ; VALORES INFINITO
    ?*HEU_MAS_INFINITO* = 9999
    ?*HEU_MENOS_INFINITO* = -9999

    ; RANDOM
    ?*HEU_RANDOM* = 100 ; del 1 al 100
    ?*HEU_RANDOM_DIVISOR* = 100 ; divisor del resultado
    ; ejemplo: ( random 1 al 100 ) / 100 = float entre 0.00 - 1.00
    ; ############### </HEURISTICOS> ###############

    ?*B* = (create$ N11 N31 N51 N22 N42 N62)
    ?*N* = (create$ N26 N46 N66 N15 N35 N55)
)

; ############### <HEURISTICOS> ###############

(deffunction tipo_pieza (?pieza)
  ; Devuelve el identificador de la pieza
  (return (sub-string 1 1 ?pieza))
)

(deffunction fila_pieza (?pieza)
   ; Devuelve la fila de la pieza
  (return (string-to-field (sub-string 3 3 ?pieza)))
)

(deffunction columna_pieza (?pieza)
   ; Devuelve la columna de la pieza
  (return (string-to-field (sub-string 2 2 ?pieza)))
)

(deffunction cuantas (?piezas ?tipo)
  ; Cuenta cuantas piezas del tipo indicado hay
  (bind ?resultado 0)
  (foreach ?pieza ?piezas
    (if (eq ?tipo (tipo_pieza ?pieza)) then
      (bind ?resultado (+ ?resultado 1))
    )
  )
  (return ?resultado)
)

(deffunction cuantas_damas (?piezas)
  ; Cuenta cuantas damas hay
  (return (cuantas ?piezas ?*DAMA*))
)

(deffunction cuantas_normales (?piezas)
  ; Cuenta cuantas piezas normales hay
  (return (cuantas ?piezas ?*PIEZA_NORMAL*))
)

(deffunction calcular_valor_piezas (?piezas)
  ; Calcula el valor de las piezas dadas
  (bind ?normales (cuantas_normales ?piezas))
  (bind ?punt_normales (* ?normales ?*HEU_VALOR_PIEZA_NORMAL*))

  (bind ?damas (cuantas_damas ?piezas))
  (bind ?punt_damas (* ?damas ?*HEU_VALOR_PIEZA_DAMA*))

  (bind ?resultado (+ ?punt_normales ?punt_damas))
  (return ?resultado)
)

(deffunction calcular_valor_por_fila_blancas (?blancas)
  ; RESULTADO:
  ; VALOR_PIEZA_NORMAL + Fila
  ; VALOR_PIEZA_DAMA + Numero de filas
  (bind ?resultado 0)
  (foreach ?pieza ?blancas
    (if (eq ?*PIEZA_NORMAL* (tipo_pieza ?pieza)) then
      (bind ?fila (fila_pieza ?pieza))
      (bind ?resultado (+ ?resultado ?fila ?*HEU_VALOR_PIEZA_NORMAL*))
    else
      (bind ?resultado (+ ?resultado ?*DIM* ?*HEU_VALOR_PIEZA_DAMA*))
    )
  )
  (return ?resultado)
)

(deffunction calcular_valor_por_fila_negras (?negras)
  ; RESULTADO:
  ; VALOR_PIEZA_NORMAL + Fila
  ; VALOR_PIEZA_DAMA + Numero de filas
  (bind ?resultado 0)
  (foreach ?pieza ?negras
    ; (printout t ?resultado crlf)
    (if (eq ?*PIEZA_NORMAL* (tipo_pieza ?pieza)) then
      (bind ?fila (fila_pieza ?pieza))
      ; Contar la fila como si empezaramos a contar desde el final
      ; Si el tablero tiene 6 filas la fila 6 sera la 1, la 5 la 2... etc
      (bind ?fila (abs (- ?fila (+ ?*DIM* 1)) ))
      (bind ?resultado (+ ?resultado ?fila ?*HEU_VALOR_PIEZA_NORMAL*))
    else
      (bind ?resultado (+ ?resultado ?*DIM* ?*HEU_VALOR_PIEZA_DAMA*))
    )
  )
  (return ?resultado)
)

; ###########################################
; ######## <SUBHEURISTICOS A USAR> #########

(deffunction sub_heuristico_diferencia_piezas(?aliadas ?contrarias)
  ; Heuristico que calcula la diferencia de valor de las piezas del tablero
  (bind ?result_aliadas (calcular_valor_piezas ?aliadas))
  (bind ?result_contrarias  (calcular_valor_piezas ?contrarias))
  (bind ?resultado (- ?result_aliadas ?result_contrarias))
  (return ?resultado)
)

(deffunction sub_heuristico_diferencia_piezas_normalizar(?aliadas ?contrarias)
  ; Heuristico que calcula la diferencia de valor de las piezas y las divide
  ; por el numero total de piezas del tablero.
  (bind ?num_piezas (+ (length$ ?aliadas) (length$ ?contrarias)))
  (bind ?result_aliadas (calcular_valor_piezas ?aliadas))
  (bind ?result_aliadas (/ ?result_aliadas ?num_piezas))
  (bind ?result_contrarias  (calcular_valor_piezas ?contrarias))
  (bind ?result_contrarias (/ ?result_contrarias ?num_piezas))
  (bind ?resultado (- ?result_aliadas ?result_contrarias))
  (return ?resultado)
)

(deffunction sub_heuristico_random ()
  ; Añade un factor random para que el juego no sea determinista
  ; ejemplo: ( random 1 al 100 ) / 100 = float entre 0.00 ... 1.00
  (return (/ (random 1 ?*HEU_RANDOM*) ?*HEU_RANDOM_DIVISOR*))
)

(deffunction sub_heuristico_valor_por_fila(?aliadas ?contrarias ?color)
  ; Cuanto mas alejadas de su fila inicial mas alto tendran el valor, este
  ; heuiristico hace que se quiera coronar piezas
  ; RESULTADO:
  ; VALOR_PIEZA_NORMAL + Fila
  ; VALOR_PIEZA_DAMA + Numero de filas
  (if ?color then
    (bind ?blancas ?aliadas)
    (bind ?negras ?contrarias)
  else
    (bind ?negras ?aliadas)
    (bind ?blancas ?contrarias)
  )

  (bind ?resultado_blancas (calcular_valor_por_fila_blancas ?blancas))
  (bind ?resultado_negras (calcular_valor_por_fila_negras ?negras))

  (if ?color then
    (bind ?resultado (- ?resultado_blancas ?resultado_negras))
    (return ?resultado)
  else
    (bind ?resultado (- ?resultado_negras ?resultado_blancas))
    (return ?resultado)
  )
)

; ######## </SUBHEURISTICOS A USAR> #########
; ###########################################

(deffunction heuristico1(?aliadas ?contrarias ?color)
  ; Aplica todos los heuristicos
  (bind ?resultado 0)
  (bind ?resultado (+ ?resultado (sub_heuristico_random)))
  (bind ?resultado (+ ?resultado (sub_heuristico_diferencia_piezas ?aliadas ?contrarias)))
  (bind ?resultado (+ ?resultado (sub_heuristico_diferencia_piezas_normalizar ?aliadas ?contrarias)))
  (bind ?resultado (+ ?resultado (sub_heuristico_valor_por_fila ?aliadas ?contrarias ?color)))
  (return ?resultado)
)

(deffunction heuristico2(?aliadas ?contrarias ?color)
  ; Aplica los heuristicos deseados para hacer pruebas
  (bind ?resultado 0)
  (bind ?resultado (+ ?resultado (sub_heuristico_random)))
  (return ?resultado)
)

; TODO añadir luego que es modulo JUEGO
; (deffunction JUEGO::heuristico(?blancas ?negras ?color)
(deffunction heuristico(?blancas ?negras ?color)
  ; ?color = True para blancas, False para negras
  ; Si una lista esta vacia es fin del juego, el resultado sera
  ; +infinito o -infinito en funcion de quien sea el que juega
  (if ?color then ; si juego con blancas
    (if (eq 0 (length$ ?negras)) then ; gano si no hay del otro color
      (return ?*HEU_MAS_INFINITO*)
    )
    (if (eq 0 (length$ ?blancas)) then ; pierdo si no hay de mi color
      (return ?*HEU_MENOS_INFINITO*)
    )
    (bind ?aliadas ?blancas)
    (bind ?contrarias ?negras)
    (return (heuristico1 ?aliadas ?contrarias ?color))
  else
    (if (eq 0 (length$ ?blancas)) then ; gano si no hay del otro color
      (return ?*HEU_MAS_INFINITO*)
    )
    (if (eq 0 (length$ ?negras)) then ; pierdo si no hay de mi color
      (return ?*HEU_MENOS_INFINITO*)
    )
    (bind ?aliadas ?negras)
    (bind ?contrarias ?blancas)
    (return (heuristico2 ?aliadas ?contrarias ?color))
  )
  ; (return (heuristico_tests ?aliadas ?contrarias ?color))
)

; ############### <HEURISTICOS> ###############
