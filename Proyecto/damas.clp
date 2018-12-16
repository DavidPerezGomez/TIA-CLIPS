; ==============================================================================
; JUEGO
; ==============================================================================
; Módulo para ejecutar el juego. Dibuja el tablero, gestiona los turnos, pide
; moviminetos al jugador y a la IA, calcula el resultado de los movimientos y
; los aplica y comprueba cuando el juego termina.
(defmodule JUEGO (export deftemplate tablero ia_movido)
                 (export defglobal DIM COLOR_J MOV_FORZADO CORONADO MOV_IA DIM)
                 (export deffunction cuantas_damas movimientos calcular_movimiento heuristico in append))

(defglobal JUEGO
    ?*DIM* = 8 ; tamaño del tablero
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
)

; estado actual del tablero
; blancas y negras son strings que representan las piezas de cada jugador
; en cada string, las piezas se representan mediante una letra para identificar
; su tipo (N/D) su columna y su fila
; N45 es un peón en x=4 & y=5 (columna 4, fila 5)
; los identificadores de cada pieza están separados por espacios
; blancas = "N11 N31 N51 N71 N22 N42 N62 N82 N13 N33 N53 N73"
(deftemplate JUEGO::tablero
  (multislot blancas)
  (multislot negras)
)

; estado de tablero para multiples movimientos
(deftemplate JUEGO::tablero_tmp
  (multislot blancas)
  (multislot negras)
  (slot pieza_a_mover)
)

(deftemplate JUEGO::ia_movido)

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
    ; (bind ?result (str-cat ?*DAMA* ?x ?y))
    (loop-for-count (?i ?x (- ?*DIM* 1))
        (if (eq 0 (mod ?i 2)) then
            (bind ?result (str-cat ?result " " ?*PIEZA_NORMAL* (+ ?x ?i) ?y))
            ; (bind ?result (str-cat ?result " " ?*DAMA* (+ ?x ?i) ?y))
        )
    )
    (return ?result)
)

; crea ua versión personalizada del tablero
(deffunction JUEGO::crear_tablero_test()
    (bind ?blancas "N33")
    (bind ?negras "N44")
    ; Cambiar las fichas a multicampos
    (bind ?negras (explode$ ?negras))
    (bind ?blancas (explode$ ?blancas))
    (assert(tablero (blancas ?blancas) (negras ?negras)))
)

; crea el tablero inicial
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

; dibuja un tablero con las piezas blancs y negras
(deffunction JUEGO::print_tablero_pequeño(?blancas ?negras)
    (loop-for-count (?i 0 ?*DIM*)
        (bind ?linea "")
        (bind ?fila (- ?*DIM* ?i))
        (loop-for-count (?col 0 ?*DIM*)
            (if (= ?col 0) then
                ; columna de números
                (bind ?linea (str-cat ?fila))
                (if (= ?fila 0) then
                    (bind ?linea (str-cat ?linea " "))
                )
            else (if (= ?fila 0) then
                ; fila de numeros
                (bind ?linea (str-cat ?linea (str-cat ?col " ")))
            else
                ; casilla del tablero
                ; buscar la pieza
                ; creamos las posibles piezas que podrían estar en esa posición
                (bind ?posibles_piezas (create$
                    (sym-cat ?*PIEZA_NORMAL* ?col ?fila)
                    (sym-cat ?*DAMA* ?col ?fila)))
                (bind ?sym FALSE)
                ; se busca en las blancas
                (foreach ?posible_pieza ?posibles_piezas
                    (if (in ?posible_pieza ?blancas) then
                        (bind ?tipo (sub-string 1 1 ?posible_pieza))
                        (if (eq ?tipo ?*PIEZA_NORMAL*) then
                            (bind ?sym ?*SYMB_PEON_B*)
                        else
                            (bind ?sym ?*SYMB_DAMA_B*)
                        )
                        (break)
                    )
                )
                (if (not ?sym) then
                    ; si no está en las blancas se busca en las negras
                    (foreach ?posible_pieza ?posibles_piezas
                        (if (in ?posible_pieza ?negras) then
                            (bind ?tipo (sub-string 1 1 ?posible_pieza))
                            (if (eq ?tipo ?*PIEZA_NORMAL*) then
                                (bind ?sym ?*SYMB_PEON_N*)
                            else
                                (bind ?sym ?*SYMB_DAMA_N*)
                            )
                            (break)
                        )
                    )
                )
                (if (not ?sym) then
                    (bind ?sym " ")
                )
                (bind ?linea (str-cat ?linea (str-cat "|" ?sym)))
            ))
        )
        (printout t ?linea crlf)
    )
)

; dibuja un tablero más grande con las piezas blancs y negras
(deffunction JUEGO::print_tablero_grande(?blancas ?negras)
    (loop-for-count (?i 0 ?*DIM*)
        (bind ?fila (- ?*DIM* ?i))
        (printout t "   ")
        (loop-for-count (?i 1 ?*DIM*)
            (printout t "----")
        )
        (printout t "-" crlf)
        (bind ?linea "")
        (loop-for-count (?col 0 ?*DIM*)
            (if (= ?col 0) then
                ; columna de números
                (bind ?linea (str-cat " " ?fila " "))
            else (if (= ?fila 0) then
                ; fila de numeros
                (bind ?linea (str-cat ?linea (str-cat "  " ?col " ")))
            else
                ; casilla del tablero
                ; buscar la pieza
                ; creamos las posibles piezas que podrían estar en esa posición
                (bind ?posibles_piezas (create$
                    (sym-cat ?*PIEZA_NORMAL* ?col ?fila)
                    (sym-cat ?*DAMA* ?col ?fila)))
                (bind ?sym FALSE)
                ; se busca en las blancas
                (foreach ?posible_pieza ?posibles_piezas
                    (if (in ?posible_pieza ?blancas) then
                        (bind ?tipo (sub-string 1 1 ?posible_pieza))
                        (if (eq ?tipo ?*PIEZA_NORMAL*) then
                            (bind ?sym ?*SYMB_G_PEON_B*)
                        else
                            (bind ?sym ?*SYMB_G_DAMA_B*)
                        )
                        (break)
                    )
                )
                (if (not ?sym) then
                    ; si no está en las blancas se busca en las negras
                    (foreach ?posible_pieza ?posibles_piezas
                        (if (in ?posible_pieza ?negras) then
                            (bind ?tipo (sub-string 1 1 ?posible_pieza))
                            (if (eq ?tipo ?*PIEZA_NORMAL*) then
                                (bind ?sym ?*SYMB_G_PEON_N*)
                            else
                                (bind ?sym ?*SYMB_G_DAMA_N*)
                            )
                            (break)
                        )
                    )
                )
                (if (not ?sym) then
                    (bind ?sym "   ")
                )
                (bind ?linea (str-cat ?linea (str-cat "|" ?sym)))
            ))
        )
        (if (not (= ?fila 0)) then
            (bind ?linea (str-cat ?linea "|"))
        )
        (printout t ?linea crlf)
    )
)

(deffunction JUEGO::print_tablero(?blancas ?negras ?pequeño)
    (if ?pequeño then
        (print_tablero_pequeño ?blancas ?negras)
    else
        (print_tablero_grande ?blancas ?negras)
    )
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
  ; (bind ?resultado (+ ?resultado (sub_heuristico_diferencia_piezas_normalizar ?aliadas ?contrarias)))
  ; (bind ?resultado (+ ?resultado (sub_heuristico_valor_por_fila ?aliadas ?contrarias ?color)))
  (return ?resultado)
)

(deffunction heuristico2(?aliadas ?contrarias ?color)
  ; Aplica los heuristicos deseados para hacer pruebas
  (bind ?resultado 0)
  (bind ?resultado (+ ?resultado (sub_heuristico_random)))
  (bind ?resultado (+ ?resultado (sub_heuristico_diferencia_piezas ?aliadas ?contrarias)))
  (bind ?resultado (+ ?resultado (sub_heuristico_diferencia_piezas_normalizar ?aliadas ?contrarias)))
  (bind ?resultado (+ ?resultado (sub_heuristico_valor_por_fila ?aliadas ?contrarias ?color)))
  (return ?resultado)
)

(deffunction JUEGO::heuristico(?blancas ?negras ?color)
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
    (return (heuristico2 ?aliadas ?contrarias ?color))
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
)

; (deffunction JUEGO::heuristico(?blancas ?negras ?color)
;     (bind ?heuristico -1)
;     ; criterios y su ponderación:
;     ; valor de las piezas
;     (bind ?pond_piezas 1)
;     (bind ?val_peon 3)
;     (bind ?val_dama 5)
;     ; posicionamiento de peones
;     (bind ?pond_pos_peones 1)
;     ; posicionamiento de las damas
;     (bind ?pond_pos_damas 1)
;     ; componente aleatorios
;     ; (bind ?rand (random -10 10))
;     (bind ?pond_rand 0.1)
;     (return (random 1 100))
; )

; ############### <HEURISTICOS> ###############

; este no hace falta. lo dejo por si acaso
(deffunction coronar(?pieza ?piezas)
    (if (eq (sub-string 1 1 ?pieza) ?*PIEZA_NORMAL*) then
        (return ?piezas)
    )
    (bind ?index (member$ ?pieza ?piezas))
    (if ?index then
        (bind ?nueva_pieza (sym-cat ?*DAMA* (sub-string 2 3 ?pieza)))
        (bind ?nuevas_piezas (replace$ ?piezas ?index ?index ?nueva_pieza))
        (return ?nuevas_piezas)
    )
    (return FALSE)
)

; calcula el resultado de realizar el movimiento ?mov sobre el tablero
; devuelve una string con las blancas y las negras separadas por un '|'
; ej: "N11 N34 D23|N56 N71"
(deffunction calcular_movimiento(?blancas ?negras ?mov ?color )
    (bind ?*CORONADO* FALSE)
    (bind ?long (length ?mov))
    (bind ?pos_origen (sub-string 1 2 ?mov))
    (bind ?pos_destino (sub-string (- ?long 1) ?long ?mov))
    (bind ?encontrada FALSE)

    ; generalización de las listas de piezas en ?aliadas y ?enemigas
    (if ?color then
        (bind ?aliadas ?blancas)
        (bind ?nuevas_aliadas ?blancas)
        (bind ?enemigas ?negras)
        (bind ?nuevas_enemigas ?negras)
    else
        (bind ?aliadas ?negras)
        (bind ?nuevas_aliadas ?negras)
        (bind ?enemigas ?blancas)
        (bind ?nuevas_enemigas ?blancas)
    )

    (bind ?index 0)
    ; iterar las aliadas buscando la pieza que se quiere mover
    (loop-for-count (?i 1 (length$ ?aliadas))
        (bind ?pieza (nth$ ?i ?aliadas))
        (bind ?tipo (sub-string 1 1 ?pieza))
        (bind ?pos (sub-string 2 3 ?pieza))
        ; si las posiciones son iguales
        (if (eq ?pos ?pos_origen) then
            ; creamos la nueva pieza después de haber sido movida
            (if (or
                    (and ?color (= ?*DIM* (string-to-field (sub-string 2 2 ?pos_destino))))
                    (and (not ?color) (= 1 (string-to-field (sub-string 2 2 ?pos_destino))))
                ) then
                (if (eq ?tipo ?*PIEZA_NORMAL*) then
                ; si la pieza llega al final del tablero y es un peón, se corona
                    (bind ?tipo ?*DAMA*)
                    (bind ?*CORONADO* TRUE)
                )
            )
            (bind ?pieza_movida (sym-cat ?tipo ?pos_destino))
            (bind ?encontrada TRUE)
            ; guardamos el índice de la pieza
            (bind ?index ?i)
            (break)
        )
    )
    (if ?encontrada then
        ; si se ha encontrado, se reemplaza la pieza original por la movida
        (bind ?nuevas_aliadas (replace$ ?aliadas ?index ?index ?pieza_movida))
    else
        ; si no se ha encontrado, se asume error y se sale del juego
        (printout t ?mov " -> mov. no encontrado" crlf)
        (halt)
    )
    (bind ?lista (explode$ ?mov))
    (if (> (length$ ?lista) 2) then
        (bind ?nuevas_enemigas ?enemigas)
        (bind ?capturadas (subseq$ ?lista 2 (- (length$ ?lista) 1)))
        (foreach ?capturada ?capturadas
            (bind ?pos_capturada (str-cat ?capturada))
            (bind ?encontrada FALSE)
            (bind ?index 0)
            ; iterar las enemigas buscando la pieza que se ha capturado
            (loop-for-count (?i 1 (length$ ?nuevas_enemigas))
                (bind ?pieza (nth$ ?i ?nuevas_enemigas))
                (bind ?pos (sub-string 2 3 ?pieza))
                ; si las posiciones son iguales
                (if (eq ?pos_capturada ?pos) then
                    (bind ?encontrada TRUE)
                    ; guardamos el índice de la pieza
                    (bind ?index ?i)
                    (break)
                )
            )
            (if ?encontrada then
                ; si se ha encontrado, se elimina la pieza capturada de la lista
                (bind ?nuevas_enemigas (delete$ ?nuevas_enemigas ?index ?index))
            )
        )
    )
    ; se recuperan los colores de las piezas
    (if ?color then
        (bind ?nuevas_blancas ?nuevas_aliadas)
        (bind ?nuevas_negras ?nuevas_enemigas)
    else
        (bind ?nuevas_blancas ?nuevas_enemigas)
        (bind ?nuevas_negras ?nuevas_aliadas)
    )
    (return (str-cat (implode$ ?nuevas_blancas) "|" (implode$ ?nuevas_negras)))
)

; aplica el movimiento ?mov al tablero formado por ?blancas y ?negras
; genera un nuevo par de vectores de blancas y negras y los utiliza
; para crear un nuevo estado tablero
; devuelve el identificador del nuevo estado
(deffunction JUEGO::aplicar_movimiento(?blancas ?negras ?mov ?color)
    (bind ?resultado (calcular_movimiento ?blancas ?negras ?mov ?color))
    (bind ?index_separador (str-index "|" ?resultado))
    (bind ?nuevas_blancas (explode$ (sub-string 1 (- ?index_separador 1) ?resultado)))
    (bind ?nuevas_negras (explode$ (sub-string (+ ?index_separador 1) (length ?resultado) ?resultado)))
    ; se crea el tablero con las nuevas piezas
    (if (and ?*MOV_FORZADO* (not ?*CORONADO*)) then
        ; si alguno de los movimientos ha sido forzado, hay posibilidad de que
        ; haya más capturas posibles en el mism turno.
        ; se hace un tablero_tmp para investigar
        (bind ?long (length ?mov))
        (bind ?pos_destino (sub-string (- ?long 1) ?long ?mov))
        (return (assert (tablero_tmp (blancas ?nuevas_blancas) (negras ?nuevas_negras) (pieza_a_mover ?pos_destino))))
    else
        ; el turno se ha terminado. se crea el nuevo tablero
        (cambiar_turno)
        (return (assert (tablero (blancas ?nuevas_blancas) (negras ?nuevas_negras))))
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
            ; si hay alguna posible pieza enemiga
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
                    ; miramos si está ocupada por alguna pieza
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

; devuelve los posibles movimientos de una dama
; solamente se tiene en cuenta un salto, aunque sea posible hacer varios
; devuelve un multicampo en el que cada valor es un movimiento posible
; cada movimiento viene en forma de string
; si no se puede mover, el multicampo estará vacío
; si el movimiento es simple (no se come) la string son solo las coordenadas del destino
; > "24" (se mueve a (2,4))
; si en el movimiento se captura a otra pieza, la string contiene las coordenadas
; de la pieza capturada y del destino separadas por un espacio
; > "35 46" (captura la pieza en (3,5) y se mueve a (4,6))
(deffunction JUEGO::mov_dama(?x ?y ?atacantes ?defendientes)
    (bind ?mov (create$))
    ; se iteran las cuatro diagonales
    ; los movimientos de cada diagonal se añadirán a ?mov
    (foreach ?horizontal (create$ 1 -1)
        (foreach ?vertical (create$ 1 -1)
            ; los movimientos de la diagnonal
            (bind ?diagonal (create$))
            ; estado de búsqueda. si FALSE, no se ha encontrado ninguna pieza
            ; en esta diagonal. si se ha encontrado algu pieza enemiga, se
            ; guarda su posición
            (bind ?enemiga_encontrada FALSE)
            ; casilla original
            (bind ?pos_x ?x)
            (bind ?pos_y ?y)
            ; avanzamos en la diagonal hasta encontrar una razón para salir
            (bind ?salir FALSE)
            (while TRUE
                (bind ?ocupada_enemiga FALSE)
                (bind ?ocupada_aliada FALSE)
                ; calculamos la siguiente casilla
                (bind ?pos_x (+ ?pos_x ?horizontal))
                (bind ?pos_y (+ ?pos_y ?vertical))

                (if (not (is_in_board ?pos_x ?pos_y)) then
                    ; si está fuera del tablero, pasamos a la siguiente diagonal
                    (bind ?mov (append ?diagonal ?mov))
                    (break)
                )
                ; comprobamos si hay una pieza enemiga
                (foreach ?enemiga ?defendientes
                    (bind ?pos (sub-string 2 3 ?enemiga))
                    (if (eq ?pos (str-cat ?pos_x ?pos_y)) then
                        (bind ?ocupada_enemiga ?pos)
                        (break)
                    )
                )
                (if (not ?ocupada_enemiga) then
                    ; si no hay una pieza enemiga
                    ; comprobamos si hay una pieza aliada
                    (foreach ?aliada ?atacantes
                        (bind ?pos (sub-string 2 3 ?aliada))
                        (if (eq ?pos (str-cat ?pos_x ?pos_y)) then
                            (bind ?ocupada_aliada ?pos)
                            (break)
                        )
                    )
                )

                (if ?ocupada_enemiga then
                    ; si hay una pieza enemiga
                    (if (not ?enemiga_encontrada) then
                        ; si es la primera enemiga que se encuentra
                        ; se guarda su posición
                        (bind ?enemiga_encontrada ?ocupada_enemiga)
                    else
                        ; si no es la primera enemiga que se encuentra
                        ; salimos
                        (bind ?salir TRUE)
                    )
                else (if ?ocupada_aliada then
                    ; si hay una pieza aliada
                    ; salimos
                    (bind ?salir TRUE)
                else
                    ; si la casilla está vacía
                    (if (not ?enemiga_encontrada) then
                        ; si no se ha encontrado ninguna enemiga
                        ; se añade si no hay algún movimiento forzado
                        (if (not ?*MOV_FORZADO*) then
                            (bind ?diagonal (append (str-cat ?pos_x ?pos_y) ?diagonal))
                        )
                    else
                        ; si se ha encontrado alguna enemiga
                        (if (not ?*MOV_FORZADO*) then
                            (bind ?*MOV_FORZADO* TRUE)
                            (bind ?mov (create$))
                            (bind ?diagonal (create$))
                        )
                        ; se añade a la diagonal
                        (bind ?diagonal (append
                            (str-cat ?enemiga_encontrada " " ?pos_x ?pos_y) ?diagonal))
                    )
                ))
                (if ?salir then
                    (bind ?mov (append ?diagonal ?mov))
                    (break)
                )
            )
        )
    )
    (return ?mov)
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
(deffunction JUEGO::movimientos(?blancas ?negras ?juegan_blancas ?pieza_a_mover)
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
        (if (or (not ?pieza_a_mover) (eq (sub-string 2 3 ?pieza) ?pieza_a_mover)) then
            (bind ?prev_forzado ?*MOV_FORZADO*)
            (bind ?tipo (sub-string 1 1 ?pieza))
            (bind ?x (string-to-field (sub-string 2 2 ?pieza)))
            (bind ?y (string-to-field (sub-string 3 3 ?pieza)))
            (if (eq ?tipo ?*PIEZA_NORMAL*) then
                (bind ?mov (mov_pieza_normal ?x ?y ?direccion ?atacantes ?defendientes))
            else (if (eq ?tipo ?*DAMA*) then
                (bind ?mov (mov_dama ?x ?y ?atacantes ?defendientes))
            ))
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
        )
    )

    (return ?movimientos)
)

; pregunta al jugador el movimiento que quiere realizar
; devuelve una string que contiene las cordenadas de la pieza y las de
; la casilla destino
(deffunction JUEGO::pedir_mov(?blancas ?negras ?juegan_blancas ?pieza_a_mover)
    (bind ?pos_mov (movimientos ?blancas ?negras ?juegan_blancas ?pieza_a_mover))
    (while TRUE
        (print_tablero ?blancas ?negras FALSE)

        (bind ?escritos (create$))
        (foreach ?mov ?pos_mov
            (bind ?pos_origen (sub-string 1 2 ?mov))
            (if (not (in ?pos_origen ?escritos)) then
                (printout t "| " ?pos_origen " ")
                (bind ?escritos (append ?pos_origen ?escritos))
            )
        )

        (printout t crlf)
        (printout t "¿Qué pieza quieres mover? xy: ")
        (bind ?pieza (str-cat (read)))

        ; DEBUG
        (if (eq ?pieza "q") then
            (assert (salir))
            (return)
        )

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
            (foreach ?mov ?pos_mov
                (if (eq (sub-string 1 2 ?mov) ?pieza) then
                    (printout t "| " (sub-string (- (length ?mov) 1) (length ?mov) ?mov) " ")
                )
            )
            (printout t crlf)
            (printout t "¿A que posición quieres moverla? xy: ")
            (bind ?posicion (str-cat (read)))
            (if (eq (length ?posicion) 3) then
                (bind ?posicion (str-cat (sub-string 1 1 ?posicion) (sub-string 3 3 ?posicion)))
            )
              (foreach ?mov ?pos_mov
                (bind ?long (length ?mov))
                (if (and (eq (sub-string 1 2 ?mov) ?pieza)
                    (eq (sub-string (- ?long 1) ?long ?mov) ?posicion)) then
                    (return (str-cat ?mov))
                )
            )
        )
    )
)

(deffunction JUEGO::turno_jugador(?blancas ?negras ?color ?pieza_a_mover)
    (bind ?mov (pedir_mov ?blancas ?negras ?color ?pieza_a_mover))
    (aplicar_movimiento ?blancas ?negras ?mov ?color)
)

; ejecuta el turno actual. si es el turno del jugador, se le pide el movimiento
; y se aplica. si es el turno del ordenador, se devuelve FALSE.
(deffunction JUEGO::turno(?blancas ?negras ?verbose ?pieza_a_mover)
    (if (eq ?*TURNO* ?*COLOR_J*) then
        (if ?verbose then
            (printout t "=================" crlf)
            (printout t "TURNO DEL JUGADOR" crlf)
            (printout t "=================" crlf)
        )
        (turno_jugador ?blancas ?negras ?*COLOR_J* ?pieza_a_mover)
        (return TRUE)
    else
        (if ?verbose then
            (printout t "===================" crlf)
            (printout t "TURNO DEL ORDENADOR" crlf)
            (printout t "===================" crlf)
        )
        (print_tablero ?blancas ?negras FALSE)
        (return FALSE)
    )
)

(defrule JUEGO::iniciar_tablero
    (declare (salience 100))
    ?f <- (inicializacion)
=>
    (retract ?f)
    (crear_tablero)
    ; (crear_tablero_test)
)

(defrule JUEGO::findejuego
    (declare (salience 200))
    (findejuego)
    =>
    (halt)
)

(defrule JUEGO::turno_intermedio
    ; solo se crean hechos tablero_tmp cuando se hace un movimiento forzado (capturando una pieza)
    ; esta regla es para comprobar si hay más movimiento obligatorios en el
    ; mismo turno
    (declare (salience 60))
    ?t <- (tablero_tmp (blancas $?b) (negras $?n) (pieza_a_mover ?p))
    =>
    ; se calculan todos los movimientos posibles
    (movimientos $?b $?n ?*TURNO* ?p)
    (if (not ?*MOV_FORZADO*) then
        ; si no hay forzados, se crea un tablero normal y se pasa el turno
        (assert (tablero (blancas $?b) (negras $?n)))
        (cambiar_turno)
    else
        ; si hay forzados, se toma otro turno
        (turno $?b $?n FALSE ?p)
    )
    (retract ?t)
)

(defrule JUEGO::turno
    (declare (salience 50))
    ?t <- (tablero (blancas $?b) (negras $?n))
    =>
    (bind ?pos_mov (movimientos $?b $?n ?*TURNO* FALSE))
    (if (eq (length ?pos_mov) 0) then
        ; si no es posible hacer ningún movimiento, el juego ha terminado.
       (if (eq ?*TURNO* FALSE) then
           (assert(ganaronblancas))
       else
           (assert(ganaronnegras))
       )
       (printout t "Fin del juego!" crlf )
    else
        (bind ?r (turno $?b $?n TRUE FALSE))
        (if ?r then
            ; turno de jugador realizado
           (retract ?t)
           else
           ; turno del ordenador. pasar focus al módulo IA.
           (focus IA)
           (return)
        )
    )
)

(defrule JUEGO::ia_movido
    (declare (salience 60))
    ?f <- (ia_movido)
    ?t <- (tablero (blancas $?b) (negras $?n))
    =>
    (bind ?mov ?*MOV_IA*)
    (bind ?*MOV_FORZADO* FALSE)
    (bind ?*CORONADO* FALSE)
    (printout t ?mov crlf)
    (aplicar_movimiento $?b $?n ?mov (not ?*COLOR_J*))
    (bind ?*MOV_IA* FALSE)
    (retract ?f)
    (retract ?t)
)

(defrule JUEGO::ganaronblancas
    (declare(salience 101))
    ?w <- (tablero (blancas $?b) (negras $?n))
    (ganaronblancas)
    =>
    (assert(findejuego))
    (print_tablero $?b $?n FALSE)
    (printout t "Han ganado las blancas!!!" crlf)
    (retract ?w)
)

(defrule JUEGO::ganaronnegras
    (declare(salience 102))
    ?m <- (tablero (blancas $?b) (negras $?n))
    (ganaronnegras)
    =>
    (assert(findejuego))
    (print_tablero $?b $?n FALSE)
    (printout t "Han ganado las negras!!!" crlf)
    (retract ?m)
)

(defrule JUEGO::salir
    (declare (salience 90))
    (salir)
    =>
    (halt)
)

(deffacts JUEGO::inicializacion
    (inicializacion)
)

; ==============================================================================
; IA
; ==============================================================================
; módulo para el cálculo de movimientos del ordenador.
(defmodule IA (import JUEGO deftemplate tablero ia_movido)
              (import JUEGO defglobal COLOR_J MOV_FORZADO CORONADO MOV_IA DIM)
              (import JUEGO deffunction movimientos calcular_movimiento cuantas_damas heuristico in append))

(defglobal IA
    ?*CONTADOR_ID* = 0
    ?*MAX_PROF* = 6
    ?*INF* = 99999
    ?*M_INF* = -99999
)

; Hecho para representar un estado en el árbol de búsqueda
; nivel: proundidad a la que está el nodo. La raíz del árbol está en el nivel 0.
; valor: heurístico asignado al nodo.
; movimiento: movimiento realizado para pasar del nodo padre al actual.
; Un estado se considera final si su nivel es igual a la máxima profundidad o
; si su valor es distinto de FALSE.
(deftemplate IA::estado
    (slot id)
    (slot id_padre)
    (slot nivel)
    (multislot blancas)
    (multislot negras)
    (slot movimiento)
    (slot valor (default FALSE))
    (slot alfa (default ?*M_INF*))
    (slot beta (default ?*INF*))
)

; Hecho para representar un estado intermedio entre dos nodos de árbol.
; Se utiliza cuando un turno requiere más de un movimiento.
; El nivel en los estados_tmp no es el nivel del padre, sino el siguiente;
; es el nivel que tendrán los nuevos estados creados a partir del estado_tmp.
(deftemplate IA::estado_tmp
    (slot id)
    (slot id_padre)
    (slot nivel)
    (multislot blancas)
    (multislot negras)
    (slot movimiento)
    (slot pieza_a_mover)
    (slot valor (default FALSE))
)

; Estado auxiliar para guardar cada una de los posibles movimientos
; que puede realizar el ordenador junto con el valor de heurístico
; calculado para cada uno.
(deftemplate IA::pos_solucion
    (slot valor)
    (slot movimiento)
)

; Estado auxiliar para almacenar el nodo actual y los nodos visitados
; durante la búsqueda.
(deftemplate IA::control
    (slot nodo_actual (default 0))
    (multislot visitados)
)

(deffunction IA::inc_contador()
    (bind ?*CONTADOR_ID* (+ ?*CONTADOR_ID* 1))
    (return ?*CONTADOR_ID*)
)

(deffunction IA::reset_contador()
    ; se resetea a 1 porque 0 está reservado para la raiz
    (bind ?*CONTADOR_ID* 1)
    (return ?*CONTADOR_ID*)
)

(deffunction IA::reset_control()
    (assert (control))
)

; Calcula los resultados del movimiento y crea los hecho correspondientes.
(deffunction IA::aplicar_movimiento(?blancas ?negras ?mov ?color ?id_padre ?nivel ?mov_acc)
    ; se calculan los resultados del movimiento
    (bind ?resultado (calcular_movimiento ?blancas ?negras ?mov ?color))

    ; se obtienen las blancas y las negras a partir del resultado
    (bind ?index_separador (str-index "|" ?resultado))
    (bind ?nuevas_blancas (explode$ (sub-string 1 (- ?index_separador 1) ?resultado)))
    (bind ?nuevas_negras (explode$ (sub-string (+ ?index_separador 1) (length ?resultado) ?resultado)))

    ; nuevo id
    (bind ?id ?*CONTADOR_ID*)
    (inc_contador)

    ; si había movimiento parcial anterior, se tiene en cuenta
    (if ?mov_acc then
        (bind ?movimiento (str-cat (sub-string 1 (- (length ?mov_acc) 3) ?mov_acc)
                                   (sub-string 3 (length ?mov) ?mov)))
    else
        (bind ?movimiento ?mov)
    )

    ; se crean los hechos
    (if (or (= (length$ ?nuevas_blancas) 0) (= (length$ ?nuevas_negras) 0)) then
        ; si alguno de los lados está vacío
        ; el estado es final, así que se le añade heurístico
        (bind ?heur (heuristico ?nuevas_blancas ?nuevas_negras (not ?*COLOR_J*)))

        (return (assert (estado (id ?id) (id_padre ?id_padre) (nivel ?nivel) (valor ?heur)
                                (blancas ?nuevas_blancas) (negras ?nuevas_negras)
                                (movimiento ?movimiento))))

    else (if (and ?*MOV_FORZADO* (not ?*CORONADO*)) then
        ; si el movimiento ha sido forzado
        ; hay posibilidad de que haya más capturas posibles en el mismo turno
        ; se hace un estado_tmp para investigar
        (bind ?long (length ?mov))
        (bind ?pos_destino (sub-string (- ?long 1) ?long ?mov))
        (return (assert (estado_tmp (id ?id_padre) (id_padre ?id_padre) (nivel ?nivel)
                                    (blancas ?nuevas_blancas) (negras ?nuevas_negras)
                                    (pieza_a_mover ?pos_destino) (movimiento ?movimiento))))

    else
        ; turno normal. se crea el estado
        (if (= ?nivel ?*MAX_PROF*) then
            ; si se ha llegado a máxima profundidad
            ; el estado es final, así que se le añade heurístico
            (bind ?heur (heuristico ?nuevas_blancas ?nuevas_negras (not ?*COLOR_J*)))
        else
            (bind ?heur FALSE)
        )

        (return (assert (estado (id ?id) (id_padre ?id_padre) (nivel ?nivel) (valor ?heur)
                                (blancas ?nuevas_blancas) (negras ?nuevas_negras)
                                (movimiento ?movimiento))))
    ))
)

; Regla para eliminar el árbol una vez terminada la búsqueda.
(defrule IA::limpiar_arbol
    (declare (salience 210))
    (limpiar)
    ?f <- (estado)
    =>
    (retract ?f)
)

; Regla para eliminar las posibles soluciones una vez terminada la búsqueda.
(defrule IA::limpiar_sol
    (declare (salience 210))
    (limpiar)
    ?f <- (pos_solucion)
    =>
    (retract ?f)
)

; Regla para devolver focus al juego.
(defrule IA::terminado
    (declare (salience 200))
    ?f <- (limpiar)
    =>
    (retract ?f)
    ; ia_movido es un hecho del módulo JUEGO
    (assert (ia_movido))

    (focus JUEGO)
    (return)
)

; Regla para iniciar todos los procesos de la IA.
(defrule IA::inicio
    (declare (salience 0))
    ; cuando solo existe del tablero del juego
    ?t <- (tablero (blancas $?blancas) (negras $?negras))

    =>

    ; se calculan los movimientos
    (bind ?movimientos (movimientos $?blancas $?negras (not ?*COLOR_J*) FALSE))

    ; comprobacion para encontrar casos en los que solo un movimiento es posible
    ; para evitar tener que hacer un árbol de búsqueda
    (bind ?unica_posib FALSE)
    (bind ?buscar TRUE)
    (bind $?nuevas_blancas $?blancas)
    (bind $?nuevas_negras $?negras)
    (while (and (= 1 (length$ ?movimientos)) ?buscar)
        ; mientras solo un movimiento sea posible
        ; se inicializa ?unica_posib en caso de que haga falta
        (bind ?mov (nth$ 1 ?movimientos))
        (if (not ?unica_posib) then
            (bind ?unica_posib ?mov)
        )

        (if (and ?*MOV_FORZADO* (not ?*CORONADO*)) then
            ; si se podría continuar el movimiento capturando más piezas
            ; se añade la última captura a al movimiento hecho hasta ahora
            (if (not (eq ?unica_posib ?mov)) then
                (bind ?unica_posib (str-cat (sub-string 1 (- (length ?unica_posib) 3) ?unica_posib)
                                          (sub-string 3 (length ?mov) ?mov)))
            )

            ; se calculan las nuevas posiciones y los posibles movimiento
            (bind ?pieza (sub-string (- (length ?mov) 1) (length ?mov) ?mov))
            (bind ?res (calcular_movimiento $?nuevas_blancas $?nuevas_negras ?mov (not ?*COLOR_J*)))
            (bind ?index_separador (str-index "|" ?res))
            (bind $?nuevas_blancas (explode$ (sub-string 1 (- ?index_separador 1) ?res)))
            (bind $?nuevas_negras (explode$ (sub-string (+ ?index_separador 1) (length ?res) ?res)))
            (bind ?movimientos (movimientos $?nuevas_blancas $?nuevas_negras (not ?*COLOR_J*) ?pieza))
        else
            ; el siguiente movimiento sería uno normal. no se continúa, y el
            ; movimiento realizado es la unica posibilidad. no hace falta árbol.
            (bind ?buscar FALSE)
        )
    )

    ; cuando se encuentran más de una sola opción
    (if ?unica_posib then
        ; si la divergencia es obligatoria, hay que buscar. si no, no
        (bind ?buscar (and ?*MOV_FORZADO* (not ?*CORONADO*)))
    )

    (if (not ?buscar) then
        ; si solo existe un movimiento posible, no hace falta IA
        ; se realiza ese movimiento y se termina la búsqueda
        (bind ?*MOV_IA* ?unica_posib)
        (assert (limpiar))

    else
        (bind ?num_piezas (+ (length$ $?blancas) (length $?negras)))
        (if (>= ?num_piezas 13) then
            (bind ?*MAX_PROF* 3)
        else (if (>= ?num_piezas 9) then
            (bind ?*MAX_PROF* 4)
        else (if (>= ?num_piezas 5) then
            (bind ?*MAX_PROF* 5)
        else
            (bind ?*MAX_PROF* 6)
        )))
        (if (>= ?*DIM* 6) then
            ; para tablero pequeños no se tienen en cuenta las damas
            (bind ?n_damas 0)
            (foreach ?pieza (create$ $?blancas $?negras)
                (if (eq "D" (sub-string 1 1 ?pieza)) then
                    (bind ?n_damas (+ ?n_damas 1))
                )
            )
            (bind ?n_damas 0)
            (if (and (> ?n_damas 0) (> ?*DIM* 6)) then
                ; para tablero grandes se añade una dama extra
                (bind ?n_damas (+ ?n_damas 1))
            )
        )
        (bind ?*MAX_PROF* (- ?*MAX_PROF* ?n_damas))
        (bind ?*MAX_PROF* (max ?*MAX_PROF* 3))
        ; se crea el nodo raiz del árbol
        (assert (estado (id 0) (id_padre FALSE) (nivel 0) (blancas $?blancas) (negras $?negras) (movimiento FALSE)))
        (reset_contador)
        (set-strategy breadth)
    )
)

; Regla para crear nodos del árbol a partir de otros existentes.
(defrule IA::crear_arbol
    (declare (salience 30))
    ; si estamos en el proceso de crear el árbol
    (not (recorrer_arbol))
    (not (limpiar))
    (not (abortar_crear_arbol))

    ; y existe un estado que no es final (no está en prof. max. y no tiene valor)
    ?e <- (estado (id ?id) (nivel ?n) (blancas $?blancas) (negras $?negras) (valor ?valor))
    (test (and (< ?n ?*MAX_PROF*) (not ?valor)))

    =>

    (bind ?nuevo_nivel (+ ?n 1))
    ; se calcula quién realiza el movimiento (jugador/IA) dependiendo el nivel
    (if (= 0 (mod ?nuevo_nivel 2)) then
        (bind ?color ?*COLOR_J*)
    else
        (bind ?color (not ?*COLOR_J*))
    )

    ; se calculan todos los movimientos y se utilizan para crear nuevos
    ; nodos del árbol
    (bind ?movimientos (movimientos $?blancas $?negras ?color FALSE))
    (foreach ?mov ?movimientos
        (aplicar_movimiento $?blancas $?negras ?mov ?color ?id ?nuevo_nivel FALSE)
    )
)

; Regla para crear nodos del árbol a partir de estados intermedios.
(defrule IA::continuar_mov
    (declare (salience 35))
    ; si estamos en el proceso de crear el árbol
    (not (recorrer_arbol))
    (not (limpiar))

    ; y existe un estado_tmp que no está máx allá del final
    ?e <- (estado_tmp (id ?id) (id_padre ?id_padre) (nivel ?nivel) (blancas $?blancas)
                      (negras $?negras) (pieza_a_mover ?pieza) (movimiento ?movimiento))
    (test (<= ?nivel ?*MAX_PROF*))

    =>

    ; se calcula quién realiza el movimiento (jugador/IA) dependiendo el nivel
    (if (= 0 (mod ?nivel 2)) then
        (bind ?color ?*COLOR_J*)
    else
        (bind ?color (not ?*COLOR_J*))
    )

    ; se calculan todos los movimientos
    (bind ?movimientos (movimientos $?blancas $?negras ?color ?pieza))
    (if (not ?*MOV_FORZADO*) then
        ; si no hay ningún mov. forzado, se crea un tablero normal
        (bind ?movimientos_opp (movimientos $?blancas $?negras (not ?color) FALSE))
        (if (or (= ?nivel ?*MAX_PROF*) (eq 0 (length$ ?movimientos_opp))) then
            ; si se ha llegado a máxima profundidad
            ; el estado es final, así que se le añade heurístico
            (bind ?heur (heuristico $?blancas $?negras (not ?*COLOR_J*)))
        else
            (bind ?heur FALSE)
        )
        (assert (estado (id ?*CONTADOR_ID*) (id_padre ?id_padre) (nivel ?nivel)
                        (blancas $?blancas) (negras $?negras) (valor ?heur)
                        (movimiento ?movimiento)))
        (inc_contador)

    else
        ; si hay forzados, se calculan los nuevos estados
        (foreach ?mov ?movimientos
            (aplicar_movimiento $?blancas $?negras ?mov ?color ?id ?nivel ?movimiento)
        )
    )

    ; se elimina el estado_tmp
    (retract ?e)
)

; Regla para determinar que el árbol ha terminado de crearse.
(defrule IA::arbol_creado
    ; menos prioridad que el resto de reglas relacionadas con
    ; la creación del árbol
    (declare (salience 10))

    ; si estamos en el proceso de crear el árbol
    (not (recorrer_arbol))
    (not (limpiar))

    ; y existe un estado final
    (estado (nivel ?n) (valor ?valor))
    (test (not (eq ?valor FALSE )))

    =>

    ; creamos el hecho de control y pasamos a recorrer el árbol
    (reset_control)
    (assert (recorrer_arbol))
)

; Regla para profundizar en el árbol.
(defrule IA::bajar
    (declare (salience 110))
    ; si estamos en proceso de recorrer el aŕbol
    (recorrer_arbol)

    ; y el nodo actual tiene un hijo que no se ha visitado
    ?control <- (control (nodo_actual ?nodo_actual) (visitados $?visitados))
    ?actual <- (estado (id ?id_a) (alfa ?alfa) (beta ?beta))
    ?hijo <- (estado (id ?id_h) (id_padre ?id_padre) (nivel ?nivel_h))
    (test (and (eq ?id_padre ?id_a ?nodo_actual) (not (in ?id_h $?visitados))))

    =>

    (if (not (eq ?nivel_h ?*MAX_PROF*)) then
        ; si el hijo no está en la profundidad máxima
        ; el hijo se convierte en el nodo actual
        (bind $?visitados (append ?id_h $?visitados))
        (modify ?control (nodo_actual ?id_h) (visitados ?visitados))

        ; y se le transmiten el alfa y beta del padre
        (modify ?hijo (alfa ?alfa) (beta ?beta))
    )
)

; Regla para propagar el valor de un nodo a su padre.
(defrule IA::subir
    ; más prioridad que la regla para profundizar (bajar)
    (declare (salience 120))

    ; si estamos en proceso de recorrer el aŕbol
    (recorrer_arbol)

    ; y el nodo actual tiene un hijo con valor distinto de FALSE
    ?control <- (control (nodo_actual ?nodo_actual) (visitados $?visitados))
    ?actual <- (estado (id ?id_a) (id_padre ?id_abuelo) (nivel ?nivel_a) (valor ?valor_a)
                (alfa ?alfa_a) (beta ?beta_a))
    ?hijo <- (estado (id ?id_h) (id_padre ?id_padre) (nivel ?nivel_h) (valor ?valor_h) (movimiento ?mov)
                (alfa ?alfa_h) (beta ?beta_h))
    (test (not (eq ?valor_h FALSE)))
    (test (eq ?id_padre ?id_a ?nodo_actual))

    =>

    ; se comprueba si el nodo actual es un nodo de min o de max
    (bind ?max (= 0 (mod ?nivel_a 2)))

    ; se calculan los nuevos valores del nodo actual
    (if ?max then
        ; si el nodo actual es de maximizar
        (if (not ?valor_a) then
            ; el valor por defecto del nodo actual es -∞
            (bind ?valor_a ?*M_INF*)
        )
        ; el nuevo valor del nodo es el máximo entre su valor previo
        ; y el valor del hijo
        (bind ?nuevo_valor_a (max ?valor_a ?valor_h))

        ; el nuevo valor del alfa es el máximo entre su valor previo
        ; y el nuevo valor del nodo
        (bind ?nuevo_alfa_a (max ?alfa_a ?nuevo_valor_a))

        ; beta se mantiene
        (bind ?nuevo_beta_a ?beta_a)
    else
        ; si el nodo actual es de minimizar
        (if (not ?valor_a) then
            ; el valor por defecto del nodo actual es ∞
            (bind ?valor_a ?*INF*)
        )
        ; el nuevo valor del nodo es el mínimo entre su valor previo
        ; y el valor del hijo
        (bind ?nuevo_valor_a (min ?valor_a ?valor_h))

        ; alfa se mantiene
        (bind ?nuevo_alfa_a ?alfa_a)

        ; el nuevo valor del beta es el máximo entre su valor previo
        ; y el nuevo valor del nodo
        (bind ?nuevo_beta_a (min ?beta_a ?nuevo_valor_a))
    )
    (modify ?actual (valor ?nuevo_valor_a) (alfa ?nuevo_alfa_a) (beta ?nuevo_beta_a))

    (if (> ?nuevo_alfa_a ?nuevo_beta_a) then
        ; si alfa es mayor que beta
        ; se podan el resto de ramas: se omiten el resto de hijos del
        ; nodo actual y el padre del actual (abuelo del hijo) se convierte
        ; en el nuevo nodo actual
        (bind ?nodo_actual ?id_abuelo)
        (if (not ?nodo_actual) then
            (bind ?nodo_actual 0)
        )
        (modify ?control (nodo_actual ?nodo_actual))
    )

    (if (= 0 ?id_a) then
        ; si el nodo actual es el nodo raiz
        ; se crea una posible solución con el valor y el movimiento
        ; del nodo hijo
        (assert (pos_solucion (valor ?valor_h) (movimiento ?mov)))
    )

    ; se elimina el nodo hijo
    (retract ?hijo)
)

; Regla para subir el puntero al nodo actual.
(defrule IA::subir_nodo_actual
    ; menos prioridad que las reglas para profundizar (bajar) o propagar valores (subir)
    (declare (salience 100))

    ; si estamos en proceso de recorrer el aŕbol
    ?f <- (recorrer_arbol)

    ; y el nodo actual no es el nodo raiz
    ?control <- (control (nodo_actual ?nodo_actual))
    ?actual <- (estado (id ?id_a) (id_padre ?id_padre) (nivel ?nivel) (valor ?valor)
                (alfa ?alfa_a) (beta ?beta_a))
    (test (eq ?id_a ?nodo_actual))
    (test (not (eq ?id_padre FALSE)))

    =>

    ; el padre del actual se convierte en el nuevo actual
    (bind ?nodo_actual ?id_padre)
    (modify ?control (nodo_actual ?nodo_actual))
)

; Regla para determinar cuando se ha terminado la búsqueda.
(defrule IA::fin
    ; menos prioridad que el resto de reglas relacionadas con la búsqueda.
    (declare (salience 90))

    ; si estamos en proceso de recorrer el aŕbol
    ?f <- (recorrer_arbol)

    ; y el nodo actual es el nodo raiz
    ; y hay una posible solución con el mismo valor que el nodo raiz
    ?control <- (control (nodo_actual 0) (visitados $?visitados))
    ?origen <- (estado (id 0) (valor ?valor_final))
    ?solucion <- (pos_solucion (valor ?valor) (movimiento ?mov))
    (test (eq ?valor ?valor_final))

    =>

    ; se guarda el movimiento de la solución
    (bind ?*MOV_IA* ?mov)

    ; se termina la búsqueda y comienza la limpieza
    (retract ?f)
    (retract ?control)
    (assert (limpiar))
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
