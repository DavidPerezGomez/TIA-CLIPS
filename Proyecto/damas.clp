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
    ?*CORONADO* = FALSE
    ?*SYMB_PEON_B* = "o" ; simbolo para las blancas
    ?*SYMB_PEON_N* = "x" ; simbolo para las negras
    ?*SYMB_DAMA_B* = "O" ; simbolo para las damas blancas
    ?*SYMB_DAMA_N* = "X" ; simbolo para las damas blancas
    ?*SYMB_G_PEON_B* = " o " ; simbolo para las blancas
    ?*SYMB_G_PEON_N* = " x " ; simbolo para las negras
    ?*SYMB_G_DAMA_B* = "oOo" ; simbolo para las damas blancas
    ?*SYMB_G_DAMA_N* = "xXx" ; simbolo para las damas blancas
    ?*SYM_V* = " " ; simbolo para el vacio
    ?*DEBUG* = TRUE
)

; estado actual del tablero
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

; 8| | | | | | | | |
; 7| | | | | | | | |
; 6| | | | | | | | |
; 5| | | | | |x| | |
; 4| | |x| | | | | |
; 3| |o| | | |x| | |
; 2|o| | | | | |o| |
; 1| | | | | | | |o|
; 0 1 2 3 4 5 6 7 8
(deffunction JUEGO::crear_tablero_test()
    (bind ?negras "N34 N63 N65")
    (bind ?blancas "N12 N23 N72 N81")
    ; Cambiar las fichas a multicampos
    (bind ?negras (explode$ ?negras))
    (bind ?blancas (explode$ ?blancas))
    (assert(tablero (blancas ?blancas) (negras ?negras)))
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

(deffunction JUEGO::print_tablero_grande(?blancas ?negras)
    (loop-for-count (?i 0 ?*DIM*)
        (bind ?fila (- ?*DIM* ?i))
        (printout t "   ---------------------------------" crlf)
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

(deffunction JUEGO::heuristco(?blancas ?negras ?color)
    (bind ?heuristco -1)
    ; criterios y su ponderación:
    ; valor de las piezas
    (bind ?pond_piezas 1)
    (bind ?val_peon 3)
    (bind ?val_dama 5)
    ; posicionamiento de peones
    (bind ?pond_pos_peones 1)
    ; posicionamiento de las damas
    (bind ?pond_pos_damas 1)
    ; componente aleatorios
    ; (bind ?rand (random -10 10))
    (bind ?pond_rand 0.1)
)

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
                    (and ?color (= 8 (string-to-field (sub-string 2 2 ?pos_destino))))
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
        ; si no se ha encontrado, se asume error y se sale del método
        (return FALSE)
    )
    (if (> (length$ (explode$ ?mov)) 2) then
        (bind ?pos_capturada (sub-string 4 5 ?mov))
        (bind ?encontrada FALSE)
        (bind ?index 0)
        ; iterar las enemigas buscando la pieza que se ha capturado
        (loop-for-count (?i 1 (length$ ?enemigas))
            (bind ?pieza (nth$ ?i ?enemigas))
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
            (bind ?nuevas_enemigas (delete$ ?enemigas ?index ?index))
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
            ; los moviminetos de la diagnoanal
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
; >
(deffunction JUEGO::pedir_mov(?blancas ?negras ?juegan_blancas ?pieza_a_mover)
    (bind ?pos_mov (movimientos ?blancas ?negras ?juegan_blancas ?pieza_a_mover))
   (if (eq (length ?pos_mov) 0) then
   (if (eq ?juegan_blancas FALSE ) then
   (assert(ganaronblancas))
   (return)
   else
   (assert(ganaronnegras))
   (return)
   )
   (printout t " fin del juego" crlf )
   (assert(findejuego))
   (return)
   )

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

(deffunction JUEGO::turno_ia(?blancas ?negras ?color ?pieza_a_mover)
    (turno_jugador ?blancas ?negras ?color ?pieza_a_mover)
)

(deffunction JUEGO::turno(?blancas ?negras ?verbose ?pieza_a_mover)
    (if (eq ?*TURNO* ?*COLOR_J*) then
        (if ?verbose then
            (printout t "=================" crlf)
            (printout t "TURNO DEL JUGADOR" crlf)
            (printout t "=================" crlf)
        )
        (turno_jugador ?blancas ?negras ?*COLOR_J* ?pieza_a_mover)
    else
        (if ?verbose then
            (printout t "===================" crlf)
            (printout t "TURNO DEL ORDENADOR" crlf)
            (printout t "===================" crlf)
        )
        (turno_ia ?blancas ?negras (not ?*COLOR_J*) ?pieza_a_mover)
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

(defrule JUEGO::findejuego
    (declare (salience 100))
    (findejuego)
    =>
    (halt)
)

(defrule JUEGO::turno
    (declare (salience 50))
    ?t <- (tablero (blancas $?b) (negras $?n))
    =>
    (turno $?b $?n TRUE FALSE)
    (retract ?t)
)

(defrule JUEGO::ganaronblancas
    (declare(salience 101))
    ?w <- (tablero (blancas $?b) (negras $?n))
    (ganaronblancas)
    =>
    (assert(findejuego))
    (printout t " han ganado las blancas")
    (retract ?w)
    (printout t " han ganado las blancas")
)

(defrule JUEGO::ganaronnegras
    (declare(salience 102))
    ?m <- (tablero (blancas $?b) (negras $?n))
    (ganaronnegras)
    =>
    (assert(findejuego))
    (printout t " han ganado las negras")
    (retract ?m)
    (printout t " han ganado las negras")
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
