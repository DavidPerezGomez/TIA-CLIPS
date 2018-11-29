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

; estado de tablero para multiples movimientos
(deftemplate JUEGO::tablero_tmp
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
    (bind $?tablero " ")
    (while (> ?v 0)
        (loop-for-count (?i 1 ?*DIM*)
            (bind ?tablero (str-cat ?tablero (str-cat "N" ?v ?i) " ")))
            (bind ?v (- ?v 1))
    )
    (bind ?tablero (explode$ ?tablero))
(loop-for-count(?a 1 ?longnegras)
(bind ?pos (member$ (nth$ ?a ?negras) ?tablero))
(bind ?tablero (replace$ ?tablero ?pos ?pos "X"))
)
(loop-for-count(?a 1 ?longblancas)
(bind ?pos (member$ (nth$ ?a ?blancas) ?tablero))
(bind ?tablero (replace$ ?tablero ?pos ?pos "O"))
)
(bind ?longtab (length$ ?tablero))
(bind ?m ?*DIM*)
(bind ?h 1)
(while (> ?m 0)
    (loop-for-count (?j 1 ?*DIM*)
        (bind ?var (nth$ ?h ?tablero))
        (if (eq ?var "X") then
        (printout t "X ")
        else
        (if(eq ?var "O") then
        (printout t "O ")
        else
        (printout t "_ "))
        )
        (bind ?h (+ ?h 1))
        )
        (bind ?m (- ?m 1))
    (printout t crlf)
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

; aplica el movimiento ?mov al tablero formado por ?blancas y ?negras
; genera un nuevo par de vectores de blancas y negras y los utiliza
; para crear un nuevo estado tablero
; devuelve el identificador del nuevo estado
(deffunction JUEGO::aplicar_movimiento(?blancas ?negras ?mov ?color)
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
    ; TODO esto de buscar la pieza igual puedo extraerlo a una función suelta
    (loop-for-count (?i 1 (length$ ?aliadas))
        (bind ?pieza (nth$ ?i ?aliadas))
        (bind ?tipo (sub-string 1 1 ?pieza))
        (bind ?pos (sub-string 2 3 ?pieza))
        ; si las posiciones son iguales
        (if (eq ?pos ?pos_origen) then
            ; creamos la nueva pieza después de haber sido movida
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

    ; se crea el tablero con las nuevas piezas
    (if ?*MOV_FORZADO* then
        ; si alguno de los movimientos ha sido forzado, hay posibilidad de que
        ; haya más capturas posibles en el mism turno.
        ; se hace un tablero_tmp para investigar
        (return (assert (tablero_tmp (blancas ?nuevas_blancas) (negras ?nuevas_negras))))
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
    (foreach ?horizontal (create$ 1 -1)
        (foreach ?vertical (create$ 1 -1)
            (bind ?diagonal (create$))
            (bind ?capturada FALSE)
            (bind ?pos_x ?x)
            (bind ?pos_y ?y)
            (while TRUE
                (bind ?pos_x (+ ?pos_x ?horizontal))
                (bind ?pos_y (+ ?pos_y ?vertical))
                (if (not (is_in_board ?pos_x ?pos_y)) then
                    ; si está fuera del tablero, pasamos a la siguiente diagonal
                    (break)
                else
                    ; creamos las posibles piezas que podrían estar en esa posición
                    (bind ?posibles_piezas (create$
                        (sym-cat ?*PIEZA_NORMAL* ?pos_x ?pos_y)
                        (sym-cat ?*DAMA* ?pos_x ?pos_y)))
                    (bind ?ocupada FALSE)
                    ; si hay alguna posible pieza enemiga
                    (foreach ?posible_pieza ?posibles_piezas
                        (if (in ?posible_pieza ?defendientes) then
                            (bind ?ocupada TRUE)
                            (bind ?capturada ?posible_pieza)
                            (break)
                        )
                    )
                    (if ?ocupada then
                        ; si hay una pieza enemiga
                        (if ?capturada then
                            ; ya se ha pasado por encima de una enemiga
                            ; se pasa a la siguiente diagonal
                            (bind ?mov (append ?diagonal ?mov))
                            (break)
                        ; else
                            ; si es la primera enemiga que nos encontramos
                            ; guardamos la posición de la pieza enemiga que
                            ; estaríamos capturando (?capturada)
                        )
                    else
                        ; si no hay una enemiga, se mira en las aliadas
                        (bind ?ocupada FALSE)
                        (foreach ?posible_pieza ?posibles_piezas
                            (if (in ?posible_pieza ?atacantes) then
                                (bind ?ocupada TRUE)
                                (break)
                            )
                        )
                        (if ?ocupada then
                            ; si hay una aliada, se pasa a la siguiente diagonal
                            (bind ?mov (append ?diagonal ?mov))
                            (break)
                        else
                            ; si está vacía
                            (if (not ?capturada) then
                                ; si no se ha capturado ninguna enemiga
                                ; se añade si no hay algún movimiento forzado
                                (if (not ?*MOV_FORZADO*) then
                                    (bind ?diagonal (append (str-cat ?pos_x ?pos_y) ?diagonal))
                                )
                            else
                                ; si se ha capturado alguna enemiga
                                (if (not ?*MOV_FORZADO*) then
                                    (bind ?*MOV_FORZADO* TRUE)
                                    (bind ?mov (create$))
                                    (bind ?diagonal (create$))
                                )
                                ; se añade a la diagonal
                                (bind ?diagonal (append
                                    (str-cat (sub-string 2 3 ?capturada) " " ?pos_x ?pos_y) ?diagonal))
                            )
                        )
                    )
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
    (return ?movimientos)
)

; pregunta al jugador el movimiento que quiere realizar
; devuelve una string que contiene las cordenadas de la pieza y las de
; la casilla destino
; >
(deffunction JUEGO::pedir_mov(?blancas ?negras ?juegan_blancas)
    (bind ?pos_mov (movimientos ?blancas ?negras ?juegan_blancas))
    (while TRUE
        (print_tablero ?blancas ?negras)
        (foreach ?mov ?pos_mov
            (printout t "| " (sub-string 1 2 ?mov) " ")
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
                (if (eq (sub-string (- ?long 1) ?long ?mov) ?posicion) then
                    (return (str-cat ?mov))
                )
            )
        )
    )
)

(deffunction JUEGO::turno_jugador(?blancas ?negras ?color)
    (bind ?mov (pedir_mov ?blancas ?negras ?color))
    (aplicar_movimiento ?blancas ?negras ?mov ?color)
    (printout t ?mov crlf)
)

(deffunction JUEGO::turno_ia(?blancas ?negras ?color)
    (turno_jugador ?blancas ?negras ?color)
)

(deffunction JUEGO::turno(?blancas ?negras ?verbose)
    (if (eq ?*TURNO* ?*COLOR_J*) then
        (if ?verbose then
            (printout t "=================" crlf)
            (printout t "TURNO DEL JUGADOR" crlf)
            (printout t "=================" crlf)
        )
        (turno_jugador ?blancas ?negras ?*COLOR_J*)
    else
        (if ?verbose then
            (printout t "===================" crlf)
            (printout t "TURNO DEL ORDENADOR" crlf)
            (printout t "===================" crlf)
        )
        (turno_ia ?blancas ?negras (not ?*COLOR_J*))
    )
)

(defrule JUEGO::iniciar_tablero
    (declare (salience 100))
    ?f <- (inicializacion)
=>
    (retract ?f)
    (crear_tablero)
)

(defrule JUEGO::turno_intermedio
    ; solo se crean hechos tablero_tmp cuando se hace un movimiento forzado (capturando una pieza)
    ; esta regla es para comprobar si hay más movimiento obligatorios en el
    ; mismo turno
    (declare (salience 60))
    ?t <- (tablero_tmp (blancas $?b) (negras $?n))
    =>
    ; se calculan todos los movimientos posibles
    (movimientos $?b $?n ?*TURNO*)
    (if (not ?*MOV_FORZADO*) then
        ; si no hay forzados, se crea un tablero normal y se pasa el turno
        (assert (tablero (blancas $?b) (negras $?n)))
        (cambiar_turno)
    else
        ; si hay forzados, se toma otro turno
        (turno $?b $?n FALSE)
    )
    (retract ?t)
)

(defrule JUEGO::turno
    (declare (salience 50))
    ?t <- (tablero (blancas $?b) (negras $?n))
    =>
    (turno $?b $?n TRUE)
    (retract ?t)
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
