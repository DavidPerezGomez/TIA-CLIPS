
;0
(deffunction isIntOrFloat (?n)
    (or (integerp ?n) (floatp ?n))
)


(deffunction allIntOrFloat (?v)
  (bind ?result TRUE)
  (foreach ?item ?v
    (if (not (isIntOrFloat ?item)) then
      (bind ?result FALSE)
      (break)
    )
  )
  (return ?result)
)

(deffunction isNaturalNumber(?a)
    (and (integerp ?a) (>= ?a 0))
)

; devuelve de cuantas cifras esta formada el numero dado
(deffunction digits(?num)
    (bind ?i 1)
    (while (not (= 0 (div ?num (** 10 ?i))))
        (bind ?i (+ ?i 1))
    )
    (return ?i)
)

; añade un elemento al final del multicampo
(deffunction append(?a $?vector)
    (return (insert$ ?vector (+ 1 (length $?vector)) ?a))
)

; como el in de python
; es para saber si un elemento esta dentro de un multicampo
(deffunction in(?a ?b)
  (if (not(multifieldp ?b)) then
    (printout t "La segunda variable tiene que ser un multicampo." crlf)
    (return)
  )

  (if (member$ ?a ?b) then
    (return TRUE)
    else (return FALSE)
  )
)

;1
(deffunction dentroDelRango (?nmin ?nmax)
  (if
    (or
      (not(isIntOrFloat ?nmin))
      (not(isIntOrFloat ?nmax))
    )
  then
  (printout t "Introduce un rango de numeros." crlf)
  (return)
  )

  ; coger el pequeño y el mayor
  (if (> ?nmin ?nmax) then
    (bind ?naux ?nmin)
    (bind ?nmin ?nmax)
    (bind ?nmax ?naux)
  )

  (printout t "Introduce un numero: ")
  (bind ?n (read))
  (while
    (or
      (not(isIntOrFloat ?n))
      (not(> ?n ?nmin))
      (not(< ?n ?nmax))
    )
  (printout t "No esta dentro del rango. Introduce un numero: ")
  (bind ?n (read))
  )

  (printout t "Esta dentro del rango." crlf)
)

;2
(deffunction acertijo(?nmin ?nmax)
  (if
    (or
      (not(integerp ?nmin))
      (not(integerp ?nmax))
    )
  then
  (printout t "Introduce numeros enteros." crlf)
  (return)
  )

  ; coger el pequeño y el mayor
  (if (> ?nmin ?nmax) then
    (bind ?naux ?nmin)
    (bind ?nmin ?nmax)
    (bind ?nmax ?naux)
  )

  (bind ?objetivo (random ?nmin ?nmax))

  (printout t "Introduce un numero: ")
  (bind ?intento (read))

  (while
    (or
      (not (integerp ?intento))
      (!= ?intento ?objetivo)
    )

  (printout t "MAL. Introduce un numero: ")
  (bind ?intento (read))
  )

  (printout t "Que bien has acertado." crlf)
)


;3
(deffunction mcd (?a ?b)
  (if
    (or
      (not(integerp ?a))
      (not(integerp ?b))
    )
  then
  (printout t "Introduce numeros enteros." crlf)
  (return)
  )

  (if (= ?a ?b) then
    (return ?a)
  else
    (if (> ?a ?b) then
      (mcd (- ?a ?b) ?b)
    else
      (mcd ?a (- ?b ?a))
    )
  )
)

;4
;mcm (a,b)= (a*b)/mcd(a,b)

(deffunction mcm(?a ?b)
  (if
    (or
      (not(integerp ?a))
      (not(integerp ?b))
    )
  then
  (printout t "Introduce numeros enteros." crlf)
  (return)
  )

  (/ (* ?a ?b) (mcd ?a ?b))
)

;5
(deffunction mes(?n)
  (if
    (or
      (not(integerp ?n))
      (<= ?n 0)
      (> ?n 12)
    )
  then
  (printout t "Introduce un numero del 1 al 12" crlf)
  (return)
  )

  (bind ?m31 (create$ 1 3 5 7 8 10 12))
  (bind ?m30 (create$ 4 6 9 11))
  (bind ?m28 (create$ 2))

  (if (in ?n ?m31) then
    (return 31)
  )

  (if (in ?n ?m30) then
    (return 30)
  )

  (if (in ?n ?m28) then
    (return 28)
  )
)

;6
(deffunction diferencia(?a ?b)
;funciona con numeros, simbolos, strings...
  (if
    (or
      (not(multifieldp ?a))
      (not(multifieldp ?b))
    )
  then
  (printout t "Introduce 2 variables multicampo." crlf)
  (return)
  )

  (bind ?result (create$))

  (progn$ (?item ?a)
    (if (not (in ?item ?b)) then
      (bind ?result (insert$ ?result 1 ?item))
    )
  )

  (progn$ (?item ?b)
    (if (not (in ?item ?a)) then
      (bind ?result (insert$ ?result 1 ?item))
    )
  )

  (return ?result)
)

;7
(deffunction concatenacion(?a ?b)
  (if
    (or
      (not(multifieldp ?a))
      (not(multifieldp ?b))
    )
  then
  (printout t "Introduce 2 variables multicampo." crlf)
  (return)
  )

  (progn$ (?item ?b)
    (bind ?a (insert$ ?a (+ (length$ ?a) 1) ?item))
  )

  (return ?a)
)

;8
;(sustituir a b (create$ b a e)) → (b b e)
(deffunction sustituir (?objetivo ?nuevo ?v)
;(replace$ <expresiónMultiC><inicio><fin><PorEstaExpresiónMultiC|Simple>)
  (bind ?posiciones (create$))
  (bind ?contador 1)
  (progn$ (?item ?v)
    (if (= (str-compare ?item ?objetivo) 0) then
      (bind ?posiciones (insert$ ?posiciones (+ (length$ ?posiciones) 1) ?contador))
    )
    (bind ?contador (+ ?contador 1))
  )

  (progn$ (?item ?posiciones)
    (bind ?v (replace$ ?v ?item ?item ?nuevo))
  )
  (return ?v)
)

;9
;(cartesiano (create$ a b)(create$ c d)) → (a c a d b c b d)
(deffunction cartesiano(?a ?b)
    (if (or (not(multifieldp ?a)) (not(multifieldp ?b)))
    then (printout t "Introduce 2 variables multicampo." crlf)
       (return)
    )
    (bind ?res (create$))
    (foreach ?item_a ?a
        (foreach ?item_b ?b
            (bind $?res (append ?item_a $?res))
            (bind $?res (append ?item_b $?res))
        )
    )
    (return ?res)
)

;10
;(escalar (create$ 2 3)(create$ 4 5)) → 2*4+3*5 → 23
(deffunction escalar(?a ?b)
;si se dan 2 multicampo vacíos se asume que son vectores nulos y su producto escalar sera cero.
    (if (or (not(multifieldp ?a)) (not(multifieldp ?b)))
    then (printout t "Introduce 2 variables multicampo." crlf)
       (return)
    )
    (if (not (= (length$ ?a) (length ?b)))
    then (printout t "Los multicampos deben ser del mismo tamaño." crlf)
       (return)
    )
    (bind ?res 0)
    (loop-for-count (?i 1 (length$ ?a))
        (bind ?tmp_a (nth$ ?i ?a))
        (bind ?tmp_b (nth$ ?i ?b))
        (if (not (and (isIntOrFloat ?tmp_a) (isIntOrFloat ?tmp_b)))
        then (printout t "Todos los valores deben ser números." crlf)
             (return)
        )
        (bind ?res (+ ?res (* ?tmp_a ?tmp_b)))
    )
    (return ?res)
)

;11
(deffunction merge(?v1 ?v2)
    ; merges two separated ordered lists
    (bind ?res (create$))
    (bind ?i 1)
    (bind ?j 1)
    (while (not (and (> ?i (length$ ?v1)) (> ?j (length$ ?v2))))
        (bind ?min 0)
        (if (> ?i (length$ ?v1))
        then (bind ?min (nth$ ?j ?v2))
             (bind ?j (+ 1 ?j))
        else (if (> ?j (length$ ?v2))
             then (bind ?min (nth$ ?i ?v1))
                  (bind ?i (+ 1 ?i))
             else (if (< (nth$ ?i ?v1) (nth$ ?j ?v2))
                  then (bind ?min (nth$ ?i ?v1))
                       (bind ?i (+ 1 ?i))
                   else (bind ?min (nth$ ?j ?v2))
                        (bind ?j (+ 1 ?j))
                  )
             )
        )
        (bind ?res (append ?min ?res))
    )
    (return ?res)
)

(deffunction mergesort($?vector)
    (foreach ?item ?vector
        (if (not (isIntOrFloat ?item))
        then (printout t "Todos los valores deben ser numeros." crlf)
             (return)
        )
    )
    (bind ?lists_size 1)
    (bind ?new_v ?vector)
    (while (< ?lists_size (length ?vector))
        (bind ?v ?new_v)
        (bind ?new_v (create$))
        (bind ?i 1)
        (while (<= ?i (length ?v))
            (bind ?a (subseq$ ?v ?i (+ ?i ?lists_size -1)))
            (bind ?i (+ ?i ?lists_size))
            (bind ?b (subseq$ ?v ?i (+ ?i ?lists_size -1)))
            (bind ?i (+ ?i ?lists_size))
            (bind ?new_v (append (merge ?a ?b) ?new_v))
        )
        (bind ?lists_size (* 2 ?lists_size))
    )
    (return ?new_v)
)

;(unico (create$ 1 3 4 2)) → TRUE
;(unico (create$ 1 3 4)) → FALSE
(deffunction unico($?vector)
    (foreach ?item ?vector
        (if (not (isIntOrFloat ?item))
        then (printout t "Todos los valores deben ser numeros." crlf)
             (return)
        )
    )
    (bind ?target (create$))
    (loop-for-count (?i 1 (length ?vector))
        (bind ?target (append ?i ?target))
    )
    (return (=  0 (str-compare (implode$ (mergesort ?vector)) (implode$ ?target))))
)

;12
(deffunction isPrime(?num)
    (bind ?root (integer (sqrt ?num)))
    (bind ?prime TRUE)
    (if (or (= ?num 0) (= ?num 1)) then
        (return FALSE)
    )
    (loop-for-count (?i 2 ?root)
        (if (= 0 (mod ?num ?i)) then
            (bind ?prime FALSE)
            (break)
        )
    )
    (return ?prime)
)

(deffunction isCapi(?num)
    (bind ?digits (digits ?num))
    (bind ?string (str-cat ?num))
    (bind ?top (sub-string 1 (div ?digits 2) ?string))
    (bind ?bottom (sub-string (- (+ ?digits 1) (div ?digits 2)) ?digits ?string))
    (bind ?capi TRUE)
    (loop-for-count (?i 1 (div ?digits 2))
        (bind ?j (- (+ (div ?digits 2) 1) ?i))
        (bind ?d1 (sub-string ?i ?i ?top))
        (bind ?d2 (sub-string ?j ?j ?bottom))
        (if (not (= 0 (str-compare ?d1 ?d2))) then
            (bind ?capi FALSE)
            (break)
        )
    )
    (return ?capi)
)

(deffunction num_primos_y_capicua()
    (printout t "Cuantos numeros quieres comprobar?" crlf)
    (bind ?num (read))
    (if (not (isNaturalNumber ?num)) then
        (printout t "Input no valido." crlf)
        (return)
    )
    (bind ?ya_calculados 0)

    (bind ?i 0)
    (while (< ?ya_calculados ?num)
        (if (and (isPrime ?i) (isCapi ?i)) then
            (printout t ?i " ")
            (bind ?ya_calculados (+ ?ya_calculados 1))
        )
        (bind ?i (+ ?i 1))
    )
    (printout t "" crlf)
    (return)
)

;13
(deffunction num_magico()
    (printout t "Inroduce el numero: ")
    (bind ?num (read))
    (if (not (isNaturalNumber ?num)) then
        (printout t "Input no valido." crlf)
        (return)
    )
    (bind ?res ?num)
    (while (not (and (not (= 0 ?res)) (= 0(div ?res 10))))
    ; salir cuando res!=0 y res sea de un dígito (res / 10 = 0)
        (bind ?tmp 0)
        (bind ?d 1)
        (bind ?max_pow (digits ?res))
        (loop-for-count (?pow 0 ?max_pow)
            (bind ?d (mod (div ?res (** 10 ?pow)) 10))
            (bind ?tmp (+ ?tmp ?d))
        )
        (bind ?res ?tmp)
    )
    (return ?res)
)

; 14
(deffunction esMedio(?num)
    (if (not (isNaturalNumber ?num)) then
        (printout t "Input no valido." crlf)
        (return)
    )
    (bind ?below 0)
    (bind ?above 0)
    (loop-for-count (?i 1 (- ?num 1))
        (bind ?below (+ ?below ?i))
    )
    (bind ?j (+ ?num 1))
    (while (< ?above ?below)
        (bind ?above (+ ?above ?j))
        (bind ?j (+ ?j 1))
    )
    (printout t "below: " ?below crlf)
    (printout t "above: " ?above crlf)
    (return (= ?above ?below))
)
