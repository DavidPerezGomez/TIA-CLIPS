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

; como el in de python
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
  (printout t "El numero no esta dentro del rango. Introduce otro: ")
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
(deffunction cartesiano (?a ?b)
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

  (progn$ (?aitem ?a)
    (progn$ (?bitem ?b)
      (bind ?result (insert$ ?result (+ (length$ ?result) 1) ?aitem))
      (bind ?result (insert$ ?result (+ (length$ ?result) 1) ?bitem))
    )
  )
  (return ?result)
)

;10
;(escalar (create$ 2 3)(create$ 4 5)) → 2*4+3*5 → 23
(deffunction escalar (?a ?b)
  (if
    (or
      (not(multifieldp ?a))
      (not(multifieldp ?b))
      (= 0 (length ?a))
      (= 0 (length ?b))
      (not(= (length$ ?a) (length ?b)))
      (not(allIntOrFloat ?a))
      (not(allIntOrFloat ?b))
    )
  then
  (printout t "Introduce 2 variables multicampo no vacías y de igual longitud compuestas de numeros." crlf)
  (return)
  )

  (bind ?longitud (length$ ?a))
  (bind ?contador 1)
  (bind ?result 0)

  (while (<= ?contador ?longitud)
    (bind ?result (+ ?result

                            (*
                              (nth$ ?contador ?a)
                              (nth$ ?contador ?b)
                            )

                  )
    )
    (bind ?contador (+ ?contador 1))
  )
  (return ?result)
)
