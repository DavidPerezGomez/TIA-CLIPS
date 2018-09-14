;0
(deffunction isValidNumber(?a)
    (or (integerp ?a) (floatp ?a))
)

;1
(deffunction suma(?a ?b)
    (if (and (isValidNumber ?a) (isValidNumber ?b))
    then (+ ?a ?b)
    else (printout t "Los parámetros no son correctos." crlf)
    )
)

;2
(deffunction signo(?a)
    (if (isValidNumber ?a)
    then (if (> ?a 0) then 1
         else (if (< ?a 0) then -1
              else 0))
    else (printout t "El parámetro no es correcto." crlf)
    )
)

;2 (alt)
(deffunction signo_alt(?a)
    (if (isValidNumber ?a)
    then (str-compare (str-cat ?a) "0")
    else (printout t "El parámetro no es correcto." crlf)
    )
)

;3
(deffunction minimoSimple(?a ?b)
    (if (and (isValidNumber ?a) (isValidNumber ?b))
    then (if (> ?a ?b) then ?b
         else ?a)
    else (printout t "Los parámetros no son correctos." crlf)
    )
)

;4
(deffunction maximoSimple(?a ?b)
    (if (and (isValidNumber ?a) (isValidNumber ?b))
    then (if (> ?a ?b) then ?a
         else ?b)
    else (printout t "Los parámetros no son correctos." crlf)
    )
)

;5
(deffunction iguales(?a ?b)
    (if (and (isValidNumber ?a) (isValidNumber ?b))
    then (= ?a ?b)
    else (printout t "Los parámetros no son correctos." crlf)
    )
)

;6
(deffunction colores(?color)
    (switch (lowcase (str-cat ?color))
        ;str-cat el input para tener en cuenta símbolos también
        (case "verde" then
            (printout t "Puedes Pasar" crlf))
        (case "rojo" then
            (printout t "No puedes cruzar" crlf))
        (case "amarillo" then
            (printout t "Parate por seguridad" crlf))
        (default then
            (printout t "Input no válido." crlf))
    )
)

;7
(deffunction cierto(?symbol)
    (or (= 0 (str-compare ?symbol s)) (= 0 (str-compare ?symbol si)))
)

;8
(deffunction ciertoTeclado()
    (bind ?var (read))
    (cierto ?var)
)
