;1
(deffunction suma(?a ?b)
    (+ ?a ?b))

;2
(deffunction signo(?a)
    (if (> ?a 0) then 1
    else (if (< ?a 0) then -1
    else 0))
)

;2 (alt)
(deffunction signo-alt(?a)
    (str-compare (str-cat ?a) "0")
)

;3
(deffunction minimoSimple(?a ?b)
    (if (> ?a ?b) then ?b
    else ?a)
)

;4
(deffunction maximoSimple(?a ?b)
    (if (> ?a ?b) then ?a
    else ?b)
)

;5
(deffunction iguales(?a ?b)
    (= ?a ?b)
)

;6
(deffunction colores(?color)
    (switch ?color
        (case "verde" then
            (printout t "Puedes Pasar" crlf))
        (case "rojo" then
            (printout t "No puedes cruzar" crlf))
        (case "amarillo" then
            (printout t "Parate por seguridad" crlf))
    )
)

;7
(deffunction cierto(?symbol)
    (or (= 0 (str-compare ?symbol s)) (= 0 (str-compare ?symbol si)))
)

;8
(deffunction ciertoTeclado()
    (bind ?var (read))
    (or (= 0 (str-compare ?var s)) (= 0 (str-compare ?var si)))
)
