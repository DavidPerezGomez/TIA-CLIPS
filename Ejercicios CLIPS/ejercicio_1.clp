;TODO comprobar la validez de los inputs (integers)

(deffunction pedir_num()
    (printout t "Introduce un número: (sin trampas plz)" crlf)
    (integer (read))
)

(deffunction confirmar()
    (printout t "¿Seguir sumando? (s/n)" crlf)
    (bind ?a (read))
    (= 0 (str-compare ?a "s"))
)

(deffunction seguir_sumando()
    (bind ?acc 0)
    (while (confirmar) do
        (bind ?acc (+ ?acc (pedir_num)))
        (printout t "Suma total: " ?acc crlf)
    )
)
