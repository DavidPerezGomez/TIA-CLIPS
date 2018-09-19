(deffunction intLength (?n)
  (bind ?n (str-cat ?n))
  (return (str-length ?n))
)


(deffunction once (?premiado ?consulta)
  (if (and (integerp ?premiado) (integerp ?consulta)) then
      (if (= ?premiado ?consulta) then
        (return 100000)
      )
      (if (= (mod ?premiado 1000) (mod ?consulta 1000)) then
        (return 50000)
      )
      (if (= (mod ?premiado 100) (mod ?consulta 100)) then
        (return 3)
      )
      (printout t "No hay premio para ti." crlf)
  else (printout t "El numero introducido no es corecto." crlf)
  )
)

; hora minuto segundo, comprobar que esta bien y si lo esta devolver hora mas 1 seg



(deffunction hora (?v)
  (bind ?hora (integer(nth$ 1 ?v)))
  (bind ?min (integer(nth$ 2 ?v)))
  (bind ?seg (integer(nth$ 3 ?v)))
  (if (or
        (< ?hora 0)
        (> ?hora 23)
      )
  then (printout t "La hora no es correcta" crlf)
  (return)
  )

  (if (or
        (< ?min 0)
        (> ?min 59)
      )
  then (printout t "Los minutos no son correctos" crlf)
  (return)
  )

  (if (or
        (< ?seg 0)
        (> ?seg 59)
      )
  then (printout t "Los segundos no son correctos" crlf)
  (return)
  )

  (bind ?seg (+ ?seg 1))

  (if (= ?seg 60) then
    (bind ?seg 0)
    (bind ?min (+ ?min 1))
  )

  (if (= ?min 60) then
    (bind ?min 0)
    (bind ?hora (+ ?hora 1))
  )

  (if (= ?hora 24) then
    (bind ?hora 0)
  )

  (return (create$ ?hora ?min ?seg))
)
