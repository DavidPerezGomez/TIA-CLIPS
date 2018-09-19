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
