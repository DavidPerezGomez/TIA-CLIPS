(deftemplate persona
    (slot nombre (type LEXEME))
    (slot ciudad (type LEXEME))
)

(deftemplate actividad
    (slot nombre (type LEXEME))
    (slot ciudad (type LEXEME))
    (slot duracion (type NUMBER))
)

(defrule R1
    (declare (salience 20))
    (persona (nombre ?n) (ciudad ?c))
    ?act <- (actividad (nombre ?v) (ciudad ?c) (duracion ?d1))
    ?prev <- (cuenta ?n ?c ?d2 ?val)
    =>
    (retract ?act)
    (retract ?prev)
    (assert (cuenta ?n ?c (+ ?d2 ?d1) (+ ?val 1)))
)

(defrule R2
    (declare (salience 10))
    (persona (nombre ?n) (ciudad ?c))
    ?act <- (actividad (nombre ?v) (ciudad ?c) (duracion ?d))
    =>
    (retract ?act)
    (assert (cuenta ?n ?c ?d 1))
)

(defrule R3
    (declare (salience 20))
    (persona (nombre ?n) (ciudad ?c))
    ?prev <- (cuenta ?n ?c ?d ?val)
    =>
    (bind ?avg (/ ?d ?val))
    (printout t ?n ", " ?c ": " ?avg " (" ?d ", " ?val ")" crlf)
    (retract ?prev)
)

(deffacts personas
	(persona (nombre Juan) (ciudad Paris))
	(persona (nombre Ana) (ciudad Edimburgo)))

(deffacts actividades
	(actividad (nombre Torre_Eiffel) (ciudad Paris) (duracion 2))
	(actividad (nombre Castillo_de_Edimburgo) (ciudad Edimburgo) (duracion 5))
	(actividad (nombre Louvre) (ciudad Paris) (duracion 6))
	(actividad (nombre Montmartre) (ciudad Paris) (duracion 1))
	(actividad (nombre Royal_Mile) (ciudad Edimburgo) (duracion 3)))
