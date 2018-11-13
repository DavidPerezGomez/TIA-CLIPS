(defrule imprimirResultado
	?a<-(result $?x)
	?b<-(min ?y)
=>
	(printout t $?x crlf)
	(retract ?a)
	(retract ?b)
)

(defrule insertarEliminarMinimo
	(declare (salience 20))
	?a<-(hecho1 ?x)
	?b<-(min ?y)
	?c<-(result $?z)
	(test (= ?x ?y))
=>
	(retract ?a)
	(retract ?b)
	(retract ?c)
	(assert (min 99999))
	(assert (result (bind $?z (insert$ $?z (+ 1 (length$ $?z)) ?y))))
)

(defrule buscarMinimo
	(declare (salience 70))
	(hecho1 ?x)
	?a<-(min ?y)
	(test (< ?x ?y))
=>
	(retract ?a)
	(assert (min ?x))
)

(defrule r3
	(declare (salience 100))
	?a<-(hechos $?x)
=>
	(foreach ?act $?x
		(assert (hecho1 ?act))
	)
	(retract ?a)
)

(deffacts init
	(hechos 2 6 3 9)
	(min 99999)
	(result (create$ ))
)
