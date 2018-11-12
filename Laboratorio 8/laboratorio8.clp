(deftemplate jarra
    (slot capacidad (type INTEGER))
    (slot cantidad (type INTEGER))
)

(deftemplate estado
    (slot jarra1)
    (slot jarra2)
    (slot estado_anterior)
    (slot movimiento)
)
