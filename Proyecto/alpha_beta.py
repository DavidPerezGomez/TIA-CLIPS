
g = {
    # nivel 1
    1: [2, 3, 4],
    # nivel 2
    2: [5, 6],
    3: [7, 8],
    4: [9, 10],
    # nivel 3
    5: [11, 12],
    6: [13, 14, 15],
    7: [16, 17, 18],
    8: [19, 20],
    9: [21, 22],
    10: [23, 24],
    #nivel 4
    11: 4,
    12: 5,
    13: 3,
    14: 6,
    15: 1,
    16: 4,
    17: 2,
    18: 1,
    19: 1,
    20: 2,
    21: 5,
    22: 4,
    23: 9,
    24: 8,
}


def terminal(s):
    """Devuelve True si el nodo es terminal"""
    try:
        int(s)
        return True
    except: # error si es una lista
        return False


def max_value(s, id_padre, alpha=None, beta=None):
    if terminal(s):
        return (s, id_padre)
    v = -9999
    for c in s:
        v_aux, id_padre_aux = min_value(g[c], c, alpha, beta)
        if v_aux > v:
            v = v_aux
            id_padre = id_padre_aux
        if beta:  # si beta es None no entra
            if v_aux >= beta:
                return v
        if alpha:
            if v_aux > alpha:
                alpha = v_aux
    return (v, id_padre)


def min_value(s, id_padre, alpha=None, beta=None):
    if terminal(s):
        return (s, id_padre)
    v = 9999
    for c in s:
        v_aux, id_padre_aux = max_value(g[c], c, alpha, beta)
        if v_aux < v:
            v = v_aux
            id_padre = id_padre_aux
        if alpha:  # si alpha es None no entra
            if v_aux <= alpha:
                return v
        if beta:
            if v_aux < beta:
                beta = v_aux
    return (v, id_padre)


def encontrar_camino_vuelta(id_padre):
    # mientras que no sea la raiz
    inicio = False
    camino = [id_padre]
    while not inicio:
        for key, value in g.items():
            try:
                if id_padre in value:
                    id_padre = key
                    camino.append(id_padre)
                    break
            except:
                pass
        if id_padre == 1:
            inicio = True
    return camino

value, id_padre = max_value(g[1], 1)

print("el valor es:", value)
print("el camino es:", encontrar_camino_vuelta(id_padre))
