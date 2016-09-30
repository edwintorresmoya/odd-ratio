# odd-ratio
Calculate the odd ratio and create a plot

OR.fx =function(exposed, outcome){ #Funcion que retorna el Odss Rate
    primera_t = table(exposed, outcome)
    #barplot(primera_t, beside = T, legend = T)
    primera_t = as.data.frame.matrix(primera_t)
    primera_t = primera_t[c(2,1),c(2,1)]
    primera_t = rbind(primera_t,c(sum(primera_t$`1`, na.rm = T),sum(primera_t$`0`, na.rm = T)))
    rownames(primera_t)[3] = "Suma"
    OR_a = (primera_t[1,1] * primera_t[2,2])/(primera_t[2,1]*primera_t[1,2])
    OR_b = (primera_t[2,1]*primera_t[1,2])/(primera_t[1,1] * primera_t[2,2])
    Risk_a = (primera_t[1,1] / primera_t[3,1])/(primera_t[1,2] / primera_t[3,2])
    Risk_b = (primera_t[1,2] / primera_t[3,2])/(primera_t[1,1] / primera_t[3,1])
    return(list(tabla = primera_t, OR1 = OR_a, OR2 = OR_b, Rsk1 = Risk_a, Rsk2 = Risk_b))
    
    "razon de exposed y no exposed es OR1 veces mayor Outcome 1, que outcome 2"
    "la razón entre fila 1 versus fila 2 es OR1 veces mayor en Columna 1 en comparación de Columna 2"
}

