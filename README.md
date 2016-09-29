# odd-ratio
Calculate the odd ratio and create a plot

OR.fx = function(exposed, outcome){ #Funcion que retorna el Odss Rate
        primera_t = table(exposed, outcome)
        barplot(primera_t, beside = T, legend = T)
    primera_t = as.data.frame.matrix(primera_t)
    primera_t = rbind(primera_t,c(sum(primera_t$`0`),sum(primera_t$`1`)))
    rownames(primera_t)[3] = "Suma"
    OR_a = (primera_t[1,1] * primera_t[2,2])/(primera_t[2,1]*primera_t[1,2])
    OR_b = (primera_t[2,1]*primera_t[1,2])/(primera_t[1,1] * primera_t[2,2])
    return(list(tabla = primera_t, OR1 = OR_a, OR2 = OR_b))
}

