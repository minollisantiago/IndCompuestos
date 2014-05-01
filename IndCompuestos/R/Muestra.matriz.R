Muestra.matriz <-
function(x, n = NULL, reemplazo = FALSE)
{

#Tamaño por defecto de la muestra
if(is.null(n)) { n <- round(nrow(x)*0.5,0) }

#Tamaño maximo de la muestra
if(n > nrow(x)) { n <- nrow(x) }

#Muestra aleatoria (ver ?sample)
muestra <- sample(rownames(x), size= n, replace= reemplazo)

#Output
return(x[muestra,])

}
