AgrupamientoKmeans <- 
function(x, numero.grupos = 2,
numero.pruebas = 100, iter.max = 10, num.seeds = 10)
{

	#Adecuacion matriz de subindicadores
	if (is.matrix(x) == FALSE) { x <- as.matrix(x) }


	#Matriz con los agrupamientos
	Matriz_grupos <- matrix(numeric(0), nrow(x), 
	numero.pruebas, byrow = TRUE, dimnames = list(rownames(x), 
	paste("CLUSTER", seq(1:numero.pruebas), sep = "")))
	
	for (i in 1:numero.pruebas)
{
		Matriz_grupos[, i] <- kmeans(x, 
		centers = numero.grupos, 
		iter.max = iter.max, nstart = num.seeds)$cluster  

}	#Finaliza el loop #Matriz con los agrupamientos
	
	#Output
	return(Matriz_grupos)

} 
