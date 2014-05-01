ShannonSpearman <- function(x, CI = Agregacion(Normalizacion(x))$CI, 
ponderaciones = Critic(Normalizacion(x)), sentido.criterios = NULL, 
coef.reescalado = 1.01, version.modificada = TRUE)
{

	#Sentido de los criterios (defecto)	
	if(is.null(sentido.criterios)) {

		sentido.criterios <- rep("max",ncol(x))

}	#Finaliza el condicional #Sentido de los criterios (defecto)
	names(sentido.criterios) <- colnames(x)


	#Reescalado	(Transformacion criterios con valores negativos)
	#Para calculo de la entropia
	for (Criterio in 1:ncol(x))
{
		#Transformacion 
		if(TRUE %in% (x[,Criterio]<0)) {

			x[,Criterio] <- x[,Criterio] - 
			(min(x[,Criterio]) * coef.reescalado) 

} 		#Finaliza el condicional #Transformacion

}	#Finaliza el loop	#Reescalado	(Transformacion criterios con valores negativos)	


	#Calculo de la medida - Version Modificada	
	if (version.modificada == TRUE) {

		#Primer termino (informacion en la matriz de subindicadores)
		#Normalizacion (Fraccion de la suma)
		Matriz_normalizada <- t(t(x)/colSums(x))

		#Inversa criterios max (porque rank ordena siempre ascendentemente)
		y <- x
		for (Criterio in 1:ncol(x))
{
			#Inversa 
			if (sentido.criterios[Criterio]=="max") {

				y[,Criterio] <- 1/x[,Criterio] 

}			#Finaliza el condicional #Inversa	
		
}		#Finaliza el loop #Inversa criterios max 

		#Matriz rankings
		Alt_rankeadas <- apply(y, 2, rank)

		#Informacion contenida en la matriz de subindicadores
		Inf_subindicadores <- sum(((colSums(Matriz_normalizada*
		log(Matriz_normalizada))/log(nrow(x)))+1)*ponderaciones*
		apply(Alt_rankeadas,2,cor, y = rank(1/CI), method = "pearson"))	

		#segundo termino (informacion en el indicador compuesto)
		#Normalizacion (Fraccion de la suma)
		#Incluye resolucion del problema de CI = 0
		CI_normalizado <- ifelse(CI/sum(CI) != 0, CI/sum(CI), 0.0001)

		#Informacion contenida en el indicador compuesto
		Inf_IC <- (sum(CI_normalizado*log(CI_normalizado))/log(nrow(x)))+1
		
		#Medida Shannon - Spearman
		SSM <- abs(Inf_subindicadores - Inf_IC)

}	#Finaliza el condicional #Calculo de la medida - Version Modificada


	#Calculo de la medida - Version Original	
	if (version.modificada == FALSE) {

		#Ordenamiento de referencia
		Ord_ref <- sort.list((1:nrow(x)), decreasing = TRUE)

		#Primer termino (informacion en la matriz de subindicadores)
		#Normalizacion (Fraccion de la suma)
		Matriz_normalizada <- t(t(x)/colSums(x))

		#Inversa criterios max (porque rank ordena siempre ascendentemente)
		y <- x
		for (Criterio in 1:ncol(x))
{
			#Inversa 
			if (sentido.criterios[Criterio]=="max") {

				y[,Criterio] <- 1/x[,Criterio] 

}			#Finaliza el condicional #Inversa	
		
}		#Finaliza el loop #Inversa criterios max 

		#Matriz rankings
		Alt_rankeadas <- apply(y, 2, rank)

		#Informacion contenida en la matriz de subindicadores
		Inf_subindicadores <- sum(((colSums(Matriz_normalizada*
		log(Matriz_normalizada))/log(nrow(x)))+1)*ponderaciones*
		apply(Alt_rankeadas,2,cor, y = rank(1/Ord_ref), method = "pearson"))	

		#segundo termino (informacion en el indicador compuesto)
		#Normalizacion (Fraccion de la suma)
		#Incluye resolucion del problema de CI = 0
		CI_normalizado <- ifelse(CI/sum(CI) != 0, CI/sum(CI), 0.0001)

		#Informacion contenida en el indicador compuesto
		Inf_IC <- ((sum(CI_normalizado*log(CI_normalizado))/log(nrow(x)))+1)*
		cor(rank(1/CI), y = rank(1/Ord_ref), method = "pearson")

		#Medida Shannon - Spearman
		SSM <- abs(Inf_subindicadores - Inf_IC)

}	#Finaliza el condicional #Calculo de la medida - Version Original

	
	#Output final de la funcion 
	return(SSM)

} #Finaliza la funcion ##MEDIDA SHANNON - SPEARMAN
 
