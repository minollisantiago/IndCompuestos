Agregacion <- function(x, metodo.agregacion = "saw", 
pesos = Critic(x), sentido.criterios = NULL, v = 0.5)
{	


	#Sentido de los criterios (defecto)	
	if(is.null(sentido.criterios)) {

		sentido.criterios <- rep("max",ncol(x))

}	#Finaliza el condicional #Sentido de los criterios (defecto)
	names(sentido.criterios) <- colnames(x)	


	#Normalizacion
	Matriz_normalizada <- x	


	#Determinacion pesos
	Vector_pesos <- pesos
	

	#Suma ponderada
	if (metodo.agregacion == "saw") {
	
	 	#Indicador compuesto
		CI <- round(Matriz_normalizada%*%Vector_pesos,5)

		#colnames de la matriz con el indicador compuesto calculado
		colnames(CI) <- "CI"
		#Rownames de la matriz con el indicador compuesto calculado
		rownames(CI) <- rownames(Matriz_normalizada)

} 	#Finaliza el condicional #Suma ponderada


	#Producto ponderado
	if (metodo.agregacion == "wp") {

		#Indicador compuesto 
		CI <- matrix(round(apply(t(t(Matriz_normalizada)^
		Vector_pesos),1,prod),5),nrow(Matriz_normalizada),1,byrow=TRUE)

		#colnames de la matriz con el indicador compuesto calculado
		colnames(CI) <- "CI"	
		#Rownames de la matriz con el indicador compuesto calculado
		rownames(CI) <- rownames(Matriz_normalizada)	

} 	#Finaliza el condicional ##Producto ponderado


	#Topsis (euclidea)
	if (metodo.agregacion == "topsis.eu") {

		#Matriz variables normalizadas*pesos
		y <- t(t(Matriz_normalizada)*Vector_pesos)		
		
		#Alternativas ideal y anti-ideal
		Ideal <- ifelse(sentido.criterios == "max", 
		apply(y,2,max), apply(y,2,min))
		Anti_ideal <- ifelse(sentido.criterios == "max", 
		apply(y,2,min), apply(y,2,max))


		#Distancias (euclideas)
		#Ideal
		Distancia_ideal <- sqrt(rowSums(t(t(y)-Ideal)^2))

		#Anti-ideal
		Distancia_anti_ideal <- sqrt(rowSums(t(t(y)-Anti_ideal)^2))

		#Indicador compuesto
		CI <- matrix(Distancia_anti_ideal/
		(Distancia_anti_ideal+Distancia_ideal),
		nrow(Matriz_normalizada),1,byrow = TRUE)

		#colnames de la matriz con el indicador compuesto calculado
		colnames(CI) <- "CI"	
		#Rownames de la matriz con el indicador compuesto calculado
		rownames(CI) <- rownames(Matriz_normalizada)

} 	#Finaliza el condicional ##Topsis (euclidea)


	#Topsis (ciudad)
	if (metodo.agregacion == "topsis.cd") {

		#Matriz variables normalizadas*pesos
		y <- t(t(Matriz_normalizada)*Vector_pesos)

		#Alternativas ideal y anti-ideal
		Ideal <- ifelse(sentido.criterios == "max", 
		apply(y,2,max), apply(y,2,min))
		Anti_ideal <- ifelse(sentido.criterios == "max", 
		apply(y,2,min), apply(y,2,max))
		
		#Distancias (ciudad)
		#Ideal
		Distancia_ideal <- rowSums(abs(t(t(y)-Ideal)))
		#Anti-ideal
		Distancia_anti_ideal <- rowSums(abs(t(t(y)-Anti_ideal)))

		#Indicador compuesto
		CI <- matrix(Distancia_anti_ideal/
		(Distancia_anti_ideal+Distancia_ideal),
		nrow(Matriz_normalizada),1,byrow = TRUE)

		#colnames de la matriz con el indicador compuesto calculado
		colnames(CI) <- "CI"	
		#Rownames de la matriz con el indicador compuesto calculado
		rownames(CI) <- rownames(Matriz_normalizada)

} 	#Finaliza el condicional ##Topsis (ciudad)


	#Topsis (Tchebycheff)
	if (metodo.agregacion == "topsis.tb") {
		
		#Matriz variables normalizadas*pesos
		y <- t(t(Matriz_normalizada)*Vector_pesos)

		#Alternativas ideal y anti-ideal
		Ideal <- ifelse(sentido.criterios == "max", 
		apply(y,2,max), apply(y,2,min))
		Anti_ideal <- ifelse(sentido.criterios == "max", 
		apply(y,2,min), apply(y,2,max))
		
		#Distancias (Tchebycheff)
		#Ideal
		Distancia_ideal <- apply(abs(t(t(y)-Ideal)), 1, max)
		#Anti-ideal
		Distancia_anti_ideal <- apply(abs(t(t(y)-Anti_ideal)), 1, max)

		#Indicador compuesto
		CI <- matrix(Distancia_anti_ideal/
		(Distancia_anti_ideal+Distancia_ideal),
		nrow(Matriz_normalizada),1,byrow = TRUE)

		#colnames de la matriz con el indicador compuesto calculado
		colnames(CI) <- "CI"	
		#Rownames de la matriz con el indicador compuesto calculado
		rownames(CI) <- rownames(Matriz_normalizada)

} 	#Finaliza el condicional ##Topsis (Tchebycheff)


	#Vikor
	if (metodo.agregacion == "Vikor") {
		
		#Matriz variables normalizadas*pesos
		y <- t(t(Matriz_normalizada)*Vector_pesos)

		#Medidas S (max group utility) y R (min individual regret)
		S_val <- rowSums(y)
		R_val <- apply(y, 1, max)
		
		#Indicador compuesto (Q)
		Q_val <- v * (S_val - min(S_val)) / (max(S_val) - min(S_val)) + 
		(1 - v) * (R_val - min(R_val)) / (max(R_val) - min(R_val))

		CI <- matrix(Q_val, nrow(Matriz_normalizada), 1, byrow = TRUE)

		#colnames de la matriz con el indicador compuesto calculado
		colnames(CI) <- "CI"	
		#Rownames de la matriz con el indicador compuesto calculado
		rownames(CI) <- rownames(Matriz_normalizada)

} 	#Finaliza el condicional ##Vikor
	

	#Ordenamiento de las alternativas 
	if ( metodo.agregacion != "Vikor" ) {

		Indices_ordenados <- sort.list(CI, decreasing = TRUE)

}	else {

		Indices_ordenados <- sort.list(CI, decreasing = FALSE)

}	#Finaliza el condicional #Ordenamiento de las alternativas 
	

	#Output 
	return(list(CI = CI, IndicesOrdenados = Indices_ordenados))


} 
