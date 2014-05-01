Normalizacion <- 
function(x, metodo = "fracc.max", 
sentido.criterios = NULL, all.max = TRUE, reescalar = TRUE, 
coef.reescalado = 1) 
{	


	#Sentido de los criterios (defecto)	
	if(is.null(sentido.criterios)) {

		sentido.criterios <- rep("max",ncol(x))

}	#Finaliza el condicional #Sentido de los criterios (defecto)
	names(sentido.criterios) <- colnames(x)	


	#Reescalado	(Transformacion criterios con valores negativos)
	if (reescalar) {

		for (Criterio in 1:ncol(x))
{
			#Transformacion 
			if(TRUE %in% (x[,Criterio]<0)) {

				x[,Criterio] <- x[,Criterio] - 
				(min(x[,Criterio]) * coef.reescalado) 

} 			#Finaliza el condicional #Transformacion

}		#Finaliza el loop

}	#Finaliza el condicional #Reescalado (Transformacion criterios con valores negativos)


	#Inversa criterios min 
	if (all.max) {

		for (Criterio in 1:ncol(x))
{
			#Inversa 
			if (sentido.criterios[Criterio]=="min") {

				x[,Criterio] <- 1/x[,Criterio] 

}			#Finaliza el condicional #Inversa	
		
}		#Finaliza el loop 

		#Ajusta sentido.criterios
		sentido.criterios <- rep("max",ncol(x))

}	#Finaliza el condicional #Inversa criterios min 


	#Normalizacion - Fraccion del maximo
	if(is.null(metodo) | metodo == "fracc.max") {

		#Proceso de normalizacion:
		#1)apply(x,2,max) me arroja el maximo por columna (criterio)
		#2)Traspongo la matriz de evaluacion para poder dividir por el max de cada criterio
		#Cuando se divide una matriz por un vector divide a todos los elementos de cada
		#columna por el elemento del vector con indice igual al numero de columna 
		#3)Finalmente traspongo de nuevo para que la matriz quede como la original 	

		#Matriz normalizada
		Matriz_normalizada <- t(t(x)/apply(x,2,max))

}	#Finaliza el condicional #Normalizacion - Fraccion del maximo


	#Normalizacion - Fraccion de la suma
	if(metodo == "fracc.sum") {

		#Proceso de normalizacion:
		#1)colSums(x) me arroja la suma de todas las observaciones por columna (criterio)
		#2)Traspongo la matriz de evaluacion para poder dividir por colSums(x)
		#Cuando se divide una matriz por un vector divide a todos los elementos de cada
		#columna por el elemento del vector con indice igual al numero de columna 
		#3)Finalmente traspongo de nuevo para que la matriz quede como la original 	

		#Matriz normalizada
		Matriz_normalizada <- t(t(x)/colSums(x))

}	#Finaliza el condicional #Normalizacion - Fraccion de la suma


	#Normalizacion - Fraccion del rango
	if(metodo == "fracc.rango") {

		#Proceso de normalizacion:
		#1)apply(x,2,max) me arroja el maximo por columna (criterio)
		#2)apply(x,2,min) me arroja el minimo por columna (criterio)
		#3)Traspongo la matriz de evaluacion para poder operar (resta y division)
		#Cuando se divide (por ej) una matriz por un vector divide a todos los elementos de cada
		#columna por el elemento del vector con indice igual al numero de columna 
		#4)Finalmente traspongo de nuevo para que la matriz quede como la original 	

		#Matriz normalizada
		Matriz_normalizada <- t((t(x)-apply(x,2,min))/
		(apply(x,2,max)-apply(x,2,min)))

}	#Finaliza el condicional #Normalizacion - Fraccion del rango


	#Normalizacion - Fraccion del vector
	if(metodo == "fracc.vector") {

		#Proceso de normalizacion:
		#1)apply(x,2,max) me arroja el maximo por columna (criterio)
		#2)apply(x,2,min) me arroja el minimo por columna (criterio)
		#3)Traspongo la matriz de evaluacion para poder operar (resta y division)
		#Cuando se divide (por ej) una matriz por un vector divide a todos los elementos de cada
		#columna por el elemento del vector con indice igual al numero de columna 
		#4)Finalmente traspongo de nuevo para que la matriz quede como la original 	

		#Matriz normalizada
		Matriz_normalizada <- t(t(x)/sqrt(colSums(x^2)))		

}	#Finaliza el condicional #Normalizacion - Fraccion del vector


	#Normalizacion - Lineal (Vikor)
	if(metodo == "lineal") {

		#Proceso de normalizacion:
		#1)apply(x,2,max) me arroja el maximo por columna (criterio)
		#2)apply(x,2,min) me arroja el minimo por columna (criterio)
		#3)Traspongo la matriz de evaluacion para poder operar (resta y division)
		#Cuando se divide (por ej) una matriz por un vector divide a todos los elementos de cada
		#columna por el elemento del vector con indice igual al numero de columna 
		#4)Finalmente traspongo de nuevo para que la matriz quede como la original 	

		#Mejor y peor valor por criterio
		BestV <- ifelse(sentido.criterios == "max", 
		apply(x, 2, max), apply(x, 2, min))

		WorstV <- ifelse(sentido.criterios == "max", 
		apply(x, 2, min), apply(x, 2, max))

		#Matriz normalizada
		Matriz_normalizada <- t((BestV - t(x))/(BestV - WorstV))	

}	#Finaliza el condicional #Normalizacion - Lineal (Vikor)


	#Normalizacion - Mediana & Desviacion Mediana
	if(metodo == "fracc.meda") {

		#Proceso de normalizacion:
		#1)apply(x,2,median) me arroja la mediana por columna (criterio)
		#2)El denominador es la desviacion mediana por columna (criterio)
		#3)Traspongo la matriz de evaluacion para poder operar (resta y division)
		#Cuando se divide (por ej) una matriz por un vector divide a todos los elementos de cada
		#columna por el elemento del vector con indice igual al numero de columna 
		#4)Finalmente traspongo de nuevo para que la matriz quede como la original 	

		#Matriz normalizada
		Matriz_normalizada <- t((t(x) - apply(x,2,median)) / 
		(apply(abs(t(t(x) - apply(x,2,median))), 2, median)))		

}	#Finaliza el condicional #Normalizacion - Mediana & Desviacion Mediana


	#Output 
	return(Matriz_normalizada)    
      

} 
