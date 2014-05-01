Critic <-
function(x)
{

##Intensidad del contraste por criterio (desviacion estandar)
Vector_ds <- apply(x,2,sd)

#Medida de conflicto entre criterios
Matriz_correlaciones <- cor(x)
Medida_conflicto <- colSums(1-Matriz_correlaciones)

#Pesos (medida conflicto*intensidad del contraste normalizado)
Vector_pesos <- Medida_conflicto*Vector_ds/sum(Medida_conflicto*Vector_ds)

#Output
return(Vector_pesos)

}
