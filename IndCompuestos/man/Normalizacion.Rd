\name{Normalizacion}
\alias{Normalizacion}
\title{Metodos de Normalizacion}
\description{Normaliza los datos contenidos en la matriz de subindicadores utilizando los distitos metodos de normalizacion soportados por la funcion.}
\usage{Normalizacion(x, metodo = "fracc.max", 
sentido.criterios = NULL, all.max = TRUE, reescalar = TRUE, 
coef.reescalado = 1)}
\arguments{
  \item{x}{Matriz (o data frame) de subindicadores.}
  \item{metodo}{Character string. Metodo de normalizacion a utilizar:"fracc.max" (defecto), "fracc.sum", "fracc.rango", "fracc.vector", "lineal" y "fracc.meda".}
  \item{sentido.criterios}{Character vector. Vector de caracteres que indican el sentido de los criterios.}
  \item{all.max}{Logical. TRUE o FALSE, indica si debe tomar la inversa de los criterios que sean de minimo.}
  \item{reescalar}{Logical. TRUE o FALSE, indica si debe reescalar los criterios con valores negativos.}
  \item{coef.reescalado}{Numeric. Coeficiente de reescalado, en caso que se reescale los criterios con valores negativos.}

}
\details{La matriz de subindicadores x, debe tener las alternativas en las filas y los criterios en las columnas.

Los metodos de normalizacion (arg. metodo) soportados son los siguientes:"fracc.max" = Fraccion del maximo, "fracc.sum" = Fraccion de la suma, "fracc.rango" = Fraccion del rango, "fracc.vector" = Fraccion del vector, "lineal" = Normalizacion lineal (Vikor) y "fracc.meda" = Estandarizacion mediana y meda. Por defecto la funcion utiliza el metodo de normalizacion de la fraccion del maximo ("fracc.max").

El argumento sentido.criterios es un vector de caracteres que indica el sentido (max o min) de cada uno de los subindicadores. Por defecto la funcion le asigna a todos los subindicadores el sentido de max.

Si se le indica a la funcion que reescale los criterios con valores negativos (reescalar = TRUE), la funcion reescala el criterio restando el minimo aumentado por el coeficiente indicado (coef.reescalado).
}

\value{
La funcion devuelve una matriz, la misma es la matriz de subindicadores normalizada utilizando el metodo de normalizacion designado al llamar la funcion.

Si la funcion es llamada solo indicando la matriz de subindicadores ej: Normalizacion(x), la misma arrojara la matriz normalizada obtenida mediante la utilizacion del metodo de normalizacion establecido por defecto, tomando la inversa de todos los criterios de minimo y reescalando los criterios con valores negativos.}
\references{}
\author{Minolli Santiago.}
\note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Agregacion}}, \code{\link{rep}}, \code{\link{colnames}}, \code{\link{rownames}}, 
\code{\link{apply}}, \code{\link{sqrt}}, \code{\link{colSums}}, \code{\link{return}}, 
\code{\link{max}}, \code{\link{min}}, \code{\link{median}} 
}
\examples{

##EJEMPLO 1:
#Carga la base de datos
data(BasePaises) 

#Normaliza la base con los metodos por defecto
Normalizacion(BasePaises) 

#Lo anterior arroja exactamente el mismo resultado que:
Normalizacion(BasePaises, metodo = "fracc.max", 
sentido.criterios = NULL, all.max = TRUE, reescalar = TRUE, 
coef.reescalado = 1) 


##EJEMPLO 2:
#Carga la base de datos
data(BasePaises) 

#Normaliza la base por fraccion del rango, reescalando y tomando inversa de los criterios de min
Normalizacion(BasePaises, metodo = "fracc.rango", 
sentido.criterios = c(rep("max",6),rep("min",3),rep("max",4))) 

#Nota: El vector de sentido de los criterios puede definirse tambien como:
#sentido.criterios = c("max","max","max","max","max","max","min","min","min","max","max","max","max")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
