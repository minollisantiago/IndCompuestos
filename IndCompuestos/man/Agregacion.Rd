\name{Agregacion}
\alias{Agregacion}
\title{Metodos de Agregacion}
\description{Construye un indicador compuesto a partir de una matriz de subindicadores y alternativas, utilizando los distintos metodos de normalizacion, pesos y agregacion soportados por la funcion.}
\usage{Agregacion(x, metodo.agregacion = "saw", 
pesos = Critic(x), sentido.criterios = NULL)
}
\arguments{
  \item{x}{Matriz (o data frame) de subindicadores (Normalizada).}
  \item{metodo.agregacion}{Character string. Metodo de agregacion a utilizar: "saw" (defecto), "wp", "topsis.eu", "topsis.cd" y "topsis.tb". }
  \item{pesos}{Numeric vector. Vector con los pesos de los subindicadores: Usa el metodo "critic" (defecto).}
  \item{sentido.criterios}{Character vector. Vector de caracteres que indican el sentido de los criterios (para TOPSIS).}
  \item{v}{Numeric vector. Vector de un elemento que representa el peso de la estrategia de la maxima utilidad grupal (para Vikor).}
}
\details{
La matriz de subindicadores x, debe tener las alternativas en las filas y los criterios en las columnas y debe estar normalizada (ver \code{\link{Normalizacion}}).

Los metodos de agregacion (arg. metodo.agregacion) soportados son los siguientes:"saw" = Suma Ponderada, "wp" = Producto ponderado, "topsis.eu" = Topsis con distancias euclideas, "topsis.cd" = Topsis con distancias ciudad, "topsis.tb" = Topsis con distancias de Tchebycheff y "Vikor" = Vikor. Por defecto la funcion utiliza el metodo de agregacion de la suma ponderada ("saw").

Para la determinacion de los pesos (arg. pesos), la funcion utiliza por defecto el metodo CRITIC (ver \code{\link{Critic}}), igualmente pueden definirse los pesos como se desee (subjetivamente o mediante otro metodo). El paquete solo incluye por ahora el metodo Critic.

El argumento sentido.criterios es un vector de caracteres que indica el sentido (max o min) de cada uno de los subindicadores. Por defecto la funcion le asigna a todos los subindicadores el sentido de max. Se utiliza solamente para identificar las alternativas ideal y anti-ideal para TOPSIS.}

\value{
La funcion devuelve dos objetos:

\item{CI}{Vector numerico con el indicador compuesto calculado para todas las alternativas de la matriz de subindicadores (filas).}

\item{IndicesOrdenados}{Indices de la matriz de subindicadores x, ordenados de forma decreciente conforme al indicador compuesto. En el caso de haber agregado los datos usando Vikor, los indices estaran ordenados de forma creciente.}

Si la funcion es llamada solo indicando la matriz de subindicadores ej: Agregacion(x), la misma arrojara el indicador compuesto calculado utilizando los metodos establecidos por defecto.}
\references{}
\author{Minolli Santiago.}
\note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Normalizacion}}, \code{\link{Critic}}, \code{\link{rep}}, \code{\link{apply}},
\code{\link{rownames}}, \code{\link{colnames}}, \code{\link{nrow}}, \code{\link{ncol}},
\code{\link{sqrt}}, \code{\link{abs}}, \code{\link{sort.list}}, \code{\link{list}}, \code{\link{return}}
}
\examples{

##EJEMPLO 1:
#Carga la base de datos
data(BasePaises) 

#Normaliza la base de datos (ver ?Normalizacion para mas detalles)
BaseNormalizada <- Normalizacion(BasePaises)

#Arroja el IC con los metodos por defecto
Agregacion(BaseNormalizada) 

#Lo anterior arroja exactamente el mismo resultado que:
Agregacion(BaseNormalizada, metodo.agregacion = "saw", 
pesos = Critic(BaseNormalizada), sentido.criterios = NULL) 


##EJEMPLO 2:
#Carga la base de datos
data(BasePaises) 

#Normaliza la base de datos (ver ?Normalizacion para mas detalles)
BaseNormalizada <- Normalizacion(BasePaises)

#Arroja el IC con los siguientes metodos: 
#Determinacion pesos: Mismo peso para todos los subindicadores
#Agregacion: Topsis con distancias euclideas
#Nota: El vector de sentido de los criterios puede definirse tambien como:
#sentido.criterios = c("max","max","max","max","max","max","min","min","min","max","max","max","max")

Agregacion(BaseNormalizada, metodo.agregacion = "topsis.eu",  
pesos = rep(1/ncol(BasePaises), ncol(BasePaises)), 
sentido.criterios = c(rep("max",6),rep("min",3),rep("max",4))) 


##EJEMPLO 3:
#Carga la base de datos
data(BasePaises) 

#Normaliza la base de datos (ver ?Normalizacion para mas detalles)
BaseNormalizada <- Normalizacion(BasePaises)

#Arroja el IC con los metodos por defecto y asigna el output a un objeto (indicador)
Indicador <- Agregacion(BaseNormalizada) 

#Arroja solo el indicador compuesto (usando el operador $)
Indicador$CI

#Arroja solo el vector de indices ordenados (usando el operador $)
Indicador$IndicesOrdenados

#Esto tambien puede hacerse sin asignar el output a un objeto (es mas engorroso)
Agregacion(BaseNormalizada)$CI 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
