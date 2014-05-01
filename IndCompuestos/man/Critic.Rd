\name{Critic}
\alias{Critic}
\title{Metodos para la Determinacion de Ponderaciones}
\description{Construye un vector de ponderaciones para todos los subindicadores (criterios) de una matriz de subindicadores, utilizando distintos metodos para el calculo de los mismos (por ahora solo incluye al metodo CRITIC).}
\usage{Critic(x)}
\arguments{
  \item{x}{Matriz (o data frame) de subindicadores (normalizada).}
}
\details{La matriz de subindicadores x, debe tener las alternativas en las filas y los criterios en las columnas y debe estar normalizada (ver \code{\link{Normalizacion}}). 

Por ahora solo soporta el metodo CRITIC (establecido por defecto) para el calculo de las ponderaciones.}
\value{Devuelve un vector numerico con atributo names, que contiene las ponderaciones para todos los subindicadores de la matriz de datos (subindicadores).}
\references{}
\author{Minolli Santiago.}
\note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Agregacion}}, \code{\link{apply}}, \code{\link{sd}},
\code{\link{cor}}, \code{\link{colSums}}, \code{\link{sum}}, \code{\link{return}}
}
\examples{

##EJEMPLO 1:
#Carga la base de datos
data(BasePaises) 

#Obtiene los pesos de los subindicadores mediante CRITIC
Critic(BasePaises) 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
