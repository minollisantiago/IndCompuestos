\name{Muestra.matriz}
\alias{Muestra.matriz}
\title{Generacion de Submatrices}
\description{Genera submatrices aleatorias utilizando la funcion sample, a partir de una matriz de subindicadores.}
\usage{Muestra.matriz(x, n = NULL, reemplazo = FALSE)}
\arguments{
  \item{x}{Matriz (o data frame) de subindicadores.}
  \item{n}{Numeric. Indica el tamaño deseado para la muestra. Limitado por el tamaño de x.}
  \item{reemplazo}{Logical. Indica si la muestra se selecciona con o sin reemplazo. Si reemplazo = TRUE, entonces selecciona la muestra con reemplazo.}
}
\details{
El argumento n esta definido por defecto como la mitad de la cantidad de alternativas de la matriz de datos x. Si n es mayor que la cantidad de alternativas de la matriz de datos, la funcion arrojara la matriz de datos.

El argumento reemplazo esta definido por defecto como reemplazo = FALSE (la muestra se selecciona sin reemplazo).}
\value{La funcion devuelve una submatriz a partir de la matriz de subindicadores x con n numero de alternativas (filas).}
\references{}
\author{Minolli Santiago.}
\note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sample}}, \code{\link{rownames}}, \code{\link{return}}
}
\examples{

##EJEMPLO 1:
#Carga la base de datos
data(BasePaises) 

#Obtiene una submatriz con los argumentos por defecto
Muestra.matriz(BasePaises) 

#Lo anterior arroja exactamente el mismo resultado que:
Muestra.matriz(BasePaises, n = NULL, reemplazo = FALSE)


##EJEMPLO 2:
#Carga la base de datos
data(BasePaises) 

#Obtiene una submatriz con reemplazo de 30 observaciones (alternativas): 
Muestra.matriz(BasePaises, n = 30, reemplazo = TRUE)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
