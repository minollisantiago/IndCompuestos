\name{ShannonSpearman}
\alias{ShannonSpearman}
\title{Medida de Shannon - Spearman}
\description{Calcula la medida de perdida de informacion al construir indicadores compuestos de Shannon - Spearman.}
\usage{
ShannonSpearman(x, CI = Agregacion(Normalizacion(x))$CI, 
ponderaciones = Critic(Normalizacion(x)), sentido.criterios = NULL, 
coef.reescalado = 1.01, version.modificada = TRUE)
}
\arguments{
  \item{x}{Matriz (o data frame) de subindicadores.}
  \item{CI}{Numeric vector. Vector numerico que contiene los valores del indicador compuesto para cada i-esima alternativa.}
  \item{ponderaciones}{Numeric vector. Vector numerico que contiene los pesos de cada j-esimo criterio (subindicador).}
  \item{sentido.criterios}{Character vector. Vector de caracteres que indican el sentido de los criterios.}
  \item{coef.reescalado}{Numeric. Coeficiente de reescalado, para reescalar los criterios con valores negativos.}
  \item{version.modificada}{Logical. Indica el metodo de calculo de la medida a utilizar. Si version.modificada = TRUE, calcula la medida con los ajustes realizados en Funes, Racagni y Guevel (2013).}
}
\details{La matriz de subindicadores x, debe tener las alternativas en las filas y los criterios en las columnas.

El argumento CI (indicador compuesto) puede obtenerse utilizando la funcion correspondiente (ver \code{\link{Agregacion}}). Por defecto, la funcion calcula el indicador compuesto utilizando la funcion Agregacion con sus valores por defecto.

Las ponderaciones de los criterios (subindicadores) pueden obtenerse utilizando la funcion correspondiente (ver \code{\link{Critic}}) o pueden fijarse utilizando otros metodos o de forma subjetiva. Por defecto la funcion los obtiene utilizando CRITIC. 

El argumento sentido.criterios se usa para obtener la matriz con los indices ordenados por subindicador (la funcion toma la inversa de los criterios de maximo puesto que para obtener los indices ordenados se usa la funcion \code{\link{rank}}, que siempre ordena de forma ascendente).

El argumento coef.reescalado se utiliza para reescalar los criterios de la matriz de subindicadores que tengan valores negativos.

El argumento version.modificada indica si la medida debe ser calculada utilizando la version original (version.modificada = FALSE) o la version modificada (version.modificada = TRUE). La funcion calcula por defecto la version modificada.}
\value{
La funcion devuelve la medida de perdida de informacion de Shannon - Spearman.

Si la funcion es llamada solo indicando la matriz de subindicadores ej: ShannonSpearman(x), la misma arrojara la medida de Shannon - Sperman calculada utilizando los metodos establecidos por defecto.}
\references{}
\author{Minolli Santiago.}
\note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Agregacion}}, \code{\link{Normalizacion}}, \code{\link{Critic}}, \code{\link{rep}},
\code{\link{ncol}}, \code{\link{nrow}}, \code{\link{colnames}}, \code{\link{rownames}}, \code{\link{min}},
\code{\link{colSums}}, \code{\link{sum}}, \code{\link{rank}}, \code{\link{log}}, \code{\link{apply}}, \code{\link{cor}},
\code{\link{abs}}, \code{\link{sort.list}}, \code{\link{return}}
}
\examples{

##EJEMPLO:
#Carga la base de datos
data(BasePaises) 

#Obtiene la medida de Shannon - Spearman con los argumentos definidos por defecto
ShannonSpearman(BasePaises) 

#Lo anterior arroja exactamente el mismo resultado que:
ShannonSpearman(BasePaises, CI = Agregacion(Normalizacion(BasePaises))$CI, 
ponderaciones = Critic(Normalizacion(BasePaises)), sentido.criterios = NULL, 
coef.reescalado = 1.01, version.modificada = TRUE)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
