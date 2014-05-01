\name{BasePaises}
\alias{BasePaises}
\docType{data}
\title{Base Merito al Credito Editada}
\description{Esta es la base de merito al credito depurada, lista para usar como argumento de las funciones del paquete.
}
\usage{data(BasePaises)}
\format{
  A data frame with 98 observations on the following 13 variables.
  \describe{
    \item{\code{PBIpc}}{a numeric vector}
    \item{\code{cPBIr}}{a numeric vector}
    \item{\code{VTCR}}{a numeric vector}
    \item{\code{DPBI}}{a numeric vector}
    \item{\code{CCPBI}}{a numeric vector}
    \item{\code{RaImp}}{a numeric vector}
    \item{\code{DTEEx}}{a numeric vector}
    \item{\code{DTEPBI}}{a numeric vector}
    \item{\code{SDTEx}}{a numeric vector}
    \item{\code{BPGPBI}}{a numeric vector}
    \item{\code{EP2007}}{a numeric vector}
    \item{\code{CR2007}}{a numeric vector}
    \item{\code{EG2007}}{a numeric vector}
  }
}
\details{
Se trata de la base de merito al credito original (BasePaisesOriginal), depurada mediante la eliminacion de las variables no numericas (columnas 1 (nombre de los paises) y 2 (codigo de los paises). Es la base utlizada en los ejemplos del paquete y esta lista para ser utilizada como argumento de cualquiera de sus funciones.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
##EJEMPLO 1:
#Carga la base de datos
data(BasePaises) 

#Operaciones sobre la base:
Normalizacion(BasePaises)
Agregacion(Normalizacion(BasePaises))
Critic(Normalizacion(BasePaises))
ShannonSpearman(BasePaises)

}
\keyword{datasets}
