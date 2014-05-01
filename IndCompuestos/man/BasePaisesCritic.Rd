\name{BasePaisesCritic}
\alias{BasePaisesCritic}
\docType{data}
\title{Base Trabajo julio 2013 - Calculo de pesos}
\description{
Esta es la base de paises latinoamericanos usada en el trabajo de perdida de informacion de julio 2013, para el calculo de las ponderaciones.
}
\usage{data(BasePaisesCritic)}
\format{
  A data frame with 20 observations on the following 11 variables.
  \describe{
    \item{\code{PBI}}{a numeric vector}
    \item{\code{TAA}}{a numeric vector}
    \item{\code{TBM}}{a numeric vector}
    \item{\code{EVN}}{a numeric vector}
    \item{\code{TMI}}{a numeric vector}
    \item{\code{GS}}{a numeric vector}
    \item{\code{ECO2}}{a numeric vector}
    \item{\code{LTC}}{a numeric vector}
    \item{\code{UI}}{a numeric vector}
    \item{\code{EG}}{a numeric vector}
    \item{\code{CR}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
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
data(BasePaisesCritic)

#Operaciones sobre la base:
Normalizacion(BasePaisesCritic)
Agregacion(Normalizacion(BasePaisesCritic))
Critic(Normalizacion(BasePaisesCritic))
ShannonSpearman(BasePaisesCritic)
}
\keyword{datasets}
