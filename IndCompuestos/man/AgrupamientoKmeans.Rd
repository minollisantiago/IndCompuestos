\name{AgrupamientoKmeans}
\alias{AgrupamientoKmeans}
\title{Agrupamiento Alternativas}
\description{Agrupa las alternativas de la matriz de subindicadores usando el algoritmo K-means.}
\usage{AgrupamientoKmeans(x, numero.grupos = 2, numero.pruebas = 100, 
iter.max = 10, num.seeds = 10)}
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{numero.grupos}{
%%     ~~Describe \code{numero.grupos} here~~
}
  \item{numero.pruebas}{
%%     ~~Describe \code{numero.pruebas} here~~
}
  \item{iter.max}{
%%     ~~Describe \code{iter.max} here~~
}
  \item{num.seeds}{
%%     ~~Describe \code{num.seeds} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, numero.grupos = 2, numero.pruebas = 100, iter.max = 10, 
    num.seeds = 10) 
{
    if (is.matrix(x) == FALSE) {
        x <- as.matrix(x)
    }
    Matriz_grupos <- matrix(numeric(0), nrow(x), numero.pruebas, 
        byrow = TRUE, dimnames = list(rownames(x), paste("CLUSTER", 
            seq(1:numero.pruebas), sep = "")))
    for (i in 1:numero.pruebas) {
        Matriz_grupos[, i] <- kmeans(x, centers = numero.grupos, 
            iter.max = iter.max, nstart = num.seeds)$cluster
    }
    return(Matriz_grupos)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
