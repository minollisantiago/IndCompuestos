\name{DropboxUrl}
\alias{DropboxUrl}
\title{Wrapper para obtener urls de Dropbox}
\description{Esta funcion sirve para obtener el url valido para descargas directamente a R de archivos que se encuentren en dropbox.}
\usage{DropboxUrl(Url, outAsVector = TRUE)}
\arguments{
  \item{Url}{Vector o lista con una o multiples urls de dropbox.}
  \item{outAsVector}{Logical. Indica si la funcion debe arrojar el output en un vector o en una lista, en el caso de pasarle multiples urls.}
}
\details{Esta funcion sirve para obtener el url valido para descargas directamente a R de archivos desde dropbox. El/los url/s que la funcion toma como primer argumento es el que genera dropbox cuando 
se selecciona la opcion "get url" para cualquier archivo desde la plataforma de dropbox en la web.}
\value{Vector o lista con el/los url/s validos para descargas a R desde dropbox.}
\references{}
\author{Minolli Santiago.}
\note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Agregacion}}, \code{\link{apply}}, \code{\link{sd}},
\code{\link{cor}}, \code{\link{colSums}}, \code{\link{sum}}, \code{\link{return}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
