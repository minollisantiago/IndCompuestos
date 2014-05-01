\name{BasePaisesOriginal}
\alias{BasePaisesOriginal}
\docType{data}
\title{Base Merito al Credito Original}
\description{Esta es la base de datos original, sin modificaciones.}
\usage{data(BasePaisesOriginal)}
\format{
  A data frame with 100 observations on the following 15 variables.
  \describe{
    \item{\code{Pais}}{a factor with levels \code{Albania} \code{AntiIdeal} \code{Argentina} \code{Armenia} \code{Azerbaiyan} \code{Banglades} \code{Belice} \code{Benin} \code{Bielorrusia} \code{Bolivia} \code{Bosnia y Herzegovina} \code{Botsuana} \code{Brasil} \code{Bulgaria} \code{Camboya} \code{Camerun} \code{Chile} \code{China} \code{Colombia} \code{Congo} \code{Costa de Marfil} \code{Costa Rica} \code{Dominica} \code{Ecuador} \code{Egipto} \code{El Salvador} \code{Etiopia} \code{Filipinas} \code{Fiyi} \code{Gambia} \code{Georgia} \code{Ghana} \code{Granada} \code{Guatemala} \code{Guinea} \code{Guyana} \code{Haiti} \code{Honduras} \code{Ideal} \code{India} \code{Indonesia} \code{Islas Salomon} \code{Jamaica} \code{Jordania} \code{Kazajistan} \code{Kenia} \code{Kirguistan} \code{Laos} \code{Lesoto} \code{Letonia} \code{Libano} \code{Lituania} \code{Macedonia} \code{Madagascar} \code{Malasia} \code{Maldivas} \code{Mali} \code{Marruecos} \code{Mauricio} \code{Mejico} \code{Moldavia} \code{Mongolia} \code{Mozambique} \code{Nepal} \code{Nicaragua} \code{Niger} \code{Nigeria} \code{Pakistan} \code{Panama} \code{Papua Nueva Guinea} \code{Paraguay} \code{Peru} \code{Polonia} \code{Rep Dominicana} \code{Ruanda} \code{Rumania} \code{Rusia} \code{Samoa} \code{San Cristobal y Nieves} \code{San Vicente y las Granadinas} \code{Santa Lucia} \code{Senegal} \code{Seychelles} \code{Sierra Leona} \code{Suazilandia} \code{Sud Africa} \code{Sudan} \code{Tailandia} \code{Tanzania} \code{Tayikistan} \code{Togo} \code{Tonga} \code{Tunisia} \code{Turquia} \code{Ucrainia} \code{Uganda} \code{Uruguay} \code{Vanuatu} \code{Venezuela} \code{Vietnam}}
    \item{\code{Codigo.Pais}}{a factor with levels \code{AI} \code{ALB} \code{ARG} \code{ARM} \code{AZE} \code{BEN} \code{BGD} \code{BGR} \code{BIH} \code{BLR} \code{BLZ} \code{BOL} \code{BRA} \code{BWA} \code{CHL} \code{CHN} \code{CIV} \code{CMR} \code{COL} \code{CRI} \code{DMA} \code{DOM} \code{ECU} \code{EGY} \code{ETH} \code{FJI} \code{GEO} \code{GHA} \code{GIN} \code{GMB} \code{GRD} \code{GTM} \code{GUY} \code{HND} \code{HTI} \code{I} \code{IDN} \code{IND} \code{JAM} \code{JOR} \code{KAZ} \code{KEN} \code{KGZ} \code{KHM} \code{KNA} \code{LAO} \code{LBN} \code{LCA} \code{LSO} \code{LTU} \code{LVA} \code{MAR} \code{MDA} \code{MDG} \code{MDV} \code{MEX} \code{MKD} \code{MLI} \code{MNG} \code{MOZ} \code{MUS} \code{MYS} \code{NER} \code{NGA} \code{NIC} \code{NPL} \code{PAK} \code{PAN} \code{PER} \code{PHL} \code{PNG} \code{POL} \code{PRY} \code{ROM} \code{RUS} \code{RWA} \code{SAM} \code{SDN} \code{SEN} \code{SLB} \code{SLE} \code{SLV} \code{SWZ} \code{SYC} \code{TGO} \code{THA} \code{TJK} \code{TON} \code{TUN} \code{TUR} \code{TZA} \code{UGA} \code{UKR} \code{URY} \code{VCT} \code{VEN} \code{VNM} \code{VUT} \code{ZAF} \code{ZAR}}
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
Esta base contiene dos variables adicionales, respecto de BasePaises, que son el nombre y codigo de cada pais (vectores de caracteres).
Se trata de la base original y no puede ser utilizada como argumento de las funciones contenidas en este paquete puesto que contiene variables no numericas.
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
data(BasePaisesOriginal) 

#Depuracion de la base de datos:
#1)Eliminacion alternativas ideal/anti ideal, nombres y codigo de las alternativas:
Matriz_subindicadores <- BasePaisesOriginal[-c(99,100),-c(1,2)] 

#2)Establece el codigo de las alternativas como rownames
rownames(Matriz_subindicadores) <- as.character(BasePaisesOriginal[-c(99,100),2])

}
\keyword{datasets}
