DropboxUrl <-
function(Url, outAsVector = TRUE)
{
     
      ##NOTAS IMPORTANTES:
      #Esta funcion sirve para obtener el url
      #valido para descargas directamente a R de archivos en dropbox
      #el url que le pasas es el que genera dropbox cuando 
      #seleccionas la opcion "get url" para cualquier archivo desde
      #dropbox (en internet)
      
      #La funcion esta "vectorizada" rudimentariamente usando
      #la funcion sapply (para hacerlo mas limpio si hace falta
      #obtener multiples archivos desde Dropbox)
      
      #Descripcion de los argumentos de la funcion:
      #Url: Vector o lista con una o multiples urls de dropbox
      #outAsVector: Output como vector o lista? (TRUE & FALSE)
      
  
      #Verdadera funcion
      Tempf <- function(Url)
      {
            
            #Separa el url
            UrlSplit <- unlist(strsplit(Url, split = "/"))
      
            #Nombre del archivo
            filename <- UrlSplit[length(UrlSplit)]
      
            #Dropbox key
            Dropboxkey <- UrlSplit[length(UrlSplit) - 1]
      
            #Url para downloads desde R
            Output <- paste("http://dl.dropbox.com/s", 
            Dropboxkey, filename, sep = "/")
      
            #Output
            Output
            
      } #Finaliza #Verdadera funcion
      
      #"Vectorizacion"
      Output <- sapply(Url, Tempf, simplify = outAsVector)
      names(Output) <- NULL
      
      #Output
      Output
  
      
}
