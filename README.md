#Paquete Indicadores Compuestos (MCDA)

|:---------------|:-------------------------------------------------------|
|**Autor:**      | Minolli Santiago
|**Version:**    | 1.3
|**Fecha:**      | Abril 2014
|**Contacto:**   | minollisantiago@gmail.com
|**Repository:** | [Link al Repositorio en GitHub](https://github.com/minollisantiago/IndCompuestos)

###Guia de instalacion

1. En primer lugar es necesario instalar y cargar el paquete devtools:

```{r}
install.packages("devtools")

library(devtools)
```

2. Descargar la carpeta IndCompuestos (hay que descomprimirla despues de bajarla de dropbox),
   luego Instalar el paquete corriendo la funcion install() simplemente definiendo la ruta en 
   la que se encuentra la carpeta IndCompuestos.
   En el siguiente ejemplo baje y descomprimi la carpeta IndCompuestos en mi escritorio, para 
   separar los nombres de las carpetas pueden usar cualquiera de estos dos separadores "/" o "\\".
   Es necesario que la ruta este entre comillas "":

```{r}
install("C:/Users/usuario/Desktop/IndCompuestos")
```

3. Finalmente, para cargar el paquete en su sesion:

```{r}
library(IndCompuestos)
```

