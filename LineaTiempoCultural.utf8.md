---
title: "Linea de tiempo"
author: "Bernardo Ancidey"
date: "23 de enero de 2020"
output: pdf_document
---



## Liberías

Se suben las librerías


```r
library(ggplot2)
library(scales)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

## Datos

Se cargan los datos:


```
##    month year                                   milestone      status
## 1      6 1818                          Correo del Orinoco     Cerrada
## 2      6 1827                                         UCV   Existente
## 3      1 1832                                         ULA   Existente
## 4      7 1833                         Biblioteca Nacional   Existente
## 5      1 1836               Archivo General de la Nación    Existente
## 6     10 1872                              Gaceta Oficial   Existente
## 7      2 1875 Museos de Bellas Artes y Ciencias Naturales   Existente
## 8      4 1881                 Teatro Municipal de Caracas   Existente
## 9      7 1883                  Teatro Baralt en Maracaibo   Existente
## 10     1 1886                       Teatro Puerto Cabello   Existente
## 11    10 1887           Academia Nacional de Bellas Artes   Existente
## 12    10 1894                             Teatro Valencia   Existente
## 13     6 1905                             Teatro Nacional   Existente
## 14     4 1917                 Protección patrimonial M.I.     Cerrada
## 15     6 1930             Orquesta Sinfónica de Venezuela   Existente
## 16     7 1936                 Radio Nacional de Venezuela   Existente
## 17     6 1936          Dir.de Cultura y Bellas Artes M.E.     Cerrada
## 18     6 1940        Dir. Cultura y Bienestar Social M.T.     Cerrada
## 19     6 1948                  Escuela Nacional de Ballet     Cerrada
## 20    11 1952                         Televisora Nacional   Existente
## 21     4 1960                                      INCIBA     Cerrada
## 22     5 1966            Cinemateca Nacional de Venezuela    Renovada
## 23     4 1968                        Monte Ávila Editores   Existente
## 24     7 1971           Conservatorio  Juan José Landaeta   Existente
## 25    11 1973                           Casa Andrés Bello   Existente
## 26    10 1974                                         GAN   Existente
## 27     9 1974                         Biblioteca Ayacucho   Existente
## 28     7 1974                                      CELARG   Existente
## 29     6 1975                 Conservatorio Simón Bolívar   Existente
## 30     8 1975                                       CONAC   Existente
## 31     4 1976                    Venezolana de Televisión   Existente
## 32     2 1976                       Teatro Teresa Carreño   Existente
## 33     2 1978             Fundación Musical Simón Bolívar   Existente
## 34    12 1982                           Librerías del Sur   Existente
## 35     5 1984                 Compañía Nacional de Teatro   Existente
## 36     3 1987                            Casa del Artista   Existente
## 37     5 1987                 Compañía Nacional de Música   Existente
## 38     9 1990           Centro de la Diversidad Cultural    Existente
## 39     9 1993            Instituto de Patrimonio Cultural   Existente
## 40     9 1993                                       CENAC   Existente
## 41    12 1996            Centro Nacional de la Fotografía   Existente
## 42     4 1997                                       CENAL   Existente
## 43     4 2005                           Museos Nacionales   Existente
## 44     5 2005                                 Red de Arte   Existente
## 45     7 2005                                      IARTES   Existente
## 46     7 2005                                        IAEM   Existente
## 47     2 2005                                        MPPC   Existente
## 48     3 2006                              Misión Cultura   Existente
## 49    12 2006                      Imprenta de la Cultura   Existente
## 50     6 2006                              Villa del Cine   Existente
## 51     2 2006      Distribuidora Venezolana de la Cultura   Existente
## 52    12 2006                                      CENDIS   Existente
## 53     2 2006                  Compañía Nacional de Danza   Existente
## 54     2 2006                              Amazonia Films   Existente
## 55     2 2006                Editorial El Perro y La Rana   Existente
## 56    10 2007                 Centro Nacional de Historia   Existente
## 57     5 2008                                     UNEARTE   Existente
## 58     1 2008                                 Alba Ciudad   Existente
## 59     8 2009                        Correo del Orinoco 2 Nueva Etapa
## 60     5 2013                   Editorial Alfredo Maneiro   Existente
## 61     9 2013                    Movimiento César Rengifo   Existente
## 62     8 2016                             Corazón Llanero   Existente
## 63     4 2017                              Corazón Urbano   Existente
## 64     2 2017                             Corazón Salsero   Existente
```

Lo primero que haremos es definir una fecha para cada una de estas filas como el día 1 del mes.




```r
df$date <- with(df, ymd(sprintf('%04d%02d%02d', year, month, 1)))
df <- df[with(df, order(date)), ]
head(df)
```

```
##   month year                     milestone    status       date
## 1     6 1818            Correo del Orinoco   Cerrada 1818-06-01
## 2     6 1827                           UCV Existente 1827-06-01
## 3     1 1832                           ULA Existente 1832-01-01
## 4     7 1833           Biblioteca Nacional Existente 1833-07-01
## 5     1 1836 Archivo General de la Nación  Existente 1836-01-01
## 6    10 1872                Gaceta Oficial Existente 1872-10-01
```
A continuación se convierte el estado en una variable categórica ordinal, en orden de criticidad que va de "Existente a "Cerrada". También definiremos algunos valores de color hexadecimales para asociarlos a estos estados.


```r
status_levels <- c("Existente", "Renovada", "Nueva Etapa", "Cerrada")
status_colors <- c("#0070C0", "#00B050", "#FFC000", "#C00000")

df$status <- factor(df$status, levels=status_levels, ordered=TRUE)
```

En la línea de tiempo, se quiere variar la altura y la dirección de las líneas, porque de lo contrario el texto de los hitos chocará.
Se necesita asignar las líneas y las alturas para que los hitos del mismo mes sean los mismos, por lo que sólo se cambian los valores de altura y posición.

A continuación, se ordena los datos por fecha y estado, de modo que el estado "cerrado"" se traza en último lugar y los colores que se muestran son para el estado de los hitos cerrados.


```r
positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
directions <- c(1, -1)

line_pos <- data.frame(
    "date"=unique(df$date),
    "position"=rep(positions, length.out=length(unique(df$date))),
    "direction"=rep(directions, length.out=length(unique(df$date)))
)

df <- merge(x=df, y=line_pos, by="date", all = TRUE)
df <- df[with(df, order(date, status)), ]

head(df)
```

```
##         date month year                     milestone    status position
## 1 1818-06-01     6 1818            Correo del Orinoco   Cerrada      0.5
## 2 1827-06-01     6 1827                           UCV Existente     -0.5
## 3 1832-01-01     1 1832                           ULA Existente      1.0
## 4 1833-07-01     7 1833           Biblioteca Nacional Existente     -1.0
## 5 1836-01-01     1 1836 Archivo General de la Nación  Existente      1.5
## 6 1872-10-01    10 1872                Gaceta Oficial Existente     -1.5
##   direction
## 1         1
## 2        -1
## 3         1
## 4        -1
## 5         1
## 6        -1
```

Si hay múltiples hitos para un mes determinado, se necesita alterar ligeramente sus posiciones (ligeramente más alto si está por encima de la línea de tiempo y ligeramente más bajo si está por debajo).

Se puede hacer un recuento acumulativo de fechas individuales para comprobar si se tienen múltiples hitos para un mes determinado.



```r
text_offset <- 0.05

df$month_count <- ave(df$date==df$date, df$date, FUN=cumsum)
df$text_position <- (df$month_count * text_offset * df$direction) + df$position
head(df)
```

```
##         date month year                     milestone    status position
## 1 1818-06-01     6 1818            Correo del Orinoco   Cerrada      0.5
## 2 1827-06-01     6 1827                           UCV Existente     -0.5
## 3 1832-01-01     1 1832                           ULA Existente      1.0
## 4 1833-07-01     7 1833           Biblioteca Nacional Existente     -1.0
## 5 1836-01-01     1 1836 Archivo General de la Nación  Existente      1.5
## 6 1872-10-01    10 1872                Gaceta Oficial Existente     -1.5
##   direction month_count text_position
## 1         1           1          0.55
## 2        -1           1         -0.55
## 3         1           1          1.05
## 4        -1           1         -1.05
## 5         1           1          1.55
## 6        -1           1         -1.55
```
Como se quiere mostrar todos los meses en las líneas de tiempo, no sólo los meses para los que hay eventos, se crear un dataframe que contenga todos los meses.

Se empieza 2 meses antes del primer hito y se termina 2 meses después del último hito para tener un poco de memoria intermedia.


```r
month_buffer <- 2

month_date_range <- seq(min(df$date) - months(month_buffer), max(df$date) + months(month_buffer), by='month')
month_format <- format(month_date_range, '%b')
month_df <- data.frame(month_date_range, month_format)
```

Se hace lo mismo para los años que también se quieren mostrar.

Sólo se mostrarán los años para los que hay un cruce de diciembre/enero, esto es lo que está haciendo la línea de intersección (intersect).


```r
year_date_range <- seq(min(df$date) - months(month_buffer), max(df$date) + months(month_buffer), by='year')
year_date_range <- as.Date(
    intersect(
        ceiling_date(year_date_range, unit="year"),
        floor_date(year_date_range, unit="year")
    ),  origin = "1970-01-01"
)
year_format <- format(year_date_range, '%Y')
year_df <- data.frame(year_date_range, year_format)
```

Ahora que se tienen los datos en un estado listo para ser trazados, se puede armar el gráfico.


```r
timeline_plot<-ggplot(df,aes(x=date,y=0, col=status, label=milestone))
timeline_plot<-timeline_plot+labs(col="Hitos")
timeline_plot<-timeline_plot+scale_color_manual(values=status_colors, labels=status_levels, drop = FALSE)
timeline_plot<-timeline_plot+theme_classic()

# Trazar una línea negra horizontal para la línea de tiempo
timeline_plot<-timeline_plot+geom_hline(yintercept=0, 
                color = "black", size=0.3)

# Trazar líneas de segmentos verticales para hitos
timeline_plot<-timeline_plot+geom_segment(data=df[df$month_count == 1,], aes(y=position,yend=0,xend=date), color='black', size=0.2)

# Trazar los puntos de dispersión en cero y la fecha
timeline_plot<-timeline_plot+geom_point(aes(y=0), size=3)

# No muestra los ejes, posiciona apropiadamente la leyenda
timeline_plot<-timeline_plot+theme(axis.line.y=element_blank(),
                 axis.text.y=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 axis.ticks.y=element_blank(),
                 axis.text.x =element_blank(),
                 axis.ticks.x =element_blank(),
                 axis.line.x =element_blank(),
                 legend.position = "bottom"
                )

# Mostrar el texto de cada mes
# timeline_plot<-timeline_plot+geom_text(data=month_df, aes(x=month_date_range,y=-0.1,label=month_format),size=2.5,vjust=0.5, color='black', angle=90)
# Mostrar el texto del año
 timeline_plot<-timeline_plot+geom_text(data=year_df, aes(x=year_date_range,y=-0.2,label=year_format, fontface="bold"), angle=90,size=2.5, color='black')
#Mostrar el texto de cada hito
timeline_plot<-timeline_plot+geom_text(aes(y=text_position,label=milestone),size=2.5)
print(timeline_plot)
```

![](LineaTiempoCultural_files/figure-latex/lineaTiempo-1.pdf)<!-- --> 

