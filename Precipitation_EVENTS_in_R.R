#ACTUALIZACIÓN: 09/01/2024

#INFORMACIÓN:
#DESCARGA A TRAVES DE https://agrometeorologia.cl/RD


### SCRIPT PARA CALCULAR EVENTOS DE PRECIPITACION----

#LIBRERIAS-----
library(ggplot2)
library(tidyverse)


#EJEMPLO CON ESTACION INIA CAUQUENES#
Estacion_PP<-read.csv("C:/Materiales_Tesis/Precipitacion_ANALISIS/Resultados/Cauquenes, CauquenesBD.csv")
Estacion_PP$Fecha  <- ifelse(nchar(Estacion_PP$Fecha ) == 10, paste(Estacion_PP$Fecha , "00:00:00"), Estacion_PP$Fecha )
Estacion_PP$Fecha<-as.POSIXct(Estacion_PP$Fecha,format="%Y-%m-%d %H:%M:%OS", tz="UTC")
Estacion_PP<-Estacion_PP %>% select(Fecha,Precipitacion)


#FUNCION QUE DEBE ENTRAR EN 2 COL (FECHA.HOR Y PRECIPITACION)------
calcular_eventos<- function(estacion_pp) {
datos_selec<-estacion_pp
colnames(estacion_pp)<-c("DateTime_UTC","Precipitacion")
colnames(datos_selec)<-c("DateTime_UTC","Precipitacion")

        
#CRITERIO 1: MAYOR A 0.1 MM HORARIOS -----
datos_selec<-datos_selec %>% drop_na()
datos_selec$pp<-ifelse(datos_selec$Precipitacion>0.1,"V","F")
rle_resultado <- rle(datos_selec$pp)
rle_df <- data.frame(values = rle_resultado$values,lengths = rle_resultado$lengths)
rle_df$fecha_inicio <- as.character(NA)
        
for (h in 1:length(rle_df$lengths)) {
if (h>1) {
pos<-rle_df$lengths[h]+pos  
}else{
pos<-rle_df$lengths[h]   
rle_df[h,4]<-as.POSIXct(datos_selec$DateTime_UTC[pos],tz="UTC")}
rle_df[h,4]<-as.POSIXct(datos_selec$DateTime_UTC[pos],tz="UTC")
}
        
rle_df[1,3]<-datos_selec$DateTime_UTC[1]
        
for (h in 2:length(rle_df$fecha_inicio)) {
rle_df[h,3]<-rle_df[h-1,4] + 3600
}

rle_df[1,3]<-as.POSIXct(rle_df[1,3],"%Y-%m-%d %H:%M:%S",tz="UTC")
rle_df[1,3]<-datos_selec$DateTime_UTC[1]
tiempos_posix <- as.POSIXct(as.numeric(rle_df[[3]]), origin = '1970-01-01', tz = "UTC")
rle_df$fecha_inicio<-tiempos_posix
colnames(rle_df)<-c("PP","Horas","Inicio","Final")
        
        
eventos_all<-rle_df 
eventos_all$Accum<-"NA"
        
        
for (h in 1:length(eventos_all$Inicio)) {
datos_selec<-estacion_pp %>% filter(DateTime_UTC>=eventos_all[h,3] & DateTime_UTC<= eventos_all[h,4])  
colnames(datos_selec)<-c("DateTime_UTC","Precipitacion")
eventos_all[h,5]<-datos_selec$Precipitacion %>% sum()
                
                
        }
        
#CRITERIO 2: DIFERENCIA DE MENOS DE 6 HORAS --------
eventos<-eventos_all%>% filter(PP=="V")
eventos$Accum <- as.numeric(as.character(eventos$Accum))
filas_a_eliminar <- c()
        
#ACA ESTA EL CRITERIO DE HORAS ENTRE 
for(i in 1:(nrow(eventos) - 1)) {
                
# Calcular la diferencia en horas
horas_diferencia <- difftime(eventos$Inicio[i + 1], eventos$Final[i], units = "hours")
# Unir filas si la diferencia es menor a 6 horas 
if(as.numeric(horas_diferencia) < 6) {
eventos$Accum[i] <- eventos$Accum[i] + eventos$Accum[i + 1]
eventos$Horas[i] <- eventos$Horas[i] + eventos$Horas[i + 1]
eventos$Final[i] <- eventos$Final[i + 1]
filas_a_eliminar <- c(filas_a_eliminar, i + 1)
}}
        
        
eventos_combinados <- eventos[-filas_a_eliminar,]
eventos_combinados <- eventos_combinados %>% arrange(Inicio)
eventos_combinados<-eventos_combinados %>% mutate(Intensidad=Accum/Horas)
print(eventos_combinados)
        
        
#DIFERENCIA ENTRE EVENTOS
eventos_combinados$Inicio_siguiente <- lead(eventos_combinados$Inicio, default = last(eventos_combinados$Inicio))
eventos_combinados$Diferencia_horas <- as.numeric(difftime(eventos_combinados$Inicio_siguiente, eventos_combinados$Final, units = "hours"))
        
#CRITERIO 3: ¿EMPIEZA EL OTRO EVENTO ANTES DE 5 DIAS ?------
eventos_combinados$Criterio <- ifelse(eventos_combinados$Diferencia_horas < (5 * 24), "V", "F") 

eventos_combinados <- eventos_combinados %>%
mutate(Fecha_Final = if_else(Criterio == "V",
Inicio_siguiente - hours(1),
Final + days(5)))
        
return(eventos_combinados)
}


###EJEMPLO####
eventos.df<-calcular_eventos(Estacion_PP)

# Crear la serie de tiempo de precipitación
p<-ggplot(data = Estacion_PP, aes(x = Fecha, y = Precipitacion)) +
        geom_line() + # Utilizar una línea para conectar los puntos
        labs(title = "Serie de Tiempo de Precipitación",
             x = "Fecha",
             y = "Precipitación (mm)") +
        theme_minimal() + # Tema minimalista adecuado para publicaciones
        theme(axis.text.x = element_text(angle = 45, hjust = 1))


eventos.intensos<-eventos.df[eventos.df$Intensidad>5,] %>% drop_na()
eventos.intensos %>% str()

for(fecha in unique(eventos.intensos$Inicio)) {
        p <- p + geom_vline(xintercept =fecha, linetype="dashed", color = "red")
}

print(p)


write.csv(eventos.df,"C:/Materiales_Tesis/eventos.df.csv")
