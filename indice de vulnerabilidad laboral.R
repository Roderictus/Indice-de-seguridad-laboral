##########  Paquetes  ############
library(foreign)
library(stringr)
library(dplyr)
library(data.table)
library(ggplot2)
library(GGally)
#library(devtools)
library(ggbiplot)
library(rgdal)
######################################################
############ Bajar datos de la intercensal ###########
######################################################
URL<- "http://www.beta.inegi.org.mx/contenidos/proyectos/enchogares/especiales/intercensal/2015/microdatos/eic2015_"
for(i in 1:32) {
  download.file(url = str_c(URL, str_pad(i,2, "left", "0"), "_csv.zip"),
                destfile =  str_c("C:/Proyectos R/Informacion/Datos Intercensal/", i), mode = "wb")
  unzip(str_c("C:/Proyectos R/Informacion/Datos Intercensal/", i), exdir = "datos")
  file.remove(str_c("C:/Proyectos R/Informacion/Datos Intercensal/",i))
}

######################################################
############ Hacer un data.frame municipal ###########
######################################################
A<-data.frame()
for(i in 1:32){
  print(i)
    df <- read.csv(file = str_c("datos/TR_PERSONA",  str_pad(i, 2, "left", "0"), ".CSV"))
  Indice_Municipio <- df %>%
    group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
    summarise(Total = sum(FACTOR)) %>%
        left_join(
          df %>%
            group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
            filter( AGUINALDO %in% c(1)) %>%
            summarise( Con_Aguinaldo  = sum(FACTOR))
          ) %>%
  left_join(
    df %>%
      group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
      filter( AGUINALDO %in% c(2)) %>%
      summarise( Sin_Aguinaldo  = sum(FACTOR))
  ) %>%
    left_join(
      df %>%
        group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
        filter(VACACIONES %in% c(3)) %>%
        summarise( Vacaciones_si= sum(FACTOR))
    ) %>%
    left_join(
      df %>%
        group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
        filter(VACACIONES %in% c(4)) %>%
        summarise( Vacaciones_no= sum(FACTOR))
    )%>%
    left_join(
      df %>%
        group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
        filter( SERVICIO_MEDICO %in% c(5)) %>%
        summarise( Servicio_Medico_si= sum(FACTOR))
    )%>%
    left_join(
      df %>%
        group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
        filter( SERVICIO_MEDICO %in% c(6)) %>%
        summarise( Servicio_Medico_no= sum(FACTOR))
    )%>%
    left_join(
      df %>%
        group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
        filter( UTILIDADES %in% c(7)) %>%
        summarise( Utilidades_si= sum(FACTOR))
    )%>%
    left_join(
      df %>%
        group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
        filter( UTILIDADES %in% c(8)) %>%
        summarise( Utilidades_no = sum(FACTOR))
    )%>%
    left_join(
      df %>%
        group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
        filter( INCAP_SUELDO %in% c(1)) %>%
        summarise( Incapacidad_Sueldo_si = sum(FACTOR))
    )%>%
    left_join(
      df %>%
        group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
        filter( INCAP_SUELDO %in% c(2)) %>%
        summarise( Incapacidad_Sueldo_no = sum(FACTOR))
    )%>%
    left_join(
      df %>%
        group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
        filter( SAR_AFORE %in% c(3)) %>%
        summarise( SAR_AFORE_si = sum(FACTOR))
    )%>%
    left_join(
      df %>%
        group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
        filter( SAR_AFORE %in% c(4)) %>%
        summarise( SAR_AFORE_no = sum(FACTOR))
    )%>%
    left_join(
      df %>%
        group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
        filter( CREDITO_VIVIENDA %in% c(5)) %>%
        summarise( Credito_Vivienda_si = sum(FACTOR))
    )%>%
    left_join(
      df %>%
        group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
        filter( CREDITO_VIVIENDA %in% c(6)) %>%
        summarise( Credito_Vivienda_no = sum(FACTOR))
    )%>%
    mutate(Aguinaldo_ratio = (Con_Aguinaldo / Sin_Aguinaldo)) %>%
    mutate(Vacaciones_ratio = (Vacaciones_si / Vacaciones_no)) %>%
    mutate(Servicio_Medico_ratio = (Servicio_Medico_si/Servicio_Medico_no)) %>%
    mutate(Utilidades_ratio = (Utilidades_si/Utilidades_no)) %>%
    mutate(Incapacidad_Sueldo_ratio = (Incapacidad_Sueldo_si/Incapacidad_Sueldo_no)) %>%
    mutate(SAR_AFORE_ratio = (SAR_AFORE_si/SAR_AFORE_no)) %>%
    mutate(Credito_Vivienda_ratio = (Credito_Vivienda_si/Credito_Vivienda_no))
    
  A<-bind_rows(A, Indice_Municipio)
}
write.csv(x = A,file = "Indice por Municipio.csv")
A<- read.csv("Indice por Municipio.csv")

##################################################################################
##############  Análisis de Componentes Principales  #############################
##################################################################################

######lectura del archivo a nivel municipal

AC <-A[complete.cases(A),] #Perdemos 126 Municipios

#Descriptivos
graphA <-ggpairs(data = AC, 
        columns = 20:26, 
        title = "Relación Ratios",
        upper = list(continous = "density"), 
        lower = list(continous = "cor"),
        diag = list(continous = "bar"),
        axisLabels = "show")

#ACP
log.Ind <-log(AC[,20:26])

Ind.pca <- prcomp(log.Ind, 
                  center = TRUE, 
                  scale. = TRUE)
print(Ind.pca)
plot(Ind.pca, type = "l")
summary(Ind.pca)

g <-ggbiplot(Ind.pca, obs.scale = 1 , var.scale =1, 
           ellipse = TRUE, circle = TRUE)

########################################################
head(A)
min(AEdo$Aguinaldo_ratio)
colnames(AC)
AEdo$AG_norm        <- ((AEdo$Aguinaldo_ratio -min(AEdo$Aguinaldo_ratio))/max((AEdo$Aguinaldo_ratio -min(AEdo$Aguinaldo_ratio))))*100
AEdo$Vac_norm       <- ((AEdo$Vacaciones_ratio -min(AEdo$Vacaciones_ratio))/max((AEdo$Vacaciones_ratio -min(AEdo$Vacaciones_ratio))))*100
AEdo$SM_norm        <- ((AEdo$Servicio_Medico_ratio -min(AEdo$Servicio_Medico_ratio))/max((AEdo$Servicio_Medico_ratio -min(AEdo$Servicio_Medico_ratio))))*100
AEdo$Ut_norm        <- ((AEdo$Utilidades_ratio -min(AEdo$Utilidades_ratio))/max((AEdo$Utilidades_ratio -min(AEdo$Utilidades_ratio))))*100
AEdo$Inc_norm       <- ((AEdo$Incapacidad_Sueldo_ratio -min(AEdo$Incapacidad_Sueldo_ratio))/max((AEdo$Incapacidad_Sueldo_ratio -min(AEdo$Incapacidad_Sueldo_ratio))))*100
AEdo$SAR_AFORE_norm <- ((AEdo$SAR_AFORE_ratio -min(AEdo$SAR_AFORE_ratio))/max((AEdo$SAR_AFORE_ratio -min(AEdo$SAR_AFORE_ratio))))*100
AEdo$Cred_Viv_norm  <- ((AEdo$Credito_Vivienda_ratio -min(AEdo$Credito_Vivienda_ratio))/max((AEdo$Credito_Vivienda_ratio -min(AEdo$Credito_Vivienda_ratio))))*100
AEdo$SUMA_INDICE    <- AEdo$AG_norm + AEdo$Vac_norm + AEdo$SM_norm + AEdo$Ut_norm + 
                     AEdo$SAR_AFORE_ratio + AEdo$Cred_Viv_norm + AEdo$Inc_norm
AEdo$Indice_Norm    <- ((AEdo$SUMA_INDICE -min(AEdo$SUMA_INDICE))/max((AEdo$SUMA_INDICE -min(AEdo$SUMA_INDICE))))*100
AEdo$Indice_Norm2    <- ((AEdo$SUMA_INDICE -min(AEdo$SUMA_INDICE))/max((AEdo$SUMA_INDICE -min(AEdo$SUMA_INDICE))))*100
AEdo$CVUN <- str_c(str_pad(AEdo$ENT, width = 2, "left", "0"),
           str_pad(AEdo$MUN, width = 3, "left", "0"))

#######################################################
########################  Mapa  #######################
#######################################################
library(ggplot2)
library(rgdal)
library(viridis)

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

Indice_Municipios <- readOGR("C:/Proyectos R/Datos-Electorales/Mapa Reducido/areas_geoestadisticas_municipales.shp",
                             "areas_geoestadisticas_municipales")
Indice_Municipios$CVUN <- str_c(str_pad(Indice_Municipios$CVE_ENT, width = 2, "left", "0"),
                           str_pad(Indice_Municipios$CVE_MUN, width = 3, "left", "0"))
Indice_Municipios_Mapa <- fortify(Indice_Municipios, 
                                  region = "CVUN")
Indice_Municipios_Mapa$CVUN <- Indice_Municipios_Mapa$id

Indice_Municipios_Mapa<- left_join(x = Indice_Municipios_Mapa, y = AC, by = "CVUN")
################

no_classes <- 10
labels <- c()
quantiles <-quantile(Indice_Municipios_Mapa$Indice_Norm2,
                     probs = seq(0,1,length.out = no_classes + 1 ), na.rm = TRUE)
labels <- c()
for(idx in 1:length(quantiles)) {
  labels<-c(labels, paste0(round(quantiles[idx],2),
                           "-",
                           round(quantiles[idx + 1],2)))
}
labels <- labels[1:length(labels)-1]
AEdo$Indice_Municipios_Mapa <- cut(AEdo$Indice_Municipios_Mapa,
                      breaks = quantiles, 
                      labels = labels, 
                      include.lowest = T)

Plot_Municipio_Indice <-ggplot() + geom_polygon(data = Indice_Municipios_Mapa, aes(fill = Indice_Norm, 
                                                                              x = long, 
                                                                              y = lat, 
                                                                              group = group)) +
  geom_path(data = Indice_Municipios_Mapa, aes( x = long, 
                                          y = lat, 
                                          group = group),
            color = "white", size = 0.2) +
  coord_equal() +
  theme_map() +
  labs( x = NULL, 
        y = NULL, NULL,
        title = " Resultados del Índice de Seguridad Laboral a nivel Municipal") +
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "viridis",
    name = "Índice de Seguridad Laboral 2015",
    discrete = T,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = T
    ))
  

ggsave("ISL Municipal.png", plot = last_plot())
head(A)
###########################################################################################
#################  Data frame a nivel municipal   #########################################
###########################################################################################

AEdo <- 
  A %>%
  group_by(NOM_ENT) %>%
  summarise(Población = sum(Total), 
            ENT = mean(ENT),
            Con_Aguinaldo = sum(Con_Aguinaldo,na.rm = TRUE), 
            Sin_Aguinaldo = sum(Sin_Aguinaldo,na.rm = TRUE),
            Vacaciones_si = sum(Vacaciones_si,na.rm = TRUE),
            Vacaciones_no = sum(Vacaciones_no,na.rm = TRUE), 
            Servicio_Medico_si = sum(Servicio_Medico_si,na.rm = TRUE),
            Servicio_Medico_no = sum(Servicio_Medico_no,na.rm = TRUE),
            Utilidades_si = sum(Utilidades_si,na.rm = TRUE), 
            Utilidades_no = sum(Utilidades_no,na.rm = TRUE), 
            Incapacidad_Sueldo_si =sum(Incapacidad_Sueldo_si,na.rm = TRUE),
            Incapacidad_Sueldo_no =sum(Incapacidad_Sueldo_no,na.rm = TRUE),
            SAR_AFORE_si = sum(SAR_AFORE_si, na.rm = TRUE),
            SAR_AFORE_no = sum(SAR_AFORE_no, na.rm = TRUE),
            Credito_Vivienda_si = sum (Credito_Vivienda_si, na.rm = TRUE),
            Credito_Vivienda_no = sum (Credito_Vivienda_no, na.rm = TRUE)) %>%
  mutate(Aguinaldo_ratio = (Con_Aguinaldo / Sin_Aguinaldo), 
         Vacaciones_ratio = (Vacaciones_si / Vacaciones_no),
         Servicio_Medico_ratio = (Servicio_Medico_si/Servicio_Medico_no),
         Utilidades_ratio = (Utilidades_si/Utilidades_no),
         Incapacidad_Sueldo_ratio = (Incapacidad_Sueldo_si/Incapacidad_Sueldo_no),
         SAR_AFORE_ratio = (SAR_AFORE_si/SAR_AFORE_no),
         Credito_Vivienda_ratio = (Credito_Vivienda_si/Credito_Vivienda_no))

AEdo$AG_norm        <- ((AEdo$Aguinaldo_ratio -min(AEdo$Aguinaldo_ratio))/max((AEdo$Aguinaldo_ratio -min(AEdo$Aguinaldo_ratio))))*100
AEdo$Vac_norm       <- ((AEdo$Vacaciones_ratio -min(AEdo$Vacaciones_ratio))/max((AEdo$Vacaciones_ratio -min(AEdo$Vacaciones_ratio))))*100
AEdo$SM_norm        <- ((AEdo$Servicio_Medico_ratio -min(AEdo$Servicio_Medico_ratio))/max((AEdo$Servicio_Medico_ratio -min(AEdo$Servicio_Medico_ratio))))*100
AEdo$Ut_norm        <- ((AEdo$Utilidades_ratio -min(AEdo$Utilidades_ratio))/max((AEdo$Utilidades_ratio -min(AEdo$Utilidades_ratio))))*100
AEdo$Inc_norm       <- ((AEdo$Incapacidad_Sueldo_ratio -min(AEdo$Incapacidad_Sueldo_ratio))/max((AEdo$Incapacidad_Sueldo_ratio -min(AEdo$Incapacidad_Sueldo_ratio))))*100
AEdo$SAR_AFORE_norm <- ((AEdo$SAR_AFORE_ratio -min(AEdo$SAR_AFORE_ratio))/max((AEdo$SAR_AFORE_ratio -min(AEdo$SAR_AFORE_ratio))))*100
AEdo$Cred_Viv_norm  <- ((AEdo$Credito_Vivienda_ratio -min(AEdo$Credito_Vivienda_ratio))/max((AEdo$Credito_Vivienda_ratio -min(AEdo$Credito_Vivienda_ratio))))*100
AEdo$SUMA_INDICE    <- AEdo$AG_norm + AEdo$Vac_norm + AEdo$SM_norm + AEdo$Ut_norm + 
  AEdo$SAR_AFORE_ratio + AEdo$Cred_Viv_norm + AEdo$Inc_norm
AEdo$Indice_Norm    <- ((AEdo$SUMA_INDICE -min(AEdo$SUMA_INDICE))/max((AEdo$SUMA_INDICE -min(AEdo$SUMA_INDICE))))*100
AEdo$Indice_Norm2    <- ((AEdo$SUMA_INDICE -min(AEdo$SUMA_INDICE))/max((AEdo$SUMA_INDICE -min(AEdo$SUMA_INDICE))))*100
AEdo$CVE_ENT <- str_pad(AEdo$ENT, width = 2, "left", "0")

#########################################################################################
######################### Mapa Estatal  #################################################
#########################################################################################

Mapa_Estados <- readOGR("C:/Proyectos R/Datos-Electorales/Marco Geoestadistico/conjunto_de_datos/areas_geoestadisticas_estatales.shp",
                        "areas_geoestadisticas_estatales")
Mapa_Estados_F <- fortify(Mapa_Estados, region = "CVE_ENT")
Mapa_Estados_F$CVE_ENT <- Mapa_Estados_F$id

Mapa_Estados_F <- left_join(x = Mapa_Estados_F, y = AEdo, by = "CVE_ENT")

colnames(Mapa_Estados_F)

Plot_Estado_Indice <-ggplot() + geom_polygon(data = Mapa_Estados_F, aes(fill = Indice_Norm, 
                                                                                   x = long, 
                                                                                   y = lat, 
                                                                                   group = group)) +
  geom_path(data = Mapa_Estados_F, aes( x = long, 
                                                y = lat, 
                                                group = group),
            color = "white", size = 0.2) +
  coord_equal() +
  theme_map() +
  labs( x = NULL, 
        y = NULL, NULL,
        title = " Resultados del Índice de Seguridad Laboral a nivel Estatal") +
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "viridis",
    name = "Índice de Seguridad Laboral 2015",
    #discrete = T,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = T
    ))


ggsave("ISL Estatal.png", plot = last_plot())

#############################################################
############### Tabla de Resultados #########################
#############################################################

D<-arrange(.data = subset(x = AEdo, select = c(NOM_ENT, Indice_Norm)), -Indice_Norm)

kable(D)

