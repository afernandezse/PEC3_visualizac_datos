setwd("/home/flatline/Documentos/Master_Data_Science/aaVisualizacion_datos/PEC3")

library(dplyr)
library(lubridate)

## Origen de los datos en crudo

# https://analisi.transparenciacatalunya.cat/Salut/COVID-19-Persones-hospitalitzades/hzw2-sfyd

hospit <- read.csv("COVID-19__Persones_hospitalitzades.csv")


hospit_group <- hospit %>%
  group_by(data_inici, nom_regio, sexe, grup_edat, index_socioeconomic) %>% 
  summarise(hospitalizaciones = sum(casos))


## Origen de los datos en crudo

# https://analisi.transparenciacatalunya.cat/Salut/COVID-19-Persones-ingressades-en-unitats-de-cr-tic/bm7c-5yei




uci <- read.csv("COVID-19__Persones_ingressades_en_unitats_de_cr_tics.csv")

uci_group <- uci %>%
  group_by(data_inici, nom_regio) %>% 
  summarise(Ingresos_UCI = sum(Ingresos_UCI))


## Origen de los datos en crudo

# https://analisi.transparenciacatalunya.cat/Salut/COVID-19-Defuncions/yu4b-y3q5


def <- read.csv("COVID-19__Defuncions.csv")

def_group <- def %>%
  group_by(data_inici, nom_regio, sexe, grup_edat, index_socioeconomic) %>% 
  summarise(defunciones = sum(defunciones))


data <- merge(hospit, def, by=c("data_inici", "nom_regio", "sexe" ,
                                     "grup_edat", "index_socioeconomic"))

hosp_def_group <- merge(hospit_group, def_group, by=c("data_inici", "nom_regio", "sexe" ,
                                "grup_edat", "index_socioeconomic"))

data_group <- merge(hosp_def_group, uci_group, by=c("data_inici", "nom_regio"))


data <- data %>% select(-ends_with(".y"))

data_group <- data %>%
  group_by(data_inici, nom_regio) %>% 
  summarise(casos = sum(casos), defunciones = sum(defunciones))


### Grouped by date and Region

data_group_date <- data_group %>%
  group_by(data_inici, nom_regio) %>% 
  summarise(hospitalizaciones = sum(hospitalizaciones),
            defunciones = sum(defunciones), Ingresos_UCI=mean(Ingresos_UCI))

data_group_date_ord <- data_group_date[order(as.Date(dmy(data_group_date$data_inici))),]


write.csv(data_group_date_ord, "hosp_uci_def_date.csv")



## Plots hospitalizaciones defunciones

hosp_def_sexe <- hosp_def_group %>%
  group_by(sexe) %>% 
  summarise(hospitalizaciones = sum(hospitalizaciones),
            defunciones = sum(defunciones))


write.csv(hosp_def_sexe, "hosp_def_sexe.csv")


hosp_def_edat <- hosp_def_group %>%
  group_by(grup_edat) %>% 
  summarise(hospitalizaciones = sum(hospitalizaciones),
            defunciones = sum(defunciones))

write.csv(hosp_def_edat, "hosp_def_edat.csv")


## Origen de los datos en crudo

## https://analisi.transparenciacatalunya.cat/Societat-benestar/La-societat-catalana-davant-del-Covid-19-Percepcio/qxjr-krv6

sentim <- read.csv("La_societat_catalana_davant_del_Covid-19._Percepcions__estats_d__nim_i_preocupacions..csv")


sentim_onada <- sentim %>%
  group_by(onada) %>% 
  summarise(nerviosismo = mean(na.omit(as.numeric(p23_1_nerviós))),
            irritacion = mean(na.omit(as.numeric(p23_2_irritat))),
            alegria = mean(na.omit(as.numeric(p23_3_alegre))),
            melancolia = mean(na.omit(as.numeric(p23_4_melancolic))),
            tension = mean(na.omit(as.numeric(p23_5_tens))),
            optimismo = mean(na.omit(as.numeric(p23_6_optimista))),
            desanimo = mean(na.omit(as.numeric(p23_7_desanimat))),
            ira = mean(na.omit(as.numeric(p23_8_enutjat))),
            ansiedad = mean(na.omit(as.numeric(p23_9_ansios))),
            apagado = mean(na.omit(as.numeric(p23_10_apagat))),
            molesto = mean(na.omit(as.numeric(p23_11_molest))),
            animado = mean(na.omit(as.numeric(p23_12_animat))),
            intranquilo = mean(na.omit(as.numeric(p23_13_intranquil))),
            contento = mean(na.omit(as.numeric(p23_15_content))),
            triste = mean(na.omit(as.numeric(p23_16_trist))),
            )


write.csv(sentim_onada, "sentim_onada.csv")


sentim_prov <- sentim %>%
  group_by(provincia) %>% 
  summarise(nerviosismo = mean(na.omit(as.numeric(p23_1_nerviós))),
            irritacion = mean(na.omit(as.numeric(p23_2_irritat))),
            alegria = mean(na.omit(as.numeric(p23_3_alegre))),
            melancolia = mean(na.omit(as.numeric(p23_4_melancolic))),
            tension = mean(na.omit(as.numeric(p23_5_tens))),
            optimismo = mean(na.omit(as.numeric(p23_6_optimista))),
            desanimo = mean(na.omit(as.numeric(p23_7_desanimat))),
            ira = mean(na.omit(as.numeric(p23_8_enutjat))),
            ansiedad = mean(na.omit(as.numeric(p23_9_ansios))),
            apagado = mean(na.omit(as.numeric(p23_10_apagat))),
            molesto = mean(na.omit(as.numeric(p23_11_molest))),
            animado = mean(na.omit(as.numeric(p23_12_animat))),
            intranquilo = mean(na.omit(as.numeric(p23_13_intranquil))),
            contento = mean(na.omit(as.numeric(p23_15_content))),
            triste = mean(na.omit(as.numeric(p23_16_trist))),
  )


write.csv(sentim_prov, "sentim_provincia.csv")


sentim_sex <- sentim %>%
  group_by(sexe) %>% 
  summarise(nerviosismo = mean(na.omit(as.numeric(p23_1_nerviós))),
            irritacion = mean(na.omit(as.numeric(p23_2_irritat))),
            alegria = mean(na.omit(as.numeric(p23_3_alegre))),
            melancolia = mean(na.omit(as.numeric(p23_4_melancolic))),
            tension = mean(na.omit(as.numeric(p23_5_tens))),
            optimismo = mean(na.omit(as.numeric(p23_6_optimista))),
            desanimo = mean(na.omit(as.numeric(p23_7_desanimat))),
            ira = mean(na.omit(as.numeric(p23_8_enutjat))),
            ansiedad = mean(na.omit(as.numeric(p23_9_ansios))),
            apagado = mean(na.omit(as.numeric(p23_10_apagat))),
            molesto = mean(na.omit(as.numeric(p23_11_molest))),
            animado = mean(na.omit(as.numeric(p23_12_animat))),
            intranquilo = mean(na.omit(as.numeric(p23_13_intranquil))),
            contento = mean(na.omit(as.numeric(p23_15_content))),
            triste = mean(na.omit(as.numeric(p23_16_trist))),
  )


write.csv(sentim_sex, "sentim_sexe.csv")




########### Certificdos desplazamientos

## Origen de los datos en crudo

## https://analisi.transparenciacatalunya.cat/Sector-P-blic/Nombre-de-certificats-autoresponsables-de-despla-a/nbeu-ihqy

desplaz <- read.csv("Nombre_de_certificats_autoresponsables_de_despla_ament_extrets_pels_ciutadans_a_trav_s_de_l_aplicaci__ConfinApp._Dades_compreses_entre_els_per_odes_del_23_03_2020_al_12_05_2.csv")

desplaz$total_certificats <- as.numeric(desplaz$total_certificats)

desplaz <- desplaz[order(as.Date(dmy(desplaz$data))),]



desplaz_data <- desplaz %>%
  group_by(data) %>% 
  summarise(certificados = sum(total_certificats)
  )


write.csv(desplaz_data, "desplazamiento_data.csv")


desplaz_com <- desplaz %>%
  group_by(nom_comarca) %>% 
  summarise(certificados = sum(total_certificats)
  )


write.csv(desplaz_com, "desplazamiento_comarca.csv")


## Origen de los datos en crudo

## https://analisi.transparenciacatalunya.cat/Sector-P-blic/Nombre-de-certificats-autoresponsables-de-despla-a/nbeu-ihqy


seq <- read.csv("COVID-19__Seq_enciaci__de_variants.csv")

seq_com_var <- seq %>%
  group_by(nom_regio, variant) %>% 
  summarise(Variante = sum(recompte)
  )




seq_var <- seq %>%
  group_by(variant) %>% 
  summarise(Variante = sum(recompte)
  )


write.csv(seq_var, "seq_variants.csv")




save.image("PEC2_vd_datasets_22122022.RData")