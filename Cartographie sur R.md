# Cartes de logements sociaux pour la région AURA


``` r
library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(grid)
```

Une fois les données et les découpages (en format shp) importés, il est nécessaire de pré-découper les sections d'intérêt.
Nous cherchons à illustrer principalement deux variables:
- Les effectifs du logement social
- Les effectifs des ventes HLM.
Comme les informations pour ces deux variables se trouvent dans des tableaux différents, nous effectuerons le décupage pour chacune d'entre elles individuellement. 

### Découpages

``` r
#Auvergne-Rhone-Alpes COM
region_logts_com <- FR_logts_com[FR_logts_com$DEP %in% c("01","03","07","15","26","38","42","43","63","69","73","74"),]
#Auvergne-Rhone-Alpes DEP
region_shp_dep <- shp_DEP[shp_DEP$code %in% c("01","03","07","15","26","38","42","43","63","69","73","74"),]
#Rhône:
rhone_logts_com <- FR_logts_com[FR_logts_com$DEP== '69' & !is.na(FR_logts_com$DEP),]
#Rhône DEP
rhone_logts_dep <-  shp_DEP[shp_DEP$code=='69',]
#Métropole de lyon:
lyon_metropole_com <- FR_logts_com[FR_logts_com$insee %in% c("69123","69003","69029","69033","69034","69040","69044","69046","69271",
                                                         "69063","69273","69068","69069","69071","69072","69275","69081","69276",
                                                         "69085","69087","69088","69089","69278","69091","69096","69100","69279",
                                                         "69116","69117","69127","69282","69283","69284","69142","69143","69149",
                                                         "69152","69153","69163","69286","69168","69191","69194","69202","69199",
                                                         "69204","69205","69207","69290","69233","69292","69293","69296","69244",
                                                         "69250","69256","69259","69260","69266","69381","69382","69383","69384",
                                                         "69385","69386","69387","69388","69389"), ]
#Lyon:
lyon_com <- FR_logts_com[FR_logts_com$insee %in% c("69123","69381","69382","69383","69384","69385","69386","69387","69388","69389"), ]

#Isère:
isere_logts_com <- FR_logts_com[FR_logts_com$DEP== "38" & !is.na(FR_logts_com$DEP),]
#isère DEP
isere_logts_dep <-  shp_DEP[shp_DEP$code=='38',]
#Grenoble:
Grenoble_metropole <- FR_logts_com[FR_logts_com$insee %in% c("38185","38057","38059","38071","38068","38111","38126","38150","38151",
                                                             "38158","38169", "38170","38179","38187","38188","38200","38229","38235",
                                                             "38258","38252","38271","38277","38279","38281","38309","38317","38325",
                                                             "38328","38364","38382","38388","38421","38423","38436","38445","38471",
                                                             "38472","38474","38478","38485","38486","38516","38524","38528","38529",
                                                             "38533","38540","38545","38562"),]

# On répete le processus pour le fichier de ventes [...]
```
### CARTES

Il est également utile de définir dorénavant les paramètres qui sont répétés pour chaque carte tels que l'échelle, la réalisation et la source. 

``` r
# > Defaults----
carte_source1 <- tm_credits(text="Source : XXXX \nRéalisation : Carolina Borré, UMR EVS et Florence Goffette-Nagot, UMR GATE", position = c("LEFT", "BOTTOM"), size= 0.7, col = "black")
escala <- tm_scale_bar(position = c("RIGHT", "BOTTOM"), width = 0.11)
```

#### Effectifs de logements sociaux + variation du parc entre 2013-2020

###### Auvergne-Rhone-Alpes
``` r
#On observe d'abbord la distribution des nos variables: 

#Delta 2013-2020. 
#On garde que les communes ayant des logements HLM dans les 2 périodes car si la commune n'a pas de logements dans une des périodes, alors la variation entre 2013-2020 peut donner un résultat comme NaN (0/0), Inf (nb/0) ou 0 (0/nb) ce qui va nous empêcher de regarder la distribution de la variable. 

no_na_delta <- region_logts_com[!is.na(region_logts_com$delta_HLM_13_20) & !is.infinite(region_logts_com$delta_HLM_13_20) & region_logts_com$delta_HLM_13_20!=0 , ]
qdelta1320_region <- quantile(no_na_delta$delta_HLM_13_20, na.rm = T, probs = seq(0,1, by =0.1)) %>% as.data.frame()

region_logts_com <- mutate(region_logts_com, 
                           qgroup_delta1320 = case_when(delta_HLM_13_20>= -100 & delta_HLM_13_20 < (-50) ~"[−100 à −50)",
                                                      delta_HLM_13_20>= -50 & delta_HLM_13_20 < (-10) ~ "[−50 à -10)",
                                                      delta_HLM_13_20>= -10 & delta_HLM_13_20 < 0 ~"[−10 à 0)",
                                                      delta_HLM_13_20== 0 ~"0",
                                                      delta_HLM_13_20> 0 & delta_HLM_13_20 <10 ~"[0 à 10)",
                                                      delta_HLM_13_20>= 10 & delta_HLM_13_20 < 50 ~"[10 à 50)",
                                                      delta_HLM_13_20>= 50 & delta_HLM_13_20 <= 100 ~"[50−100]",
                                                      delta_HLM_13_20> 100~ ">100",
                                                      is.na(delta_HLM_13_20)~"Aucun logement social dans la commune"))

region_logts_com$qgroup_delta1320 <- factor(region_logts_com$qgroup_delta1320,
                                          levels = c("[−100 à −50)","[−50 à -10)","[−10 à 0)","0","[0 à 10)","[10 à 50)","[50−100]",">100","Aucun logement social dans la commune"))
                                          
qgroup_delta1320_region <- table(region_logts_com$qgroup_delta1320) %>% as.data.frame()
#QHLM_20
no_o <- region_logts_com[region_logts_com$QHLM_20 !=0, ]
qhlm20_region <- quantile(no_o$QHLM_20, na.rm = T, probs = seq(0,1, by =0.1)) %>% as.data.frame()

```

Carte:
``` r
parc_region <- tm_shape(region_shp_dep) + 
               tm_polygons(col="white", border.col = "black", lwd = 0.6)

parc_region +  tm_shape(region_logts_com) +
               tm_symbols(col = "qgroup_delta1320", size = "QHLM_20", scale = 5.5,
                          border.lwd = NA, alpha = 0.8,  sizes.legend = c(5,100,1000,10000,21329),
                          sizes.legend.labels = format(c(5,100,1000,10000,21329), big.mark=" ", trim=TRUE),
                          palette = c("#0000FF","#3991C3","#83BBDB","#808080","#FEC756","#F88B22","#EA6E13","#B34002","#FFFFFF"),
                          textNA = " ", colorNA=NULL,
                          title.size = "Effectifs en 2020", title.col = "Taux de variation 2013-2020\n(en %)",  legend.size.is.portrait = TRUE,
                          shapes.legend.fill = "#FEC756") + 
  
               carte_source1 +
               tm_scale_bar(position = c(0.7,0.05), width = 0.11)+
               tm_layout(title = "Logements sociaux\npar commune : effectifs 2020 et\nvariation 2013−2020", title.position = c("LEFT","TOP"),
                         inner.margins=c(0.1,0.3,0.01,0.05), main.title.size = 11, legend.outside=FALSE, 
                         legend.position= c(0.01,0.1),legend.width = 0.9)+
  tm_add_legend('line', lty = "solid", col = "black", lwd = 1.5, labels = c("Limite de départements"))
  
  ```
  
 ##### Résultat: 
  
  <img src= https://github.com/CarolinaCABL/Cartographie/blob/main/eff_region.PNG />
  
 ###### Rhône
  
 ##### Résultat: 
   
  <img src= https://github.com/CarolinaCABL/Cartographie/blob/main/eff_rhone.PNG />
