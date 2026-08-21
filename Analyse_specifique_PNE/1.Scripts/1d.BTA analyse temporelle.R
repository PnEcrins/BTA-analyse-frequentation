

### Charge les données
setwd("C:/Users/TERRA/Documents/Projects/BiodivTourAlps")
source("1.Scripts/0.BTA preparation donnees.R")

library(lme4)
library(chngpt)


##########################
### ANALYSES ANNUELLES ###
##########################

### Fonction d'analyse
BTA_Analyse_tempo <- function(df_mois, Type){
  
  N_annees <- df_mois[df_mois$Value>0,] %>% group_by(Nom_Compteur) %>% summarise(N=length(unique(Annee))) %>% as.data.frame
  df_moisSUB <- subset(df_mois, (Nom_Compteur %in% N_annees$Nom_Compteur[N_annees$N>=10]) & is.na(Value)==F)
  if(Type %in% c("Ecocompteurs", "Compteurs routiers")){df_moisSUB <- subset(df_moisSUB, Nb_jours>10 & Mois %in% c("06", "07", "08", "09") )}
  
  df_moisSUB$Annee <- as.numeric(df_moisSUB$Annee)
  df_moisSUB$Annee_num <- as.numeric(df_moisSUB$Annee)-2000
  
  ggplot(df_moisSUB)+
    geom_point(aes(x=Annee, y=Value))+
    facet_wrap(~Nom_Compteur, scale="free_y")
  
  if(Type %in% c("Ecocompteurs", "Compteurs routiers")){
    Mod_annuel <- glmer.nb(Value ~ Annee_num + Mois + Nb_jours + (1|Nom_Compteur), data=df_moisSUB)
  } else {
    Mod_annuel <- glmer.nb(Value ~ Annee_num + (1|Nom_Compteur), data=df_moisSUB)
  }
  
  
  # Test plot estimates
  estim <- as.data.frame(summary(Mod_annuel)$coefficients)
  estim$Variable <- rownames(estim) %>% replace(., .=="Annee_num", "Annee")
  estim$Variable2 <- revalue(sub("Mois", "", estim$Variable), c("01"="Jan", "02"="Fev", "03"="Mar", "04"="Avr", "05"="Mai", "06"="Jun", "07"="Jul", "08"="Aou", "09"="Sep", "10"="Oct", "11"="Nov", "12"="Dec"), warn_missing = F) %>% factor(., c("Jan", "Fev", "Mar", "Avr", "Mai", "Jun", "Jul", "Aou", "Sep", "Oct", "Nov", "Dec"))
  names(estim)[2]<-"SE"
  
  Lim_Annee <- max(abs(estim$Estimate[estim$Variable=="Annee"])+abs(1.96*estim$SE[estim$Variable=="Annee"]))*1.1
  Lim_Mois <- max(abs(estim$Estimate[grepl("Mois", estim$Variable)])+abs(1.96*estim$SE[grepl("Mois", estim$Variable)]))*1.2
  
  G_annuel <- ggplot(estim[estim$Variable=="Annee",])+
    geom_pointrange(aes(x=Variable, y=Estimate, ymin=Estimate-1.96*SE, ymax=Estimate+1.96*SE))+
    xlab("")+ylab("Coefficient")+
    ylim(-Lim_Annee, Lim_Annee)+
    geom_hline(yintercept=0)+
    theme_minimal()
  
  if(Type %in% c("Ecocompteurs", "Compteurs routiers")){
  G_saison <- ggplot(estim[grepl("Mois", estim$Variable),])+
    geom_pointrange(aes(x=Variable2, y=Estimate, ymin=Estimate-1.96*SE, ymax=Estimate+1.96*SE))+
    xlab("")+ylab("Coefficient")+
    ylim(-Lim_Mois, Lim_Mois)+
    geom_hline(yintercept=0)+
    theme_minimal()
  } else {G_saison<-ggplot()+theme_void()}
  
  G_return <- grid.arrange(G_annuel, G_saison, 
                           nrow=2, 
                           top=paste0(Type, "\n", min(df_moisSUB$Annee, na.rm=T), " - ", max(df_moisSUB$Annee, na.rm=T))
  )
  
  return(list(Mod_annuel=Mod_annuel, G_return=G_return))
  
}

Res_rout <- BTA_Analyse_tempo(rout_mois %>% mutate(Nom_Compteur=Nom_uniq), "Compteurs routiers")
Res_eco <- BTA_Analyse_tempo(eco_mois %>% mutate(Nom_Compteur=Nom), "Ecocompteurs")
Res_refuges <- BTA_Analyse_tempo(refuges_nuits %>% mutate(Nom_Compteur=Nom_spatial), "Refuges")
Res_mdp <- BTA_Analyse_tempo(visites_mdp %>% mutate(Nom_Compteur=Nom_spatial) %>% subset(., Value>0), "Maisons de parc")


G_tendances <- grid.arrange(
  Res_refuges$G_return, Res_mdp$G_return, Res_rout$G_return, Res_eco$G_return, 
  nrow=1
)
cowplot::save_plot("2.Outputs/Plots/1d_annuel.png", G_tendances, base_height=5, base_width=10)





### Avec seuil (exploratoire)
# df_moisSUB$Residus <- residuals(Mod_annuel)
# 
# Mod_covid <-chngptm(formula.1=Residus ~ 1,
#             formula.2= ~Annee_num,
#             df_moisSUB, type="hinge", family="gaussian")
# 
# summary(Mod_covid)
# 
# 
# Mod_covid_test <-chngpt.test(formula.null=Residus ~ 1,
#                         formula.chngpt=~ Annee_num,
#                         data=df_moisSUB,
#                         chngpts=20, # Test for 2020
#                         type="segmented", 
#                         family="gaussian")
# 
# Mod_covid_test






###########################
### Analyse saisonniere ###
###########################

inat_raw_data$Date <- as.Date(inat_raw_data$datetime)
inat_raw_data$JDay <- format(inat_raw_data$Date, "%j") %>% as.numeric(.)
hist(inat_raw_data$JDay)


inat_sum <- inat_raw_data %>%
  group_by(Date) %>%
  summarise(N_data=n(), N_observers=length(unique(user_login))) %>%
  mutate(JDay=format(.$Date, "%j"), Annee=format(.$Date, "%Y")) %>%
  subset(., Annee>2016)

## TRANSFORM TO TS: https://stackoverflow.com/questions/52470591/creating-a-ts-time-series-with-missing-values-from-a-data-frame
library(zoo)
inat_ts <- data.frame(date=inat_sum$Date, value=inat_sum$N_observers) %>% read.zoo(FUN = as.Date) %>% as.ts %>% replace(., is.na(.), 0) %>% ts(., freq=365.25) 
plot(inat_ts)


# DECOMPOSE ONCE I HAVE A TS: https://rpubs.com/davoodastaraky/TSA1
inat_decomp <- decompose(inat_ts)
G_decompose <- plot(inat_decomp)
summary(inat_decomp$trend)

png("2.Outputs/Plots/1d_decompose.png", width = 25, height = 19, units = "cm", res = 300)
plot(inat_decomp)
dev.off()

season <- data.frame(date=1:365, Freq=inat_decomp$seasonal[1:365])
season$Cat <- cut(season$Freq, quantile(season$Freq, probs=c(0, 0.5, 0.95, 1)), include.lowest=T, labels=c("0-50%", "50-95%", "95-100%"))


# Peak 95-100%
Peak_ext <- c(min(season$date[season$Cat=="95-100%"]), max(season$date[season$Cat=="95-100%"]))
Peak_form <- (Peak_ext-1) %>% as.Date(.) %>% format("%d/%m")

# Below 50%
Low_ext <- c(max(season$date[season$Cat=="0-50%" & season$date<200]), min(season$date[season$Cat=="0-50%" & season$date>200]))
Low_form <- (Low_ext-1) %>% as.Date(.) %>% format("%d/%m")

# plot
G_season <- ggplot(season)+
  geom_point(aes(date, Freq, col=Cat))+
  geom_vline(xintercept=Peak_ext, col="darkred", linewidth=1.2, linetype="dashed")+
  geom_vline(xintercept=Low_ext, col="lightblue", linewidth=1.2, linetype="dashed")+
  scale_colour_manual(values=c("lightblue", "orange", "darkred"), name="Quantile")+
  xlab("Date (jour)")+ylab("Nombre d'observateurs")

print(paste0("Pic : ", Peak_form[1], "-", Peak_form[2], "   /   ", "Basse saison : ", Low_form[2], "-", Low_form[1]))

cowplot::save_plot("2.Outputs/Plots/1d_saison.png", G_season, base_height=6, base_width=9)





