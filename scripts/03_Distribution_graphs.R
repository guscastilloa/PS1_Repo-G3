##############################################################################-
# DATE:
#   2024/feb/10
# AUTHOR:
#  Jaime Buitrago
# DESCRIPTION:
#   Dsitribution graphs for continuos variables
##############################################################################-

# Prepare workspace

rm (list=ls())
source("scripts/00_packages.R")
gc()

############################################################################-
# 1. Database preparation ----
############################################################################-
stats <- arrow::read_parquet("stores/db.parquet")


stats$exp <- ifelse(stats$exp<0,0,stats$exp)

stats <- data.frame(stats%>%
                      select(c(y_ingLab_m_ha, exp, age, esc)))


stats <- stats%>%mutate(y_ingLab_m_ha=ifelse(is.na(y_ingLab_m_ha), 0.001,
                                             y_ingLab_m_ha))

stats <- stats%>%mutate(exp=ifelse(is.na(exp), 22.011,
                                             exp))

stats <- stats%>%mutate(esc=ifelse(is.na(esc), 11.430,
                                   esc))

############################################################################-
# 2. Graphs ----
############################################################################-


g1 <- ggplot(stats%>%filter(!is.na(y_ingLab_m_ha)), aes(x=y_ingLab_m_ha)) +
  geom_density()+
  geom_vline(aes(xintercept=mean(y_ingLab_m_ha)),
              color="blue", linetype="dashed", size=1)+
  labs( title = "Ingreso laboral (miles)")+
  theme_apa()

g2 <- ggplot(stats%>%filter(!is.na(exp)), aes(x=exp)) +
  geom_density()+
  geom_vline(aes(xintercept=mean(exp)),
             color="blue", linetype="dashed", size=1)+
  labs( title = "Experiencia laboral")+
  theme_apa()

g3 <- ggplot(stats, aes(x=age)) +
  geom_density()+
  geom_vline(aes(xintercept=mean(age)),
             color="blue", linetype="dashed", size=1)+
  labs( title = "Edad")+
  theme_apa()

g4 <- ggplot(stats%>%filter(!is.na(esc)), aes(x=esc)) +
  geom_density()+
  geom_vline(aes(xintercept=mean(esc)),
             color="blue", linetype="dashed", size=1)+
  labs( title = "Años de escolaridad")+
  theme_apa()

ggpubr::ggarrange(g1+rremove("xlab")+rremove("ylab"), 
                  g2+rremove("xlab")+rremove("ylab"), 
                  g3+rremove("xlab")+rremove("ylab"), 
                  g4+rremove("xlab")+rremove("ylab"), 
                  ncol = 2, nrow = 2, align = "hv", vjust=5,
                  font.label = list(size = 7, face = "italic"))

ggsave("views/g1.jpg", scale=1, width = 7, height = 5 , units = 'in', dpi=600)
