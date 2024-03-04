##############################################################################-
# DATE:
#   2024/feb/10
# AUTHOR:
#  Jaime Buitrago
# DESCRIPTION:
# Scatter plots graphs for continuos all variables
##############################################################################-

# Prepare workspace

rm (list=ls())
source("scripts/00_packages.R")
gc()

############################################################################-
# 1. Database preparation ----
############################################################################-
stats <- arrow::read_parquet("stores/db.parquet")

stats <- stats %>% mutate(woman=1-sex,
                          woman=factor(woman, levels=c(0,1),
                                       labels=c('Hombre','Mujer'))
                    )

stats$exp <- ifelse(stats$exp<0,0,stats$exp)

stats <- data.frame(stats%>%
                      select(c(y_ingLab_m_ha, exp, age, esc, 
                               woman, sector, p6430)))

stats <- stats%>%mutate(exp=ifelse(is.na(exp), 22.011,
                                   exp))

stats <- stats%>%mutate(esc=ifelse(is.na(esc), 11.430,
                                   esc))

stats[,1] <- log(stats[,1])

stats <- stats%>%group_by(exp)%>%
  mutate(y1=mean(y_ingLab_m_ha, na.rm = TRUE))%>%ungroup()%>%
  group_by(age)%>%
  mutate(y2=mean(y_ingLab_m_ha, na.rm = TRUE))%>%ungroup()%>%
  group_by(esc)%>%
  mutate(y3=mean(y_ingLab_m_ha, na.rm = TRUE))%>%ungroup()%>%
  group_by(woman)%>%
  mutate(y4=mean(y_ingLab_m_ha, na.rm = TRUE))%>%ungroup()
  
############################################################################-
# 1. Database preparation ----
############################################################################-

g1 <- ggplot(stats, aes(x=exp))+
  geom_point(aes(y=y_ingLab_m_ha),alpha=0.2)+
  geom_point(aes(y=y1), colour="blue", size=2, alpha=0.7)+
  facet_grid(~woman)+
  labs(title="Experiencia laboral", y="Ingreso (ln)")+
  theme_bw()

g2 <- ggplot(stats, aes(x=age))+
  geom_point(aes(y=y_ingLab_m_ha),alpha=0.2)+
  geom_point(aes(y=y2), colour="blue", size=2, alpha=0.7)+
  facet_grid(~woman)+
  labs(title="Edad")+
  theme_bw()

g3 <- ggplot(stats, aes(x=esc))+
  geom_point(aes(y=y_ingLab_m_ha),alpha=0.2)+
  geom_point(aes(y=y3), colour="blue", size=2, alpha=0.7)+
  facet_grid(~woman)+
  labs(title="Años de escolaridad", y="Ingreso (ln)")+
  theme_bw()

g4 <- ggplot(stats, aes(x=woman))+
  geom_point(aes(y=y_ingLab_m_ha),alpha=0.2)+
  geom_point(aes(y=y4), colour="blue", size=2, alpha=0.7)+
  labs(title="Sexo")+
  theme_bw()

ggpubr::ggarrange(g1+rremove("xlab"), 
                  g2+rremove("xlab")+rremove("ylab"), 
                  g3+rremove("xlab"), 
                  g4+rremove("xlab")+rremove("ylab"), 
                  ncol = 2, nrow =2, align = "hv", vjust=5,
                  font.label = list(size = 7, face = "italic"))
ggsave("views/g2_1.jpg", scale=1, width = 7, height = 5 , units = 'in', dpi=600)




