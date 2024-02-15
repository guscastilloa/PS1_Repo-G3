
ied <- c("Colegio Aguas \n Claras (CED)",
         "Colegio \n Jaime Garzón (IED)",
         "Colegio Monte \n Verde (IED)","Colegio El \n Destino (IED)",
         "Colegio El \n Manantial (CED)","Colegio \n El Salitre (IED)",
         "Colegio El \n Uval (IED)","Colegio El \n Verjon (IED)",
         "Colegio Gabriel \n García Marquez (IED)",
         "Colegio Juan de \n la Cruz Varela (IED)",
         "Colegio Mochuelo \n Alto (CED)","Colegio Nicolás \n Buenaventura (IED)",
         "Colegio Nuevo \n Horizonte (IED)","Colegio \n Chizacá (CED)",
         "Colegio \n El Curubital (CED)","Colegio El \n Hato (CED)",
         "Colegio José \n Celestino Mutis (IED)",
         "Colegio La \n Argentina (CED)",
         "Colegio La \n Mayoría (CED)",
         "Colegio La \n Unión (CED)",
         "Colegio Las \n Mercedes (CED)",
         "Colegio \n Los Andes (CED)",
         "Colegio \n Los Arrayanes (CED)",
         "Colegio \n Olarte (CED)","Colegio \n Pasquilla (IED)",
         "Colegio \n Quiba Alta (IED)","Colegio San \n Cayetano (IED)",
         "Colegio San Martín \n de Porres (IED)")

### L3
y22 <- c(66.6667,75,66.6667,91.6667,58.3333,41.6667,66.6667,100,91.6667,
         91.6667,58.3333,66.6667,75,50,66.6667,66.6667,100,58.3333,58.3333,
         50,50,50,66.6667,66.6667,83.3333,91.6667,83.3333,75)

y23 <- c(58.3333,83.3333,83.3333,100,66.6667,66.6667,91.6667,75,66.6667,
         91.6667,66.6667,75,91.6667,58.3333,66.6667,58.3333,100,66.6667,
         75,58.3333,58.3333,58.3333,66.6667,66.6667,91.6667,91.6667,83.3333,75)

db <- data.frame(ied, y22, y23)


ggplot(db, aes(x=y22, y=y23)) +
  geom_smooth(method = lm, se = FALSE, color="#B04A29", linetype = "dotted")+
  geom_point(color="#E77B06", size=4, alpha=0.6) +
  labs(x="2022", y="2023")+
  xlim(min(y22)-10, 100)+ylim(min(y23)-10, 100)+
  theme(axis.ticks =  element_blank(),
        axis.line.x = element_line(color = "#000000"),
        axis.line.y = element_line(color = "#000000"),
        axis.title.x = element_text(size=10, hjust = 1, family="sans", 
                                    face="bold"),
        axis.title.y = element_text(size=10, hjust = 1, family="sans",
                                    face="bold"),
        axis.text.x=element_text(hjust=0.001, color = "#000000", family="sans"),
        axis.text.y=element_text(hjust=0.001, color = "#000000", family="sans"),
        strip.background =element_blank(),
        panel.background = element_blank()
  ) 
