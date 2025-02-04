library(ggplot2)
library(dplyr)
russia2021<-read.csv("/Users/ainefernandez/documents/f1datascience/Russia2021.csv")




L45<-filter(russia2021,Lap==45)
L46<-filter(russia2021,Lap==46)
L47<-filter(russia2021,Lap==47)
L48<-filter(russia2021,Lap==48)
L49<-filter(russia2021,Lap==49)
L50<-filter(russia2021,Lap==50)
L51<-filter(russia2021,Lap==51)
L52<-filter(russia2021,Lap==52)

p<-ggplot(L52, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Fastest_compound),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("SLICK" = "gray", "INTERMEDIATE" = "#4E841A"))+
  coord_flip()+coord_equal()+
  theme_classic()+theme(panel.background = element_rect(fill = "white", color = "white"),
                        plot.background = element_rect(fill = "white", color = "white"),
                        axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.title = element_blank(),
                        plot.title=element_text(size = 18, hjust = 0.5),
                        plot.subtitle=element_text(size = 15, hjust = 0.5),
                        plot.caption = element_text(size = 13, hjust=0.5),
                        legend.text=element_text(size=13),
                        legend.title=element_text(size=13))+
  
  labs(title = "Slicks vs Intermediates",subtitle="2021 Russia GP",caption = "@f1.datascience")+theme(legend.position = "none")
p




susuka2022<-read.csv("/Users/ainefernandez/documents/f1datascience/Susuka2022.csv")
L3<-filter(susuka2022,susuka2022$Lap==3)
L4<-filter(susuka2022,susuka2022$Lap==4)
L5<-filter(susuka2022,susuka2022$Lap==5)
L6<-filter(susuka2022,susuka2022$Lap==6)
L7<-filter(susuka2022,susuka2022$Lap==7)
L8<-filter(susuka2022,susuka2022$Lap==8)
L9<-filter(susuka2022,susuka2022$Lap==9)
L10<-filter(susuka2022,susuka2022$Lap==10)
L11<-filter(susuka2022,susuka2022$Lap==11)
L15<-filter(susuka2022,susuka2022$Lap==15)
L22<-filter(susuka2022,susuka2022$Lap==22)
L28<-filter(susuka2022,susuka2022$Lap==28)



p2<-ggplot(L22, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Fastest_compound),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("WET" = "steelblue", "INTERMEDIATE" = "#4E841A"))+
  coord_flip()+coord_equal()+
  theme_classic()+theme(panel.background = element_rect(fill = "white", color = "white"),
                        plot.background = element_rect(fill = "white", color = "white"),
                        axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.title = element_blank(),
                        plot.title=element_text(size = 18, hjust = 0.5),
                        plot.subtitle=element_text(size = 15, hjust = 0.5),
                        plot.caption = element_text(size = 13, hjust=0.5),
                        legend.text=element_text(size=13),
                        legend.title=element_text(size=13))+
  
  labs(title = "Wets vs Intermediates",subtitle="2022 Japanese GP",caption = "@f1.datascience")+theme(legend.position = "none")
p2

