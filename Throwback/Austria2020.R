library(ggplot2)
library(dplyr)
library(gganimate)
setwd("/Users/ainefernandez/documents/F1DataScience")
austria2020<-read.csv("AUS2020.csv")
anim <- ggplot(austria2020) +
  geom_line(size = 1, aes(x = Lap, y = Position, col = Driver, group=Driver)) +
  geom_point(size = 3, aes(x = Lap, y = Position, col = Driver)) +
  scale_color_manual(values = c("SAI" = "#47C7FC","NOR" = "#F58020", "HAM" = "#6CD3BF",
                                "PER" = "#FD4BC7", "BOT" = "#565F64", "LEC" = "#F91536"))+
                                 
  scale_y_reverse(breaks = seq(1, 6, by = 1))+
  scale_x_continuous(breaks=seq(63,71, by = 1))+
  theme_bw() +
  labs(x = "Lap", y = "Position") +
  transition_reveal(Lap) +
  theme(plot.title=element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust=0.5))+
  labs(title = "2020 Austria GP Race",subtitle= "Laps 63-71", caption = "@f1.datascience")
anim


austria<-read.csv("HamNorLaptimesAus2020.csv")

austriaNOR<-ggplot(austria, aes(x=Lap, y=LapTime, col=Driver)) +
  geom_line(size=1) +
  geom_point(size=3)+
  scale_color_manual(values=c("NOR" = "#F58020", "HAM" = "#6CD3BF"))+
  scale_y_continuous(limits=c(67.4,69.2),breaks=seq(67.4, 69.2, by = 0.1))+
  scale_x_continuous(breaks=seq(67,71, by = 1))+
  theme_bw()+
  labs(x = "Lap", y = "Lap Time (s)",title="NOR and HAM laptimes comparison",subtitle ="2020 Austria GP (67-71)",caption = "@f1.datascience" )+
  theme(panel.background=element_rect(fill="white", color="white"),
        plot.background=element_rect(fill="white", color="white"),
        plot.title=element_text(size=18, hjust=0.5),
        plot.subtitle=element_text(size=15, hjust=0.5),
        plot.caption=element_text(size=11, hjust=0.5))

austriaNOR

sectors<-read.csv("Norsectors.csv")
austriaNOR2<-ggplot(sectors, aes(x=Sector, y=Time,col=Driver)) +
  geom_line(size=1) +
  geom_point(size=3)+
  scale_color_manual(values=c("NOR" = "#F58020", "HAM" = "#6CD3BF"))+
  scale_y_continuous(limits=c(16,32),breaks=seq(16, 32, by = 1))+
  scale_x_continuous(breaks=seq(1,3, by = 1))+
  theme_bw()+
  labs(x = "Sector", y = "Time (s)",title="NOR and HAM sector times",subtitle ="2020 Austria GP (71)",caption = "@f1.datascience" )+
  theme(panel.background=element_rect(fill="white", color="white"),
        plot.background=element_rect(fill="white", color="white"),
        plot.title=element_text(size=18, hjust=0.5),
        plot.subtitle=element_text(size=15, hjust=0.5),
        plot.caption=element_text(size=11, hjust=0.5),
        legend.position = "None")

austriaNOR2
