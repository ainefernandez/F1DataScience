library(ggplot2)
library(gganimate)
setwd("/Users/ainefernandez/documents/F1DataScience")
stints<-read.csv("StintsMonaco2021.csv")
stints$Driver<-factor(stints$Driver,levels=c("LEC","BOT","MSC","MAZ","TSU",
                                                           "LAT","RUS","ALO","RIC","RAI",
                                                           "GIO","OCO","STR","HAM","GAS",
                                                           "VET","PER","NOR","SAI","VER"))

stints$sc<-interaction(stints$Stint,stints$Compound)
stints$sc<-factor(stints$sc,levels=c("3.SOFT","2.SOFT","2.MEDIUM","2.HARD","1.SOFT","1.MEDIUM","1.HARD"))
ggplot(stints, aes(Driver,StintLength,fill=sc))+
  geom_bar(stat="identity")+
  coord_flip()+
  scale_fill_manual(values=c("1.SOFT"="#FF3333","2.SOFT"="#FF3333","3.SOFT"="#FF3333","1.MEDIUM"="#ffe541",
                             "2.MEDIUM"="#ffe541","1.HARD"="#d2d2d2","2.HARD"="#d2d2d2"),name="Compound",labels=c("1.SOFT"="Soft","2.SOFT"="Soft",
                                                                                                                  "3.SOFT"="Soft","1.MEDIUM"="Medium",                                                                                                                "2.MEDIUM"="Medium","1.HARD"="Hard","2.HARD"="Hard"))+
  scale_y_continuous(limits=c(0,80),breaks = seq(0, 80, by = 5))+
  labs(title="Tyre strategies 2021 Monaco GP",caption = "@f1.datascience",y="Lap")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title=element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust=0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -20),size=12),
        axis.ticks.y =element_blank(),
        axis.title.y=element_blank(),
        legend.text = element_text(size=12), 
        legend.title = element_text(size=12))+theme(legend.position = "none")


pits<-read.csv("pitstopmonaco.csv")
pp<-ggplot(pits) +
  geom_line(size = 1,aes(x = Lap, y = Position,col=Driver)) +
  geom_point(size=3,aes(x=Lap,y=Position,col=Driver))+
  scale_color_manual(values=c("HAM"="#6CD3BF","GAS"="maroon","PER"="#3671C6","VET"="#358C75"))+
  scale_y_reverse(breaks=seq(1, 10, by = 1))+
  scale_x_continuous(breaks=seq(28, 36, by = 1))+
  theme_bw()+
  theme(plot.caption = element_text(size = 11, hjust=0.5))+
  labs(x = "Lap", y = "Position",caption="@f1.datascience")
pp
anim <- pp + transition_reveal(Lap) +
  labs(title = "GAS, HAM, PER and VET positions before and after pitstops")
anim



laptimes<-read.csv("laptimesmonaco.csv")
ggplot(laptimes) +
  geom_line(size = 1,aes(x = Lap, y = Laptime,col=Driver)) +
  geom_point(size=3,aes(x=Lap,y=Laptime,col=Driver))+
  scale_x_continuous(breaks=seq(29, 36, by = 1))+
  scale_y_continuous(breaks=seq(70, 94, by = 2))+
  scale_color_manual(values=c("HAM"="#6CD3BF","GAS"="maroon","PER"="#3671C6","VET"="#358C75"))+
  theme_bw()+
  theme(plot.caption = element_text(size = 11, hjust=0.5),
        plot.title=element_text(size = 18, hjust = 0.5),
        axis.title = element_text(size = 13),
        plot.subtitle=element_text(size = 15, hjust = 0.5))+
  labs(x = "Lap", y = "Laptime (s)",title="GAS, HAM, PER and VET laptime comparison",subtitle="2021 Monaco GP (29-36)",caption="@f1.datascience")
  