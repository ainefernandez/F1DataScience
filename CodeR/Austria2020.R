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
        plot.caption = element_text(size = 11, hjust=0.5),
        plot.subtitle=element_text(size=15, hjust=0.5))+
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







stints<-read.csv("StintsUK2022.csv")
stints$Driver<-factor(stints$Driver,levels=c("ALB","ZHO","RUS","BOT","GAS",
                                             "OCO","TSU","RIC","LAT","STR",
                                             "MAG","VET","MSC","VER","NOR",
                                             "ALO","LEC","HAM","PER","SAI"))

stints$Stint_Compound <- paste(stints$Stint, stints$Compound)
stints$Stint_Compound<-factor(stints$Stint_Compound,levels=c("5 SOFT","4 HARD","4 SOFT","4 MEDIUM","3 SOFT","3 HARD","3 MEDIUM","2 SOFT","2 MEDIUM","2 HARD",
                                                             "1 SOFT","1 MEDIUM","1 HARD"))
stintsUK <- ggplot(stints, aes(Driver, StintLength, fill = Stint_Compound)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 52), breaks = seq(0, 52, by = 2)) +
  scale_fill_manual(values = c("1 SOFT" = "#FF3333", "2 SOFT" = "#FF3333", "3 SOFT" = "#FF3333", "4 SOFT" = "#FF3333","5 SOFT"="#FF3333",
                               "1 MEDIUM" = "#ffe541", "2 MEDIUM" = "#ffe541","3 MEDIUM" = "#ffe541", "4 MEDIUM" = "#ffe541",
                               "1 HARD" = "#d2d2d2", "2 HARD" = "#d2d2d2", "3 HARD" = "#d2d2d2", "4 HARD" = "#d2d2d2")) +
  labs(title = "Tyre strategies", subtitle = "2022 British GP", caption = "@f1.datascience", y = "Lap") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -20), size = 12),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  theme(legend.position = "none") 

stintsUK


VERQ<-read.csv("telVERQUK2022.csv")
SAIQ<-read.csv("telSAIQUK2022.csv")
LECQ<-read.csv("telLECQUK2022.csv")
SAIQ$faster <- NA

for (i in 1:nrow(SAIQ)) {
  if (!is.na(VERQ$Speed[i]) & !is.na(SAIQ$Speed[i]) & !is.na(LECQ$Speed[i])) {
    if (SAIQ$Speed[i] > VERQ$Speed[i] & SAIQ$Speed[i] > LECQ$Speed[i]) {
      SAIQ$faster[i] <- "Sainz is faster"
    } else if (VERQ$Speed[i] > SAIQ$Speed[i] & VERQ$Speed[i] > LECQ$Speed[i]) {
      SAIQ$faster[i] <- "Verstappen is faster"
    } else if (LECQ$Speed[i] > VERQ$Speed[i] & LECQ$Speed[i] >SAIQ$Speed[i]) {
      SAIQ$faster[i] <- "Leclerc is faster"
    } else {
      SAIQ$faster[i] <- "Sainz is faster"
    }
  } else {
    SAIQ$faster[i] <- "Sainz is faster"
  }
}
LapDataSAIUK2022<- data.frame(X = SAIQ$X, Y = SAIQ$Y, Speed=SAIQ$Speed, Faster=SAIQ$faster)
pSAIUK <- ggplot(LapDataSAIUK2022, aes(x = Y, y = X)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(Y), y = lag(X), xend = Y, yend = X, color = Faster, group = Faster),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("Verstappen is faster" = "#3671C6", "Sainz is faster" = "#F91536", "Leclerc is faster" = "#00A551"))  +
  coord_equal() + coord_flip()+
  theme_classic() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13)) +
  labs(title = "SAI, VER and LEC fastest lap comparison",
       subtitle = "2022 British GP",
       caption = "@f1.datascience")
pSAIUK

UK2022<-read.csv("PositionsUK2022.csv")
anim2 <- ggplot(UK2022) +
  geom_line(size = 1, aes(x = Lap, y = Position, col = Driver, group=Driver)) +
  geom_point(size = 3, aes(x = Lap, y = Position, col = Driver)) +
  scale_color_manual(values = c("PER" = "#3671C6", "HAM" = "#6CD3BF",
                                 "SAI" = "#F91536", "LEC"="#00A551"))+
  
  scale_y_reverse(breaks = seq(1, 4, by = 1))+
  scale_x_continuous(breaks=seq(42,52, by = 1))+
  theme_bw() +
  labs(x = "Lap", y = "Position") +
  transition_reveal(Lap) +
  theme(plot.title=element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust=0.5),
        plot.subtitle=element_text(size=15, hjust=0.5))+
  labs(title = "2022 British GP Race",subtitle= "Laps 42-52", caption = "@f1.datascience")
anim2


UK<-read.csv("racepaceUK2022.csv")
UK<-filter(UK,UK$LapNumber>=43)
UKSAI<-ggplot(UK, aes(x=LapNumber, y=LapTimeSeconds, col=Driver)) +
  geom_line(size=1) +
  geom_point(size=3)+
  scale_color_manual(values=c("NOR" = "#F58020", "HAM" = "#6CD3BF","PER" = "#3671C6",
                              "SAI" = "#F91536", "LEC"="#00A551", "ALO"="#FD4BC7"))+
  scale_y_continuous(limits=c(90.4,93.2),breaks=seq(90.4, 93.2, by = 0.2))+
  scale_x_continuous(breaks=seq(43,52, by = 1))+
  theme_bw()+
  labs(x = "Lap", y = "Lap Time (s)",title="SAI,PER,HAM,LEC,ALO and NOR laptimes comparison",subtitle ="2022 British GP (43-52)",caption = "@f1.datascience" )+
  theme(panel.background=element_rect(fill="white", color="white"),
        plot.background=element_rect(fill="white", color="white"),
        plot.title=element_text(size=18, hjust=0.5),
        plot.subtitle=element_text(size=15, hjust=0.5),
        plot.caption=element_text(size=11, hjust=0.5))

UKSAI
