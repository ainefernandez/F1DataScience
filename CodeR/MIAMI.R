library(ggplot2)
library(dplyr)
setwd("/Users/ainefernandez/documents/F1DataScience")
gapMiamiQ<-read.csv("GapsMiami.csv")
names(gapMiamiQ)<-c("Driver","Team","Laptime","Gap")
pQualy<-ggplot(gapMiamiQ, aes(x=Gap,y=reorder(Driver,-Gap),fill=Team))+ 
  geom_vline(xintercept = c(0.5,1, 1.5,2.0), linetype = "dashed", color = "grey")+
  
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
                               "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo"="#C92D4B"))+
  scale_x_continuous(breaks = seq(0, 2, by = 0.1))+
  scale_y_discrete(expand = c(0,0))+
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Gap to best in Qualy")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title=element_text(size = 18, hjust = 0.5),
        plot.subtitle=element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust=0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -30)),
        axis.ticks.y =element_blank())+
  labs(caption = "@f1.datascience",subtitle = "2023 Miami GP")+theme(legend.position = "none")
pQualy

SAIQ<-read.csv("telSAIQMIAMI.csv")
PERQ<-read.csv("telPERQMIAMI.csv")
ALOQ<-read.csv("telALOQMIAMI.csv")
PERQ$faster <- NA

for (i in 1:nrow(PERQ)) {
  if (!is.na(PERQ$Speed[i]) & !is.na(ALOQ$Speed[i]) & !is.na(SAIQ$Speed[i])) {
    if (PERQ$Speed[i] > ALOQ$Speed[i] & PERQ$Speed[i] > SAIQ$Speed[i]) {
      PERQ$faster[i] <- "Pérez is faster"
    } else if (ALOQ$Speed[i] > PERQ$Speed[i] & ALOQ$Speed[i] > SAIQ$Speed[i]) {
      PERQ$faster[i] <- "Alonso is faster"
    } else if (SAIQ$Speed[i] > PERQ$Speed[i] & SAIQ$Speed[i] > ALOQ$Speed[i]) {
      PERQ$faster[i] <- "Sainz is faster"
    } else {
      PERQ$faster[i] <- "Pérez is faster"
    }
  } else {
    PERQ$faster[i] <-"Pérez is faster"
  }
}
LapDataPERMiami<- data.frame(X = PERQ$X, Y = PERQ$Y, Speed=PERQ$Speed, Faster=PERQ$faster)
pPERM<-ggplot(LapDataPERMiami, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Faster,group=Faster),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("Pérez is faster" = "#3671C6", "Sainz is faster" = "#F91536","Alonso is faster"="#358C75"))+
  coord_flip()+coord_equal()+
  theme_classic()+theme(panel.background = element_rect(fill = "white", color = "white"),
                        plot.background = element_rect(fill = "white", color = "white"),
                        axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.title = element_blank(),
                        plot.title=element_text(size = 18, hjust = 0.5),
                        plot.subtitle=element_text(size = 15, hjust = 0.5),
                        plot.caption = element_text(size = 11, hjust=0.5),
                        legend.text=element_text(size=13),
                        legend.title=element_text(size=13))+
  
  labs(title="PER, ALO and SAI fastest lap comparison",subtitle="2023 Miami GP",caption = "@f1.datascience")
pPERM

miami<-read.csv("racepacemiamicheco.csv")
miamihard<-read.csv("HardMIAMI.csv")
miamihard$compound<-"Hard"
miamimedium$compound<-"Medium"
miamicompounds$compound <- factor(miamicompounds$compound)
miamicompounds<-rbind(miamimedium,miamihard)
class(miami$LapNumber)
ggplot(miami, aes(x=LapNumber, y=LapTimeSeconds, col=Driver)) +
  geom_line(size=1) +
  geom_point(size=3)+
  scale_color_manual(values=c("VER"="#3671C6","PER"="red"))+
  scale_y_continuous(limits=c(90,110),breaks=seq(90, 110, by = 5))+
  scale_x_continuous(breaks=seq(0, 57, by = 2))+
  theme_bw()+
  labs(x = "Lap", y = "Lap Time (s)",title="VER and PER laptimes comparison",subtitle ="2023 Miami GP",caption = "@f1.datascience" )+
  theme(panel.background=element_rect(fill="white", color="white"),
        plot.background=element_rect(fill="white", color="white"),
        plot.title=element_text(size=18, hjust=0.5),
        plot.subtitle=element_text(size=15, hjust=0.5),
        plot.caption=element_text(size=11, hjust=0.5))

View(miami)
pitstopMiami<-read.csv("PitstopMiami.csv")

# create the bar plot
# create a data frame with the data

# create the bar plot
ggplot(pitstopMiami, aes(x = Team, y = PitstopTime, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  
  labs(title = "Pitlane times 2022 vs 2023",subtitle = "2023 Miami GP",caption = "@f1.datascience",
       x = "Team", y = "Pitlane Time (seconds)", fill = "Year")+ theme_bw()+
  scale_y_continuous(expand=c(0,0),limits=c(0,40),breaks=seq(0, 40, by = 5))+
  theme(panel.background=element_rect(fill="white", color="white"),
        plot.background=element_rect(fill="white", color="white"),
        plot.title=element_text(size=18, hjust=0.5),
        plot.subtitle=element_text(size=15, hjust=0.5),
        plot.caption=element_text(size=11, hjust=0.5))


resultsMiami<-read.csv("ResultsMIAMIR2023.csv")
resultsMiami<-resultsMiami[c("Abbreviation","GridPosition","Position")]
resultsMiami$GridPos[resultsMiami$Driver=="VER"]<-9
names(resultsMiami)<-c("Driver","GridPos","RacePos")
left_label <- paste(resultsMiami$Driver, round(resultsMiami$GridPos),sep=", ")
right_label <- paste(resultsMiami$Driver, round(resultsMiami$RacePos),sep=", ")
p <- ggplot(resultsMiami) + 
  geom_segment(aes(x=1, xend=2, y=GridPos, yend=RacePos, col=Driver), size=1, show.legend=F) +
  geom_vline(xintercept=1, linetype="dashed", size=.1) +
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(values = c("VER"="#3671C6","PER"="#3671C6","ALO"="#358C75","SAI"="#F91536","HAM"="#6CD3BF","STR"="#358C75","RUS"="#6CD3BF","BOT"="#C92D4B","GAS"="#2293D1","ALB"="#37BEDD",
                                "TSU"="#5E8FAA","SAR"="#37BEDD","MAG"="#B6BABD","DEV"="#5E8FAA","HUL"="#B6BABD","ZHO"="#C92D4B","NOR"="#F58020","OCO"="#2293D1","LEC"="#F91536","PIA"="#F58020")) +
  scale_y_reverse() + 
  geom_text(aes(y = GridPos, x = 1, label = left_label), hjust = 1, size = 4) +
  geom_text(aes(y = RacePos, x = 2, label = right_label), hjust = 0, size = 4) +
  geom_text(label = "Grid Position", x = 1, y = max(resultsAUS$GridPos, resultsAUS$RacePos) * 1.1, hjust = 1.2, size = 5) +
  geom_text(label = "Race Position", x = 2, y = max(resultsAUS$GridPos, resultsAUS$RacePos) * 1.1, hjust = -0.1, size = 5) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y=element_blank(),
        plot.title=element_text(size = 18, hjust = 0.5),
        plot.subtitle=element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust=0.5),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13))+
  labs(title="Grid and race position",subtitle="2023 Miami GP",caption = "@f1.datascience")
p
