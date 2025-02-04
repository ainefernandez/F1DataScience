library(ggplot2)
library(dplyr)
library(gganimate)
setwd("/Users/ainefernandez/documents/F1DataScience")
drivercolors <- c("VER" = "#3671C6","PER" = "#3671C6","ALO" = "#358C75","SAI" = "#F91536","HAM" = "#6CD3BF",
                  "STR" = "#358C75","RUS" = "#6CD3BF","BOT" = "#04C404","GAS" = "#FF66C4","ALB" = "#37BEDD",
                  "TSU" = "#0335D3","SAR" = "#37BEDD","MAG" = "#B6BABD","DEV" = "#5E8FAA","HUL" = "#B6BABD",
                  "ZHO" = "#04C404", "NOR" = "#F58020","OCO" = "#FF66C4","LEC" = "#F91536","PIA" = "#F58020", "RIC"="#0335D3","LAW"="#5E8FAA","BEA"="#F91536","COL"="#37BEDD")
teamscolors<-c("Red Bull Racing"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
               "Alpine"="#FF66C4","Haas F1 Team"="#B6BABD","RB"="#0335D3","McLaren"="#F58020","Kick Sauber"="#04C404","AlphaTauri"="#5E8FAA","Alfa Romeo"="#C92D4B")


VERQ <- read.csv("VERQAustin.csv")
SAIQ <- read.csv("SAIQAustin.csv")
NORQ <- read.csv("NORQAustin.csv")
NORQ$faster <- NA

for (i in 1:nrow(NORQ)) {
  if (!is.na(VERQ$Speed[i]) & !is.na(NORQ$Speed[i]) & !is.na(SAIQ$Speed[i])) {
    if (SAIQ$Speed[i] > VERQ$Speed[i] & SAIQ$Speed[i] > NORQ$Speed[i]) {
      NORQ$faster[i] <- "Sainz is faster"
    } else if (VERQ$Speed[i] > SAIQ$Speed[i] & VERQ$Speed[i] > NORQ$Speed[i]) {
      NORQ$faster[i] <- "Verstappen is faster"
    } else if (NORQ$Speed[i] > SAIQ$Speed[i] & NORQ$Speed[i] > VERQ$Speed[i]) {
      NORQ$faster[i] <- "Norris is faster"
    } else {
      NORQ$faster[i] <- "Norris is faster"
    }
  } else {
    NORQ$faster[i] <- "Norris is faster"
  }
}


LapDataVERJAP<- data.frame(X = NORQ$X, Y = NORQ$Y, Speed=NORQ$Speed, Faster=NORQ$faster)
pSAIQ <- ggplot(LapDataVERJAP, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Faster, group = Faster),
               size = 3, lineend = "round") +
  scale_color_manual(
    values = c("Sainz is faster" = "#F91536", "Norris is faster" = "#F58020", "Verstappen is faster" = "#3671C6"),
    breaks = c("Norris is faster", "Verstappen is faster", "Sainz is faster")
  ) +
  coord_flip()+coord_equal()+
  theme_classic() + theme(
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 13)
  ) +
  labs(title = "NOR, VER, and SAI fastest lap comparison", subtitle = "2024 US GP")

pSAIQ


gapSingaporeQ<-read.csv("GapCanada.csv")
pQualyS<-ggplot(gapSingaporeQ, aes(x=Gap,y=reorder(Driver,-Gap),fill=Driver))+ 
  geom_vline(xintercept = seq(0, 2.6, by = 0.2), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, 2.6, by = .2))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(Gap,3)), hjust = -0.1, size = 4) +
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
  labs(caption = "@f1.datascience",subtitle = "2024 Canadian GP")+theme(legend.position = "none")
pQualyS




gapSingaporeQ<-read.csv("idealgapCanada.csv")
#gapSingaporeQ<-gapSingaporeQ[gapSingaporeQ$Driver!="ZHO",]
pQualyS<-ggplot(gapSingaporeQ, aes(x=Gap,y=reorder(Driver,-Gap),fill=Driver))+ 
  geom_vline(xintercept = seq(0, 2.6, by = 0.2), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, 2.6, by = .2))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(Gap,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Ideal Qualy Laps Gaps")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Canadian GP")+theme(legend.position = "none")
pQualyS

gapSingaporeQ<-read.csv("idealgapsTCanada.csv")
#gapSingaporeQ<-gapSingaporeQ[gapSingaporeQ$Driver!="ZHO",]
pQualyS<-ggplot(gapSingaporeQ, aes(x=Gap,y=reorder(Team,-Gap),fill=Team))+ 
  geom_vline(xintercept = seq(0, 1.8, by = 0.2), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=teamscolors)+
  scale_x_continuous(breaks = seq(0, 1.8, by = .2))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(Gap,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Ideal Qualy Laps Gaps (Teams)")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Canadian GP")+theme(legend.position = "none")
pQualyS


gapSingaporeQ<-read.csv("bestsectorCanada.csv")
#gapSingaporeQ<-gapSingaporeQ[gapSingaporeQ$Driver!="ZHO",]
pQualyS<-ggplot(gapSingaporeQ, aes(x=GapSector1,y=reorder(Driver,-GapSector1),fill=Driver))+ 
  geom_vline(xintercept = seq(0, 1, by = 0.1), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0,1, by = 0.1))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(GapSector1,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Gap to fastest Sector 1")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Canadian GP")+theme(legend.position = "none")
pQualyS


pQualyS<-ggplot(gapSingaporeQ, aes(x=GapSector2,y=reorder(Driver,-GapSector2),fill=Driver))+ 
  geom_vline(xintercept = seq(0, 1, by = .1), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, 1, by = .1))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(GapSector2,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Gap to fastest Sector 2")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Canadian GP")+theme(legend.position = "none")
pQualyS

pQualyS<-ggplot(gapSingaporeQ, aes(x=GapSector3,y=reorder(Driver,-GapSector3),fill=Driver))+ 
  geom_vline(xintercept = seq(0,.8, by = .05), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, .8, by = .05))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(GapSector3,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Gap To fastest Sector 3")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Canadian GP")+theme(legend.position = "none")
pQualyS


results<-read.csv("ResultsCanada.csv")
left_label <- paste(results$Abbreviation, round(results$GridPosition),sep=", ")
right_label <- paste(results$Abbreviation, round(results$Position),sep=", ")

p <- ggplot(results) + 
  geom_segment(aes(x=1, xend=2, y=GridPosition, yend=Position, col=Abbreviation), size=1, show.legend=F) +
  geom_vline(xintercept=1, linetype="dashed", size=.1) +
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(values = drivercolors) +
  scale_y_reverse() + 
  geom_text(aes(y = GridPosition, x = 1, label = left_label), hjust = 1, size = 3) +
  geom_text(aes(y =Position, x = 2, label = right_label), hjust = 0, size = 3) +
  geom_text(label = "Grid Position", x = 1, y = max(results$GridPosition, results$Position) * 1.1, hjust = 1.2, size = 5) +
  geom_text(label = "Race Position", x = 2, y = max(results$GridPosition, results$Position) * 1.1, hjust = -0.1, size = 5) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y=element_blank(),
        plot.title=element_text(size = 18, hjust = 0.5),
        plot.subtitle=element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust=0.5),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13))+
  labs(title="Grid and race position",subtitle="2024 Canadian GP",caption = "@f1.datascience")
p

stints<-read.csv("StintsCanada.csv")
stints$Stint_Compound <- paste(stints$Stint, stints$Compound)
stints$Stint_Compound<-factor(stints$Stint_Compound,levels=c("5 MEDIUM","5 HARD","6 SOFT","5 SOFT","4 HARD","4 MEDIUM","4 INTERMEDIATE","4 SOFT","3 SOFT","3 HARD","3 MEDIUM","3 INTERMEDIATE","2 SOFT","2 MEDIUM","2 HARD", "2 INTERMEDIATE",
                                                             "1 SOFT","1 MEDIUM","1 HARD","1 WET","1 INTERMEDIATE"))
stints$Driver<-factor(stints$Driver,levels=c("SAR","LEC","PER","ALB","SAI","ZHO","TSU",
                                             "BOT","MAG","HUL","OCO","GAS",
                                             "RIC","STR","ALO","PIA","HAM",
                                             "RUS","NOR","VER"))
stintsSpain <- ggplot(stints, aes(Driver, StintLength, fill = Stint_Compound)) +
  geom_bar(stat = "identity",color="black") +
  coord_flip() +
  scale_y_continuous(limits = c(0,70), breaks = seq(0, 70, by = 2)) +
  scale_fill_manual(values = c("6 SOFT"="#FF3333","5 SOFT"="#FF3333","1 SOFT" = "#FF3333", "2 SOFT" = "#FF3333", "3 SOFT" = "#FF3333", "4 SOFT" = "#FF3333", 
                               "1 MEDIUM" = "#ffe541", "2 MEDIUM" = "#ffe541",
                               "3 MEDIUM" = "#ffe541", "4 MEDIUM" = "#ffe541","5 MEDIUM" = "#ffe541",
                               "1 HARD" = "#d2d2d2", "2 HARD" = "#d2d2d2", "3 HARD" = "#d2d2d2", "4 HARD" = "#d2d2d2","5 HARD" = "#d2d2d2","1 INTERMEDIATE"="#66A441","2 INTERMEDIATE"="#66A441","3 INTERMEDIATE"="#66A441","4 INTERMEDIATE"="#66A441","1 WET"="#2B8CCB")) +
  labs(title = "Tyre strategies", subtitle = "2024 Canadian GP", caption = "@f1.datascience", y = "Lap") +
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

stintsSpain

gapSingaporeQ<-read.csv("GapRacePaceCanada.csv")
names(gapSingaporeQ)<-c("Team","Gap")
pQualyS<-ggplot(gapSingaporeQ, aes(x=Gap,y=reorder(Team,-Gap),fill=Team))+ 
  geom_vline(xintercept = seq(0, 5, by =.5), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=teamscolors)+
  scale_x_continuous(breaks = seq(0, 5, by =.5))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(Gap,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Team") +
  ggtitle("Race Pace")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Canadian GP")+theme(legend.position = "none")
pQualyS

racepace<-read.csv("RacePaceChinaDrivers.csv")
racepacep <- ggplot(racepace, aes(x = reorder(Driver, LapTime, FUN = mean), y = LapTime, fill = Driver)) +
  geom_boxplot() +
  labs(x = "Driver", y = "Lap Time") +
  ggtitle("Race pace comparison (Sorted by mean laptime)") +
  scale_y_continuous(limits = c(97.5, 108.5), breaks = seq(97.5, 108.5, by = 0.5)) +
  scale_fill_manual(values = drivercolors) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    axis.ticks.y = element_blank(),
    legend.position = "none") +
  labs(caption = "@f1.datascience", subtitle = "2024 Chinese GP")

racepacep


gapSingaporeQ<-read.csv("GapRacePaceCanadaWET.csv")
names(gapSingaporeQ)<-c("Team","Gap")
pQualyS<-ggplot(gapSingaporeQ, aes(x=Gap,y=reorder(Team,-Gap),fill=Team))+ 
  geom_vline(xintercept = seq(0, 2.2, by =.1), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=teamscolors)+
  scale_x_continuous(breaks = seq(0, 2.2, by =.1))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(Gap,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Team") +
  ggtitle("Race Pace in the wet")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Canada GP (Laps 1-45)")+theme(legend.position = "none")
pQualyS



