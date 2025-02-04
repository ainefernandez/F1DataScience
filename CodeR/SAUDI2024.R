library(ggplot2)
library(dplyr)
library(gganimate)
setwd("/Users/ainefernandez/documents/F1DataScience")
drivercolors <- c("VER" = "#3671C6","PER" = "#3671C6","ALO" = "#358C75","SAI" = "#F91536","HAM" = "#6CD3BF",
                  "STR" = "#358C75","RUS" = "#6CD3BF","BOT" = "#04C404","GAS" = "#FF66C4","ALB" = "#37BEDD",
                  "TSU" = "#0335D3","SAR" = "#37BEDD","MAG" = "#B6BABD","DEV" = "#5E8FAA","HUL" = "#B6BABD",
                  "ZHO" = "#04C404", "NOR" = "#F58020","OCO" = "#FF66C4","LEC" = "#F91536","PIA" = "#F58020", "RIC"="#0335D3","LAW"="#5E8FAA","BEA"="#F91536")
teamscolors<-c("Red Bull Racing"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
               "Alpine"="#FF66C4","Haas F1 Team"="#B6BABD","RB"="#0335D3","McLaren"="#F58020","Kick Sauber"="#04C404")


PERQ <- read.csv("PERQSaudi.csv")
VERQ <- read.csv("VERQSaudi.csv")
LECQ <- read.csv("LECQSaudi.csv")
VERQ$faster <- NA

for (i in 1:nrow(VERQ)) {
  if (!is.na(RUSQ$Speed[i]) & !is.na(LECQ$Speed[i]) & !is.na(VERQ$Speed[i])) {
    if (VERQ$Speed[i] > PERQ$Speed[i] & VERQ$Speed[i] > LECQ$Speed[i]) {
      VERQ$faster[i] <- "Verstappen is faster"
    } else if (LECQ$Speed[i] > VERQ$Speed[i] & LECQ$Speed[i] > PERQ$Speed[i]) {
      VERQ$faster[i] <- "Leclerc is faster"
    } else if (PERQ$Speed[i] > VERQ$Speed[i] & PERQ$Speed[i] > LECQ$Speed[i]) {
      VERQ$faster[i] <- "Pérez is faster"
    } else {
      VERQ$faster[i] <- "Verstappen is faster"
    }
  } else {
    VERQ$faster[i] <- "Verstappen is faster"
  }
}

LapDataVERJAP<- data.frame(X = VERQ$X, Y = VERQ$Y, Speed=VERQ$Speed, Faster=VERQ$faster)
pSAIQ <- ggplot(LapDataVERJAP, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Faster, group = Faster),
               size = 3, lineend = "round") +
  scale_color_manual(
    values = c("Verstappen is faster" = "#3671C6", "Leclerc is faster" = "#F91536", "Pérez is faster" = "gold"),
    breaks = c("Verstappen is faster", "Leclerc is faster", "Pérez is faster")
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
  labs(title = "VER, LEC, and PER fastest lap comparison", subtitle = "2024 Saudi Arabian GP", caption = "@f1.datascience")

pSAIQ


gapSingaporeQ<-read.csv("GapSaudi.csv")
pQualyS<-ggplot(gapSingaporeQ, aes(x=Gap,y=reorder(Driver,-Gap),fill=Driver))+ 
  geom_vline(xintercept = seq(0, 2.2, by = 0.2), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, 2.2, by = .2))+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Saudi Arabian GP")+theme(legend.position = "none")
pQualyS




gapSingaporeQ<-read.csv("idealgapSaudi.csv")
gapSingaporeQ<-gapSingaporeQ[gapSingaporeQ$Driver!="ZHO",]
pQualyS<-ggplot(gapSingaporeQ, aes(x=Gap,y=reorder(Driver,-Gap),fill=Driver))+ 
  geom_vline(xintercept = seq(0, 2.2, by = 0.2), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, 2.2, by = .2))+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Saudi Arabian GP")+theme(legend.position = "none")
pQualyS




gapSingaporeQ<-read.csv("bestsectorSaudi.csv")
gapSingaporeQ<-gapSingaporeQ[gapSingaporeQ$Driver!="ZHO",]
pQualyS<-ggplot(gapSingaporeQ, aes(x=GapSector1,y=reorder(Driver,-GapSector1),fill=Driver))+ 
  geom_vline(xintercept = seq(0, .9, by = 0.1), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, .9, by = 0.1))+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Saudi Arabian GP")+theme(legend.position = "none")
pQualyS


pQualyS<-ggplot(gapSingaporeQ, aes(x=GapSector2,y=reorder(Driver,-GapSector2),fill=Driver))+ 
  geom_vline(xintercept = seq(0, .6, by = .1), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, .6, by = 0.1))+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Saudi Arabian GP")+theme(legend.position = "none")
pQualyS

pQualyS<-ggplot(gapSingaporeQ, aes(x=GapSector3,y=reorder(Driver,-GapSector3),fill=Driver))+ 
  geom_vline(xintercept = seq(0, .7, by = .1), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, .7, by = 0.1))+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Saudi Arabian GP")+theme(legend.position = "none")
pQualyS


#Race

results<-read.csv("ResultsSaudi.csv")
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
  labs(title="Grid and race position",subtitle="2024 Saudi Arabian GP",caption = "@f1.datascience")
p

stints<-read.csv("StintsSaudi.csv")
stints$Stint_Compound <- paste(stints$Stint, stints$Compound)
stints$Stint_Compound<-factor(stints$Stint_Compound,levels=c("6 SOFT","5 SOFT","4 HARD","4 MEDIUM","4 SOFT","3 SOFT","3 HARD","3 MEDIUM","2 SOFT","2 MEDIUM","2 HARD",
                                                             "1 SOFT","1 MEDIUM","1 HARD"))
stints$Driver<-factor(stints$Driver,levels=c("GAS","STR","ZHO","BOT","RIC","SAR",
                                             "TSU","OCO","MAG","ALB","HUL",
                                             "HAM","NOR","BEA","RUS","ALO",
                                             "PIA","LEC","PER","VER"))
stintsSpain <- ggplot(stints, aes(Driver, StintLength, fill = Stint_Compound)) +
  geom_bar(stat = "identity",color="black") +
  coord_flip() +
  scale_y_continuous(limits = c(0,50), breaks = seq(0, 50, by = 2)) +
  scale_fill_manual(values = c("6 SOFT"="#FF3333","5 SOFT"="#FF3333","1 SOFT" = "#FF3333", "2 SOFT" = "#FF3333", "3 SOFT" = "#FF3333", "4 SOFT" = "#FF3333", "1 MEDIUM" = "#ffe541", "2 MEDIUM" = "#ffe541",
                               "3 MEDIUM" = "#ffe541", "4 MEDIUM" = "#ffe541",
                               "1 HARD" = "#d2d2d2", "2 HARD" = "#d2d2d2", "3 HARD" = "#d2d2d2", "4 HARD" = "#d2d2d2")) +
  labs(title = "Tyre strategies", subtitle = "2024 Saudi Arabian GP", caption = "@f1.datascience", y = "Lap") +
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

gapSingaporeQ<-read.csv("GapRacePaceSaudi.csv")
names(gapSingaporeQ)<-c("Team","Gap")
pQualyS<-ggplot(gapSingaporeQ, aes(x=Gap,y=reorder(Team,-Gap),fill=Team))+ 
  geom_vline(xintercept = seq(0, 2.4, by = 0.2), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=teamscolors)+
  scale_x_continuous(breaks = seq(0, 2.4, by = .2))+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Saudi Arabian GP")+theme(legend.position = "none")
pQualyS

singapur<-read.csv("POSSaudi2024Race.csv")
drivercol<-c("PIA" = "#F58021","RUS"="#6CD3BF","BEA"="#F91536","HAM"="black","NOR"="skyblue")
monlando <- ggplot(singapur, aes(x = LapNumber, y = Position, color = Driver)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_y_reverse(limits = c(12,1), breaks = seq(12,1, by = -1)) +
  scale_x_continuous(breaks = seq(1, 50, by = 1)) +
  scale_color_manual(values = drivercol) +
  theme_bw() +
  labs(
    x = "Lap",
    y = "Position",
    title = "PIA,RUS, BEA, NOR and HAM positions during the race",
    subtitle = "2024 Saudi Arabian GP",
    caption = "@f1.datascience"
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5)
  )
monlando

podiumsdrivers<-read.csv("Max100.csv")
podiumsdrivers$Year<-factor(podiumsdrivers$Year,levels=c("2016","2017","2018","2019","2020","2021","2022","2023","2024"))
podiumsdriversplot <- ggplot(podiumsdrivers, aes(x = Podiums, y =Year, fill = Position)) +
  geom_vline(xintercept = seq(0, 22, by = 2), linetype = "dashed", color = "grey") +
  geom_bar(stat="identity",color="black") + 
  scale_x_continuous(limits = c(0, 22), breaks = seq(0, 22, by = 2)) +
  scale_y_discrete(expand = c(0, 0))+
  scale_fill_manual(values = c( "gold","#C0C0C0", "#CD7F32")) +
  xlab("Podiums/Podios") +
  ylab("Year/Año") +
  ggtitle("Verstappen's podiums in F1/Podios de Verstappen en la F1") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 14, hjust = 0.5),
        axis.ticks.y=element_blank(),
        axis.text.y = element_text(margin = margin(r = -25),size=13),
        axis.title = element_text(size = 14),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15)) +
  labs(caption = " ", subtitle = "2016-2024*", fill = "Position/Posición") 
podiumsdriversplot 


gapSingaporeQ<-read.csv("GapAUS2014.csv")
pQualyS<-ggplot(gapSingaporeQ, aes(x=Gap,y=reorder(Driver,-Gap),fill=Driver))+ 
  geom_vline(xintercept = seq(0, 1.6, by = 0.2), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("HAM"="#6CD3BF","ROS"="#6CD3BF","RIC"="#3671C6","ALO"="#F91536","MAG"="#F58021"))+
  scale_x_continuous(breaks = seq(0, 1.6, by = .2))+
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
  labs(caption = "@f1.datascience",subtitle = "2014 Australian GP")+theme(legend.position = "none")
pQualyS



# Load the necessary library
library(ggplot2)

# Define team colors
teamscolors <- c("Red Bull Racing"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
                 "Alpine"="#FF66C4","Haas F1 Team"="#B6BABD","RB"="#0335D3","McLaren"="#F58020","Kick Sauber"="#04C404")

# Create the data frame
wins <- data.frame(
  Wins = c(243, 183, 125, 115, 114),
  Team = c("Ferrari", "McLaren", "Mercedes", "Red Bull Racing", "Williams")
)

# Create the bar plot
ggplot(wins, aes(x = Team, y = Wins, fill = Team)) +
  geom_hline(yintercept = seq(0, 250, by = 50), linetype = "dashed", color = "grey")+
  geom_bar(stat = "identity") +
  geom_text(aes(label = Wins), vjust = -0.5, size = 4, color = "black") +  # Add labels with number of wins
  scale_fill_manual(values = teamscolors) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 250), breaks = seq(0, 250, by = 50)) +
  labs(title = "Most wins by an F1 Team",
       x = "Team",
       y = "Number of Wins",
       caption = "@f1.datascience",
       subtitle = "After the Saudi Arabia GP") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.position = "none")
