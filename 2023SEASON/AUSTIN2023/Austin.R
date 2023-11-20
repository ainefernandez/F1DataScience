library(ggplot2)
library(dplyr)
library(gganimate)
setwd("/Users/ainefernandez/documents/F1DataScience")
drivercolors <- c("VER" = "#3671C6","PER" = "#3671C6","ALO" = "#358C75","SAI" = "#F91536","HAM" = "#6CD3BF",
                  "STR" = "#358C75","RUS" = "#6CD3BF","BOT" = "#C92D4B","GAS" = "#2293D1","ALB" = "#37BEDD",
                  "TSU" = "#5E8FAA","SAR" = "#37BEDD","MAG" = "#B6BABD","DEV" = "#5E8FAA","HUL" = "#B6BABD",
                  "ZHO" = "#C92D4B", "NOR" = "#F58020","OCO" = "#2293D1","LEC" = "#F91536","PIA" = "#F58020", "RIC"="#5E8FAA","LAW"="#5E8FAA")
teamscolors<-c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
               "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo"="#C92D4B")
LECQ <- read.csv("LECQAustin.csv")
NORQ <- read.csv("NORQAustin.csv")
HAMQ <- read.csv("HAMQAustin.csv")
LECQ$faster <- NA

for (i in 1:nrow(LECQ)) {
  if (!is.na(LECQ$Speed[i]) & !is.na(NORQ$Speed[i]) & !is.na(HAMQ$Speed[i])) {
    if (LECQ$Speed[i] > NORQ$Speed[i] & LECQ$Speed[i] > HAMQ$Speed[i]) {
      LECQ$faster[i] <- "Leclerc is faster"
    } else if (NORQ$Speed[i] > LECQ$Speed[i] & NORQ$Speed[i] > HAMQ$Speed[i]) {
      LECQ$faster[i] <- "Norris is faster"
    } else if (HAMQ$Speed[i] > LECQ$Speed[i] & HAMQ$Speed[i] > NORQ$Speed[i]) {
      LECQ$faster[i] <- "Hamilton is faster"
    } else {
      LECQ$faster[i] <- "Leclerc is faster"
    }
  } else {
    LECQ$faster[i] <- "Leclerc is faster"
  }
}

LapDataVERJAP<- data.frame(X = LECQ$X, Y = LECQ$Y, Speed=LECQ$Speed, Faster=LECQ$faster)
pSAIQ <- ggplot(LapDataVERJAP, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Faster, group = Faster),
               size = 3, lineend = "round") +
  scale_color_manual(
    values = c("Hamilton is faster" = "#6CD3BF", "Norris is faster" = "#F58020", "Leclerc is faster" = "#F91536"),
    breaks = c("Leclerc is faster", "Norris is faster", "Hamilton is faster")
  ) +
  coord_flip() + coord_equal() +
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
  labs(title = "LEC, NOR, and HAM Fastest Lap Comparison", subtitle = "2023 US GP", caption = "@f1.datascience")

pSAIQ

gapSingaporeQ<-read.csv("GapAustin.csv")
gapSingaporeQ$Gap[gapSingaporeQ$Driver=="VER"]<-0.358
pQualyS<-ggplot(gapSingaporeQ, aes(x=Gap,y=reorder(Driver,-Gap),fill=Driver))+ 
  geom_vline(xintercept = c(.2,.4,.6,.8,1,1.2,1.4,1.6,1.8,2,2.2), linetype = "dashed", color = "grey")+
  
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, 2.2, by = .2))+
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
  labs(caption = "@f1.datascience",subtitle = "2023 US GP")+theme(legend.position = "none")
pQualyS

singapur<-read.csv("GapNorrisSprint.csv")
monlando<-ggplot(singapur, aes(x=Lap, y=Gap)) +
  geom_line(size=1, col="#F58020") +
  geom_point(size=3, col="#F58020")+
  geom_hline(yintercept = 1, color = "red", linetype = "solid",size=1) +
  scale_y_continuous(limits=c(0,7),breaks=seq(0, 7, by = .5))+
  scale_x_continuous(breaks=seq(10,19, by = 1))+
  theme_bw()+
  labs(x = "Lap", y = "Gap (s)",title="NOR gap to LEC",subtitle ="2023 US GP Sprint (10-19)",caption = "@f1.datascience" )+
  theme(panel.background=element_rect(fill="white", color="white"),
        plot.background=element_rect(fill="white", color="white"),
        plot.title=element_text(size=18, hjust=0.5),
        plot.subtitle=element_text(size=15, hjust=0.5),
        plot.caption=element_text(size=11, hjust=0.5))

monlando

results<-read.csv("ResultsAustinR2023.csv")
results <- results[results$Abbreviation != "nan", ]
results$GridPosition[results$Abbreviation=="HUL"]<-18
results$GridPosition[results$Abbreviation=="TSU"]<-11
results$GridPosition[results$Abbreviation=="MAG"]<-17
results$GridPosition[results$Abbreviation=="ALO"]<-19
results$GridPosition[results$Abbreviation=="STR"]<-20
results$GridPosition[results$Abbreviation=="HAM"]<-3
results$GridPosition[results$Abbreviation=="LEC"]<-1
results$Position[results$Abbreviation=="LEC"]<-20
results$Position[results$Abbreviation=="TSU"]<-8

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
  labs(title="Grid and race position",subtitle="2023 US GP",caption = "@f1.datascience")
p


stints<-read.csv("StintsAustin.csv")
stints$Stint_Compound <- paste(stints$Stint, stints$Compound)
stints$Stint_Compound<-factor(stints$Stint_Compound,levels=c("5 SOFT","4 HARD","4 MEDIUM","4 SOFT","3 SOFT","3 HARD","3 MEDIUM","2 SOFT","2 MEDIUM","2 HARD",
                                                             "1 SOFT","1 MEDIUM","1 HARD"))
stints$Driver<-factor(stints$Driver,levels=c("OCO","PIA","ALO","RIC","MAG",
                                             "ZHO","LEC","HAM","BOT","HUL",
                                             "SAR","ALB","TSU","STR","GAS",
                                             "RUS","PER","SAI","NOR","VER"))
stintsSpain <- ggplot(stints, aes(Driver, StintLength, fill = Stint_Compound)) +
  geom_bar(stat = "identity",color="black") +
  coord_flip() +
  scale_y_continuous(limits = c(0,56), breaks = seq(0, 56, by = 2)) +
  scale_fill_manual(values = c("5 SOFT"="#FF3333","1 SOFT" = "#FF3333", "2 SOFT" = "#FF3333", "3 SOFT" = "#FF3333", "4 SOFT" = "#FF3333", "1 MEDIUM" = "#ffe541", "2 MEDIUM" = "#ffe541",
                               "3 MEDIUM" = "#ffe541", "4 MEDIUM" = "#ffe541",
                               "1 HARD" = "#d2d2d2", "2 HARD" = "#d2d2d2", "3 HARD" = "#d2d2d2", "4 HARD" = "#d2d2d2")) +
  labs(title = "Tyre strategies", subtitle = "2023 US GP", caption = "@f1.datascience", y = "Lap") +
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

singapur2<-read.csv("laptimesAustin.csv")
monlando2<-ggplot(singapur2, aes(x=LapNumber, y=LapTime, col=Driver)) +
  geom_line(size=1) +
  geom_point(size=3)+
  scale_color_manual(values=c("HAM"="#6CD3BF","NOR"="#F58020","VER"="#3671C6"))+
  scale_y_continuous(limits=c(99.5,103),breaks=seq(99.5, 103, by = .5))+
  scale_x_continuous(breaks=seq(40,56, by = 1))+
  theme_bw()+
  labs(x = "Lap", y = "Lap Time (s)",title="VER, HAM, NOR laptimes comparison",subtitle ="2023 US GP (46-56)",caption = "@f1.datascience" )+
  theme(panel.background=element_rect(fill="white", color="white"),
        plot.background=element_rect(fill="white", color="white"),
        plot.title=element_text(size=18, hjust=0.5),
        plot.subtitle=element_text(size=15, hjust=0.5),
        plot.caption=element_text(size=11, hjust=0.5))

monlando2

posMex<-read.csv("POSAustin2023.csv")
monlando3 <- ggplot(posMex, aes(x = LapNumber, y = Position, col = Driver)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("TSU" = "#5E8FAA", "SAR" = "gold", "ALB" = "#37BEDD", "STR" = "#358C75", "ALO" = "black")) +
  scale_y_reverse(limits = c(20, 7), breaks = seq(1, 20, by = 1)) +
  scale_x_continuous(breaks = seq(0, 56, by = 4)) +
  theme_bw() +
  labs(x = "Lap", y = "Position", title = "STR,TSU,ALB,SAR and ALO Position Changes", subtitle = "2023 US GP", caption = "@f1.datascience") +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5))

monlando3
