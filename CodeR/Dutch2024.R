library(ggplot2)
library(dplyr)
library(gganimate)
setwd("/Users/ainefernandez/documents/F1DataScience")
drivercolors <- c("VER" = "#3671C6","PER" = "#3671C6","ALO" = "#358C75","SAI" = "#F91536","HAM" = "#6CD3BF","DOO" = "#FF66C4",
                  "STR" = "#358C75","RUS" = "#6CD3BF","BOT" = "#04C404","GAS" = "#FF66C4","ALB" = "#37BEDD",
                  "TSU" = "#0335D3","SAR" = "#37BEDD","MAG" = "#B6BABD","DEV" = "#5E8FAA","HUL" = "#B6BABD",
                  "ZHO" = "#04C404", "NOR" = "#F58020","OCO" = "#FF66C4","LEC" = "#F91536","PIA" = "#F58020", "RIC"="#0335D3","LAW"="#5E8FAA","BEA"="#B6BABD","COL"="#37BEDD")
teamscolors<-c("Red Bull Racing"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
               "Alpine"="#FF66C4","Haas F1 Team"="#B6BABD","RB"="#0335D3","McLaren"="#F58020","Kick Sauber"="#04C404","AlphaTauri"="#5E8FAA","Alfa Romeo"="#C92D4B")





results<-read.csv("ResultsAbu.csv")
#results$GridPosition[results$Abbreviation=="HAM"]<-19
#results$GridPosition[results$Abbreviation=="OCO"]<-20
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
  labs(title="Grid and race position",subtitle="2024 Abu Dhabi GP",caption = " ")
p

stints<-read.csv("StintsAbu.csv")
stints$Stint_Compound <- paste(stints$Stint, stints$Compound)
stints$Stint_Compound<-factor(stints$Stint_Compound,levels=c("5 MEDIUM","5 HARD","6 SOFT","5 SOFT","4 HARD","4 MEDIUM","4 INTERMEDIATE","4 SOFT","3 SOFT","3 HARD","3 MEDIUM","3 INTERMEDIATE","2 SOFT","2 MEDIUM","2 HARD", "2 INTERMEDIATE",
                                                             "1 SOFT","1 MEDIUM","1 HARD","1 WET","1 INTERMEDIATE"))
stints$Driver<-factor(stints$Driver,levels=c("PER","COL","BOT","LAW","MAG","DOO","STR",
                                             "ZHO","TSU","ALB","PIA","ALO",
                                             "HUL","GAS","VER","RUS","HAM",
                                             "LEC","SAI","NOR"))
stintsSpain <- ggplot(stints, aes(Driver, StintLength, fill = Stint_Compound)) +
  geom_bar(stat = "identity",color="black") +
  coord_flip() +
  scale_y_continuous(limits = c(0,58), breaks = seq(0, 58, by = 2)) +
  scale_fill_manual(values = c("6 SOFT"="#FF3333","5 SOFT"="#FF3333","1 SOFT" = "#FF3333", "2 SOFT" = "#FF3333", "3 SOFT" = "#FF3333", "4 SOFT" = "#FF3333", 
                               "1 MEDIUM" = "#ffe541", "2 MEDIUM" = "#ffe541",
                               "3 MEDIUM" = "#ffe541", "4 MEDIUM" = "#ffe541","5 MEDIUM" = "#ffe541",
                               "1 HARD" = "#d2d2d2", "2 HARD" = "#d2d2d2", "3 HARD" = "#d2d2d2", "4 HARD" = "#d2d2d2","5 HARD" = "#d2d2d2","1 INTERMEDIATE"="#66A441","2 INTERMEDIATE"="#66A441","3 INTERMEDIATE"="#66A441","4 INTERMEDIATE"="#66A441","1 WET"="#2B8CCB")) +
  labs(title = "Tyre strategies", subtitle = "2024 Abu Dhabi GP", caption = " ", y = "Lap") +
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

gapSingaporeQ<-read.csv("GapRacePaceAbu.csv")
names(gapSingaporeQ)<-c("Team","Gap")
pQualyS<-ggplot(gapSingaporeQ, aes(x=Gap,y=reorder(Team,-Gap),fill=Team))+ 
  geom_vline(xintercept = seq(0, 1.6, by =.2), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=teamscolors)+
  scale_x_continuous(breaks = seq(0,1.6, by =.2))+
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
        axis.text = element_text(size = 10),
        axis.text.y = element_text(margin = margin(r = -30)),
        axis.ticks.y =element_blank())+
  labs(caption = " ",subtitle = "2024 Abu Dhabi GP")+theme(legend.position = "none")
pQualyS


# Load the data
gapSingaporeQ <- read.csv("RacePaceQatarDrivers.csv")
names(gapSingaporeQ) <- c("Driver", "Team", "LapTime","LapNumber")



# Create a boxplot
library(ggplot2)

pQualyS <- ggplot(gapSingaporeQ, aes(x = reorder(Driver, LapTime), y = LapTime, fill = Team)) +
  geom_boxplot() +
  scale_fill_manual(values = teamscolors) +
  scale_y_continuous(breaks = seq(floor(min(gapSingaporeQ$LapTime)), ceiling(max(gapSingaporeQ$LapTime)), by = 0.5)) +
  xlab("Driver") +
  ylab("Lap Time (seconds)") +
  ggtitle("Race Pace (Drivers)") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 10),
    axis.ticks.y = element_blank()
  ) +
  labs(caption = " ", subtitle = "2024 Qatar GP")+theme(legend.position = "none")

pQualyS

data <- read.csv("POSAbu2024Race.csv")
ggplot(data, aes(x = LapNumber, y = Position)) +
  geom_line(color = "#F91536", size = 1) +  # Line plot
  geom_point(color = "#F91536", size = 2) +  # Points on the line
  scale_y_reverse(breaks = seq(0, 20, by = 1)) +
  scale_x_continuous(limits = c(0,58), breaks = seq(0, 58, by = 2)) +# Reverse y-axis for positions
  labs(
    title = "Leclerc's amazing recovery drive",
    x = "Lap Number",
    y = "Position"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9),
    axis.ticks.y = element_blank()
  ) +
  labs(caption = " ", subtitle = "2024 Abu Dhabi GP")+theme(legend.position = "none")







HAMQ <- read.csv("RICFLSinga.csv")
NORQ <- read.csv("NORFLSinga.csv")
NORQ$faster <- NA

for (i in 1:nrow(NORQ)) {
  if (!is.na(NORQ$Speed[i]) & !is.na(HAMQ$Speed[i])) {
    if (HAMQ$Speed[i] > NORQ$Speed[i]) {
      NORQ$faster[i] <- "Ricciardo is faster"
    } else if (NORQ$Speed[i] > HAMQ$Speed[i]) {
      NORQ$faster[i] <- "Norris is faster"
    } else {
      NORQ$faster[i] <- "Ricciardo is faster"
    }
  } else {
    NORQ$faster[i] <- "Ricciardo is faster"
  }
}


LapDataVERJAP<- data.frame(X = NORQ$X, Y = NORQ$Y, Speed=NORQ$Speed, Faster=NORQ$faster)
pSAIQ <- ggplot(LapDataVERJAP, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Faster, group = Faster),
               size = 3, lineend = "round") +
  scale_color_manual(
    values = c("Ricciardo is faster" = "#0335D3", "Norris is faster" = "#F58020", "Verstappen is faster" = "#3671C6"),
    breaks = c("Ricciardo is faster","Norris is faster")
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
  labs(title = "RIC vs NOR fastest lap comparison", subtitle = "2024 Singapore GP", caption = "")

pSAIQ
