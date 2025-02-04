library(ggplot2)
library(dplyr)
library(gganimate)
setwd("/Users/ainefernandez/documents/F1DataScience")
VERQ<-read.csv("telVERQBEL.csv")
LECQ<-read.csv("telLECQBEL.csv")
PERQ<-read.csv("telPERQBEL.csv")
VERQ$faster <- NA

for (i in 1:nrow(VERQ)) {
  if (!is.na(VERQ$Speed[i]) & !is.na(LECQ$Speed[i]) & !is.na(PERQ$Speed[i])) {
    if (VERQ$Speed[i] > LECQ$Speed[i] & VERQ$Speed[i] > PERQ$Speed[i]) {
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

LapDataVER <- data.frame(X = VERQ$X, Y = VERQ$Y, Speed = VERQ$Speed, Faster = VERQ$faster)

pVERS <- ggplot(LapDataVER, aes(x = Y, y = X)) +  # Corrected the data frame name to LapDataVER
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(Y), y = lag(X), xend = Y, yend = X, color = Faster, group = Faster),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("Verstappen is faster" = "#3671C6", "Leclerc is faster" = "#F91536", "Pérez is faster" = "gold")) +
  coord_flip() +
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
  labs(title = "VER, LEC and PER fastest lap comparison",
       subtitle = "2023 Belgian GP",
       caption = "@f1.datascience")

pVERS


gapBELQ<-read.csv("BELQ.csv")
pQualyS<-ggplot(gapBELQ, aes(x=GapQ3,y=reorder(Driver,-GapQ3),fill=Team))+ 
  geom_vline(xintercept = c(0.5,1, 1.5,2.0,2.5,3.0), linetype = "dashed", color = "grey")+
  
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
                               "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo "="#C92D4B"))+
  scale_x_continuous(breaks = seq(0, 3.0, by = 0.2))+
  scale_y_discrete(expand = c(0,0))+
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Gap to best in Q3")+
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
  labs(caption = "@f1.datascience",subtitle = "2023 Belgian GP")+theme(legend.position = "none")
pQualyS


gapBELQ<-read.csv("BELQ.csv")
pQualyS2<-ggplot(gapBELQ, aes(x=GapQ2,y=reorder(Driver,-GapQ2),fill=Team))+ 
  geom_vline(xintercept = c(0.5,1, 1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0), linetype = "dashed", color = "grey")+
  
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
                               "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo"="#C92D4B"))+
  scale_x_continuous(breaks = seq(0, 5.0, by = 0.5))+
  scale_y_discrete(expand = c(0,0))+
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Gap to best in Q2")+
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
  labs(caption = "@f1.datascience",subtitle = "2023 Belgian GP")+theme(legend.position = "none")
pQualyS2

VERQ<-read.csv("telVERSQBEL.csv")
SAIQ<-read.csv("telSAISQBEL.csv")
PIAQ<-read.csv("telPIASQBEL.csv")
VERQ$faster <- NA

for (i in 1:nrow(VERQ)) {
  if (!is.na(VERQ$Speed[i]) & !is.na(PIAQ$Speed[i]) & !is.na(SAIQ$Speed[i])) {
    if (VERQ$Speed[i] > PIAQ$Speed[i] & VERQ$Speed[i] > SAIQ$Speed[i]) {
      VERQ$faster[i] <- "Verstappen is faster"
    } else if (PIAQ$Speed[i] >= VERQ$Speed[i] & PIAQ$Speed[i] > SAIQ$Speed[i]) {
      VERQ$faster[i] <- "Piastri is faster"
    } else if (SAIQ$Speed[i] > VERQ$Speed[i] & SAIQ$Speed[i] > PIAQ$Speed[i]) {
      VERQ$faster[i] <- "Sainz is faster"
    } else {
      VERQ$faster[i] <- "Verstappen is faster"
    }
  } else {
    VERQ$faster[i] <- "Verstappen is faster"
  }
}

LapDataVER2 <- data.frame(X = VERQ$X, Y = VERQ$Y, Speed = VERQ$Speed, Faster = VERQ$faster)

pVERS2 <- ggplot(LapDataVER2, aes(x = Y, y = X)) + 
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(Y), y = lag(X), xend = Y, yend = X, color = Faster, group = Faster),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("Verstappen is faster" = "#3671C6", "Sainz is faster" = "#F91536", "Piastri is faster" = "#F58020")) +
  coord_flip() +
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
  labs(title = "VER, PIA and SAI fastest lap comparison",
       subtitle = "2023 Belgian GP: Sprint Shootout",
       caption = "@f1.datascience")

pVERS2

race_pace_data<-read.csv("RacePaceBEL.csv")
# Calculate the median race pace for each driver
driver_median_race_pace <- aggregate(LapTimeSeconds ~ Driver, data = race_pace_data, median)

# Sort the "Driver" factor levels based on median race pace
race_pace_data$Driver <- factor(race_pace_data$Driver, levels = driver_median_race_pace[order(driver_median_race_pace$`LapTimeSeconds`), "Driver"])

# Create the boxplot using ggplot
racepace <- ggplot(race_pace_data, aes(x = Driver, y = LapTimeSeconds, fill = Driver)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(120, 145), breaks = seq(120, 145, by = 2)) +
  scale_fill_manual(values = c( "SAI" = "#F91536", "LEC" = "#F91536","PIA"="#F58020",
                                "HAM" = "#6CD3BF", "RUS" = "#6CD3BF", "NOR" = "#F58020")) +
  labs(title = "Race Pace Comparison",
       x = "Driver",
       y = "LapTime (s)",
       subtitle = "2019 Belgian GP",
       caption = "@f1.datascience") +
  theme_bw() +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none")

racepace




stints<-read.csv("StintsBelgium.csv")
stints$Stint_Compound <- paste(stints$Stint, stints$Compound)
stints$Stint_Compound<-factor(stints$Stint_Compound,levels=c("4 HARD","4 SOFT","4 MEDIUM","3 SOFT","3 HARD","3 MEDIUM","2 SOFT","2 MEDIUM","2 HARD",
                                                             "1 SOFT","1 MEDIUM","1 HARD"))
stints$Driver<-factor(stints$Driver,levels=c("PIA","SAI","HUL","SAR","RIC",
                                                           "MAG","ALB","ZHO","BOT","GAS",
                                                           "TSU","STR","OCO","NOR","RUS",
                                                           "ALO","HAM","LEC","PER","VER"))
stintsSpain <- ggplot(stints, aes(Driver, StintLength, fill = Stint_Compound)) +
  geom_bar(stat = "identity",color="black") +
  coord_flip() +
  scale_y_continuous(limits = c(0,44), breaks = seq(0, 44, by = 2)) +
  scale_fill_manual(values = c("1 SOFT" = "#FF3333", "2 SOFT" = "#FF3333", "3 SOFT" = "#FF3333", "4 SOFT" = "#FF3333", "1 MEDIUM" = "#ffe541", "2 MEDIUM" = "#ffe541",
                               "3 MEDIUM" = "#ffe541", "4 MEDIUM" = "#ffe541",
                               "1 HARD" = "#d2d2d2", "2 HARD" = "#d2d2d2", "3 HARD" = "#d2d2d2", "4 HARD" = "#d2d2d2")) +
  labs(title = "Tyre strategies", subtitle = "2023 Belgian GP", caption = "@f1.datascience", y = "Lap") +
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

results<-read.csv("ResultsBELR2023.csv")
results$GridPosition[results$Abbreviation=="HUL"]<-20
left_label <- paste(results$Abbreviation, round(results$GridPosition),sep=", ")
right_label <- paste(results$Abbreviation, round(results$Position),sep=", ")

p <- ggplot(results) + 
  geom_segment(aes(x=1, xend=2, y=GridPosition, yend=Position, col=Abbreviation), size=1, show.legend=F) +
  geom_vline(xintercept=1, linetype="dashed", size=.1) +
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(values = c("VER"="#3671C6","PER"="#3671C6","ALO"="#358C75","SAI"="#F91536","HAM"="#6CD3BF","STR"="#358C75","RUS"="#6CD3BF","BOT"="#C92D4B","GAS"="#2293D1","ALB"="#37BEDD",
                                "TSU"="#5E8FAA","SAR"="#37BEDD","MAG"="#B6BABD","DEV"="#5E8FAA","HUL"="#B6BABD","ZHO"="#C92D4B","NOR"="#F58020","OCO"="#2293D1","LEC"="#F91536","PIA"="#F58020")) +
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
  labs(title="Grid and race position",subtitle="2023 Belgian GP",caption = "@f1.datascience")
p


race_pace_data<-read.csv("BELGIUM2023.csv")
# Calculate the median race pace for each driver
driver_median_race_pace <- aggregate(LapTimeSeconds ~ Driver, data = race_pace_data, median)

# Sort the "Driver" factor levels based on median race pace
race_pace_data$Driver <- factor(race_pace_data$Driver, levels = driver_median_race_pace[order(driver_median_race_pace$`LapTimeSeconds`), "Driver"])

# Create the boxplot using ggplot
racepace <- ggplot(race_pace_data, aes(x = Driver, y = LapTimeSeconds, fill = Driver)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(104, 130), breaks = seq(104, 130, by = 2)) +
  scale_fill_manual(values = c("VER" = "#3671C6", "PER" = "#3671C6", "LEC" = "#F91536",
                               "HAM" = "#6CD3BF","ALO"="#358C75")) +
  labs(title = "Race Pace Comparison",
       x = "Driver",
       y = "LapTime (s)",
       subtitle = "2023 Belgian GP",
       caption = "@f1.datascience") +
  theme_bw() +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none")

racepace

laps<-read.csv("LandoBELGIUM2023.csv")
lapsa<-laps[laps$LapNumber>16, ]
mcl<-ggplot(lapsa, aes(x=LapNumber, y=LapTimeSeconds, col=Driver)) +
  geom_line(size=1) +
  geom_point(size=3)+
  scale_color_manual(values=c("OCO"="#2293D1","NOR"="#F58020","STR"="#358C75"))+
  scale_y_continuous(limits=c(110,134),breaks=seq(110, 134, by = 2))+
  scale_x_continuous(breaks=seq(17,44, by = 1))+
  theme_bw()+
  labs(x = "Lap", y = "Lap Time (s)",title="NOR,OCO and STR laptimes comparison",subtitle ="2023 Belgian GP (17-44)",caption = "@f1.datascience" )+
  theme(panel.background=element_rect(fill="white", color="white"),
        plot.background=element_rect(fill="white", color="white"),
        plot.title=element_text(size=18, hjust=0.5),
        plot.subtitle=element_text(size=15, hjust=0.5),
        plot.caption=element_text(size=11, hjust=0.5))

mcl

