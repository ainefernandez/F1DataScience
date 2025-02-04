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
VERQ <- read.csv("VERQQAT.csv")
RUSQ <- read.csv("RUSQQAT.csv")
HAMQ <- read.csv("HAMQQAT.csv")
VERQ$faster <- NA

for (i in 1:nrow(VERQ)) {
  if (!is.na(VERQ$Speed[i]) & !is.na(RUSQ$Speed[i]) & !is.na(HAMQ$Speed[i])) {
    if (VERQ$Speed[i] > RUSQ$Speed[i] & VERQ$Speed[i] > HAMQ$Speed[i]) {
      VERQ$faster[i] <- "Verstappen is faster"
    } else if (RUSQ$Speed[i] > VERQ$Speed[i] & RUSQ$Speed[i] > HAMQ$Speed[i]) {
      VERQ$faster[i] <- "Russell is faster"
    } else if (HAMQ$Speed[i] > VERQ$Speed[i] & HAMQ$Speed[i] > RUSQ$Speed[i]) {
      VERQ$faster[i] <- "Hamilton is faster"
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
    values = c("Hamilton is faster" = "purple", "Russell is faster" = "#6CD3BF", "Verstappen is faster" = "#3671C6"),
    breaks = c("Verstappen is faster", "Russell is faster", "Hamilton is faster")
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
  labs(title = "VER, RUS, and HAM Fastest Lap Comparison", subtitle = "2023 Qatar GP", caption = "@f1.datascience")

pSAIQ

results<-read.csv("ResultsQatarR2023.csv")
results <- results[results$Abbreviation != "nan", ]
results$GridPosition[results$Abbreviation=="PER"]<-20
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
  labs(title="Grid and race position",subtitle="2023 Qatar GP",caption = "@f1.datascience")
p

stints<-read.csv("StintsQAT.csv")
stints$Stint_Compound <- paste(stints$Stint, stints$Compound)
stints$Stint_Compound<-factor(stints$Stint_Compound,levels=c("5 SOFT","4 HARD","4 MEDIUM","4 SOFT","3 SOFT","3 HARD","3 MEDIUM","2 SOFT","2 MEDIUM","2 HARD",
                                                             "1 SOFT","1 MEDIUM","1 HARD"))
stints$Driver<-factor(stints$Driver,levels=c("SAI","HAM","SAR","LAW","HUL",
                                             "TSU","MAG","ALB","GAS","STR",
                                             "PER","ZHO","BOT","OCO","ALO",
                                             "LEC","RUS","NOR","PIA","VER"))
stintsSpain <- ggplot(stints, aes(Driver, StintLength, fill = Stint_Compound)) +
  geom_bar(stat = "identity",color="black") +
  coord_flip() +
  scale_y_continuous(limits = c(0,58), breaks = seq(0, 58, by = 2)) +
  scale_fill_manual(values = c("5 SOFT"="#FF3333","1 SOFT" = "#FF3333", "2 SOFT" = "#FF3333", "3 SOFT" = "#FF3333", "4 SOFT" = "#FF3333", "1 MEDIUM" = "#ffe541", "2 MEDIUM" = "#ffe541",
                               "3 MEDIUM" = "#ffe541", "4 MEDIUM" = "#ffe541",
                               "1 HARD" = "#d2d2d2", "2 HARD" = "#d2d2d2", "3 HARD" = "#d2d2d2", "4 HARD" = "#d2d2d2")) +
  labs(title = "Tyre strategies", subtitle = "2023 Qatar GP", caption = "@f1.datascience", y = "Lap") +
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


racepace<-read.csv("racepaceQAT.csv")
library(dplyr)  # Load the dplyr package for data manipulation

# Calculate median lap time for each driver
median_lap_time <- racepace %>%
  group_by(Driver) %>%
  summarise(MedianLapTime = median(LapTime))

# Reorder the "Driver" factor based on median lap time
racepace$Driver <- factor(racepace$Driver, levels = median_lap_time$Driver[order(median_lap_time$MedianLapTime)])

# Create the reordered boxplot
racepaceplot <- ggplot(racepace, aes(x = reorder(Driver, LapTime), y = LapTime, fill = Driver)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(84, 92), breaks = seq(84, 92, by = 2)) +
  scale_fill_manual(values = drivercolors) +
  labs(title = "Race Pace Comparison",
       x = "Driver",
       y = "LapTime (s)",
       subtitle = "2023 Qatar GP",
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

racepaceplot

pitstop<-read.csv("pitstopsQAT.csv")
average_pitstop <- pitstop %>%
  group_by(Team) %>%
  summarise(Avg_Pitstop = mean(Pitsop, na.rm = TRUE))
pitstopplot<-ggplot(average_pitstop, aes(x =reorder(Team,Avg_Pitstop), y = Avg_Pitstop, fill=Team)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = teamscolors) +
  scale_y_continuous(limits = c(0,5.5), breaks = seq(0, 5.5, by = .5), expand = c(0,0))+
  labs(title = "Average Pitstop time by team",
       x = "Team",
       y = "Pitstop time (s)",
       subtitle = "2023 Qatar GP",
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
pitstopplot
