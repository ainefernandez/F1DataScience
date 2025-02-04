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
LECQ <- read.csv("LECQMexico.csv")
SAIQ <- read.csv("SAIQMexico.csv")
VERQ <- read.csv("VERQMexico.csv")
LECQ$faster <- NA

for (i in 1:nrow(LECQ)) {
  if (!is.na(LECQ$Speed[i]) & !is.na(SAIQ$Speed[i]) & !is.na(VERQ$Speed[i])) {
    if (LECQ$Speed[i] > SAIQ$Speed[i] & LECQ$Speed[i] > VERQ$Speed[i]) {
      LECQ$faster[i] <- "Leclerc is faster"
    } else if (SAIQ$Speed[i] > LECQ$Speed[i] & SAIQ$Speed[i] > VERQ$Speed[i]) {
      LECQ$faster[i] <- "Sainz is faster"
    } else if (VERQ$Speed[i] > LECQ$Speed[i] & VERQ$Speed[i] > SAIQ$Speed[i]) {
      LECQ$faster[i] <- "Verstappen is faster"
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
    values = c("Verstappen is faster" = "#3671C6", "Sainz is faster" = "gold", "Leclerc is faster" = "#F91536"),
    breaks = c("Leclerc is faster", "Sainz is faster", "Verstappen is faster")
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
  labs(title = "LEC, SAI, and VER Fastest Lap Comparison", subtitle = "2023 Mexican GP", caption = "@f1.datascience")

pSAIQ

results<-read.csv("ResultsMexicoR2023.csv")
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
  labs(title="Grid and race position",subtitle="2023 Mexican GP",caption = "@f1.datascience")
p



racepace<-read.csv("racepaceMexico.csv")
library(dplyr)  # Load the dplyr package for data manipulation

# Calculate median lap time for each driver
median_lap_time <- racepace %>%
  group_by(Driver) %>%
  summarise(MedianLapTime = median(LapTime))

# Reorder the "Driver" factor based on median lap time
racepace$Driver <- factor(racepace$Driver, levels = median_lap_time$Driver[order(median_lap_time$MedianLapTime)])
max(racepace$LapTime)
# Create the reordered boxplot
racepaceplot <- ggplot(racepace, aes(x = reorder(Driver, LapTime), y = LapTime, fill = Driver)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(81, 88), breaks = seq(81, 88, by = 1)) +
  scale_fill_manual(values = drivercolors) +
  labs(title = "Race Pace Comparison",
       x = "Driver",
       y = "LapTime (s)",
       subtitle = "2023 Mexico GP",
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

PosMex2<-read.csv("POSMexico2023.csv")
monlando3 <- ggplot(PosMex2, aes(x = LapNumber, y = Position, col = Driver)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("NOR" = "#F58020", "VER" = "#3671C6", "LEC" = "#F91536", "SAI" = "gold", "HAM" = "#6CD3BF"),
                     breaks = c("VER", "HAM", "LEC","SAI","NOR")) +
  scale_y_reverse(limits = c(20, 1), breaks = seq(1, 20, by = 1)) +
  scale_x_continuous(breaks = seq(0, 72, by = 4)) +
  theme_bw() +
  labs(x = "Lap", y = "Position", title = "VER,HAM,LEC,SAI and NOR Position Changes", subtitle = "2023 Mexican GP", caption = "@f1.datascience") +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5))

monlando3
