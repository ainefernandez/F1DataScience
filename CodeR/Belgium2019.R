library(ggplot2)
library(dplyr)
library(gganimate)
setwd("/Users/ainefernandez/documents/F1DataScience")
VETQ<-read.csv("telVET2019.csv")
LECQ<-read.csv("telLEC2019.csv")
HAMQ<-read.csv("telHAM2019.csv")
LECQ$faster <- NA

for (i in 1:nrow(LECQ)) {
  if (!is.na(LECQ$Speed[i]) & !is.na(HAMQ$Speed[i]) & !is.na(VETQ$Speed[i])) {
    if (LECQ$Speed[i] > VETQ$Speed[i] & LECQ$Speed[i] > HAMQ$Speed[i]) {
      LECQ$faster[i] <- "Leclerc is faster"
    } else if (VETQ$Speed[i] > LECQ$Speed[i] & VETQ$Speed[i] > HAMQ$Speed[i]) {
      LECQ$faster[i] <- "Vettel is faster"
    } else if (HAMQ$Speed[i] > LECQ$Speed[i] & HAMQ$Speed[i] >VETQ$Speed[i]) {
      LECQ$faster[i] <- "Hamilton is faster"
    } else {
      LECQ$faster[i] <- "Leclerc is faster"
    }
  } else {
    LECQ$faster[i] <- "Leclerc is faster"
  }
}

LapDataLEC<- data.frame(X = LECQ$X, Y = LECQ$Y, Speed=LECQ$Speed, Faster=LECQ$faster)
pVERS <- ggplot(LapDataLEC, aes(x = Y, y = X)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(Y), y = lag(X), xend = Y, yend = X, color = Faster, group = Faster),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("Vettel is faster" = "gold", "Leclerc is faster" = "#F91536", "Hamilton is faster" = "#6CD3BF"))  +
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
  labs(title = "LEC, VET and HAM fastest lap comparison",
       subtitle = "2019 Belgian GP",
       caption = "@f1.datascience")
pVERS




spa2019<-read.csv("Belgium2019.csv")
anim <- ggplot(spa2019) +
  geom_line(size = 1, aes(x = Lap, y = Position, col = Driver)) +
  geom_point(size = 3, aes(x = Lap, y = Position, col = Driver)) +
  scale_color_manual(values = c("LEC" = "#F91536", "HAM" = "#6CD3BF",
                                "VET" = "gold", "BOT" = "steelblue")) +
  scale_y_reverse(breaks = seq(1, 4, by = 1)) +
  scale_x_continuous(breaks = seq(20, 30, by = 1)) +
  theme_bw() +
  labs(x = "Lap", y = "Position") +
  transition_reveal(Lap) +  # Corrected the transition_reveal function call
  ggtitle("2019 Belgian GP Race") +  # Used ggtitle() to set the plot title
  labs(subtitle = "Laps 20-30", caption = "@f1.datascience") +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),  # Added subtitle size
        plot.caption = element_text(size = 11, hjust = 0.5))

anim









# Load required libraries
library(ggplot2)
library(gganimate)

# Create the data frame with the given data
data <- data.frame(
  Driver = c(rep("LEC", 11), rep("VET", 11), rep("HAM", 11), rep("BOT", 11)),
  Position = c(1, 2, 4, 3, 2, 2, 2, 1, 1, 1, 1,
               4, 4, 3, 1, 1, 1, 1, 2, 2, 2, 2,
               2, 1, 4, 4, 3, 3, 3, 3, 3, 3, 3,
               3, 3, 2, 2, 4, 4, 4, 4, 4, 4, 4),
  Lap = rep(20:30, times = 4)
)

# Create the animated plot
anim <- ggplot(data) +
  geom_line(size = 1, aes(x = Lap, y = Position, col = Driver)) +
  geom_point(size = 3, aes(x = Lap, y = Position, col = Driver)) +
  scale_color_manual(values = c("LEC" = "#F91536", "HAM" = "#6CD3BF",
                                "VET" = "gold", "BOT" = "steelblue")) +
  scale_y_reverse(breaks = seq(1, 4, by = 1)) +
  scale_x_continuous(breaks = seq(20, 30, by = 1)) +
  theme_bw() +
  labs(x = "Lap", y = "Position") +
  transition_reveal(Lap) +
  ggtitle("2019 Belgian GP Race") +
  labs(subtitle = "Laps 20-30", caption = "@f1.datascience") +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5))

# Display the animated plot
anim


race_pace_data<-read.csv("BEL2019.csv")
# Calculate the median race pace for each driver
driver_median_race_pace <- aggregate(LapTimeSeconds ~ Driver, data = race_pace_data, median)

# Sort the "Driver" factor levels based on median race pace
race_pace_data$Driver <- factor(race_pace_data$Driver, levels = driver_median_race_pace[order(driver_median_race_pace$`LapTimeSeconds`), "Driver"])

# Create the boxplot using ggplot
racepace <- ggplot(race_pace_data, aes(x = Driver, y = LapTimeSeconds, fill = Driver)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(104, 130), breaks = seq(104, 130, by = 2)) +
  scale_fill_manual(values = c( "VET" = "#F91536", "LEC" = "#F91536",
                               "HAM" = "#6CD3BF", "BOT" = "#6CD3BF", "NOR" = "#F58020")) +
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



