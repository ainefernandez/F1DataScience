library(ggplot2)
library(dplyr)
library(gganimate)
setwd("/Users/ainefernandez/documents/F1DataScience")
drivercolors <- c("VER" = "#3671C6","PER" = "#3671C6","ALO" = "#358C75","SAI" = "#F91536","HAM" = "#6CD3BF",
                  "STR" = "#358C75","RUS" = "#6CD3BF","BOT" = "#C92D4B","GAS" = "#2293D1","ALB" = "#37BEDD",
                  "TSU" = "#5E8FAA","SAR" = "#37BEDD","MAG" = "#B6BABD","DEV" = "#5E8FAA","HUL" = "#B6BABD",
                  "ZHO" = "#C92D4B", "NOR" = "#F58020","OCO" = "#2293D1","LEC" = "#F91536","PIA" = "#F58020", "RIC"="#5E8FAA","LAW"="#5E8FAA")
teamscolors<-c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
               "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo "="#C92D4B")
results<-read.csv("ResultsVegasR2023.csv")
results <- results[results$Abbreviation != "nan", ]
results$GridPosition[results$Abbreviation=="ZHO"]<-19
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
  labs(title="Grid and race position",subtitle="2023 Las Vegas GP",caption = "@f1.datascience")
p


PosMex2<-read.csv("VegasPos.csv")
monlando3 <- ggplot(PosMex2, aes(x = Lap, y = Position, col = Driver)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("PIA" = "#F58020", "STR" = "#358C75", "OCO" = "#2293D1", "SAI" = "#F91536", "PER" = "#3671C6")) +
  scale_y_reverse(limits = c(20, 1), breaks = seq(1, 20, by = 1)) +
  scale_x_continuous(breaks = seq(0, 50, by = 5)) +
  theme_bw() +
  labs(x = "Lap", y = "Position", title = "Position changes during the race", subtitle = "2023 Las Vegas GP", caption = "@f1.datascience") +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5))

monlando3

data<-read.csv("PitstopsVegas.csv")
average_pitstop <- data %>%
  group_by(Team) %>%
  summarize(Avg_Pitstop = mean(Pitstop, na.rm = TRUE))

pp <- ggplot(average_pitstop, aes(x = reorder(Team,Avg_Pitstop), y=Avg_Pitstop, fill = Team)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 11), breaks = seq(0, 11, by = 0.5), expand = c(0, 0)) +
  scale_fill_manual(values=teamscolors)+
  labs(title = "Average Pitstop by team",
       x = "Driver",
       y = "Pitstop Time (seconds)") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        legend.position = "None") +
  labs(subtitle = "2023 Las Vegas GP", caption = "@f1.datascience")

pp
