library(ggplot2)
library(dplyr)
library(gganimate)
setwd("/Users/ainefernandez/documents/F1DataScience")

pointsdrivers<-read.csv("Points2024.csv")
pointsdrivers$Driver<-factor(pointsdrivers$Driver,levels=c("BOT","SAR","ZHO","ALB","OCO",
                                                           "MAG","GAS","BEA","RIC","TSU",
                                                           "HUL","STR","ALO","RUS","PER",
                                                           "HAM","SAI","PIA","LEC","NOR","VER"))
pointsdrivers$GP<-factor(pointsdrivers$GP, levels = c("Belgium","Hungary","Great Britain","Austria","Spain","Canada","Monaco","Imola","Miami","China","Japan","Australia","Saudi Arabia","Bahrain"))


PointsDP <- ggplot(pointsdrivers, aes(x = Points, y = Driver, fill = GP)) + 
  geom_vline(xintercept = seq(0, 280, by = 20), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Belgium"="#DE4815","Hungary"="#49C988","Great Britain"="#AC60D4","Austria"="#00A6ED","Spain"="#D1B705","Canada"="#EA7D1C","Monaco"="#FF4455","Imola"="#009246","Miami"="#1BA3B7","China"="#EDCC10","Japan"="#ED79CC",
                               "Australia"="#00247D","Saudi Arabia"="#006C35","Bahrain" = "#CE1126"), name = "GP") +  # Define legend title and colors
  scale_x_continuous(breaks = seq(0, 280, by = 20)) +
  scale_y_discrete(expand = c(0,0)) +
  xlab("Points") +
  ylab("Driver") +
  ggtitle("2024 Drivers Standings") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -30)),
        axis.ticks.y = element_blank(),
        legend.position = "right",
        legend.text=element_text(size = 12),
        legend.title =element_text(size = 12)) +
  labs(caption = "@f1.datascience", subtitle = "Summer Break (14 races)") +
  theme(legend.position = "right")  # Define legend position

PointsDP

pointsteams<-read.csv("PointsTeams2024.csv")
pointsteams$Team<-factor(pointsteams$Team,levels=c("Kick Sauber","Williams","Alpine","Haas F1 Team","RB",
                                                       "Aston Martin","Mercedes","Ferrari","McLaren","Red Bull Racing"))
pointsteams$GP<-factor(pointsteams$GP, levels = c("Belgium","Hungary","Great Britain","Austria","Spain","Canada","Monaco","Imola","Miami","China","Japan","Australia","Saudi Arabia","Bahrain"))

PointsTP <- ggplot(pointsteams, aes(x = Points, y = reorder(Team, Points), fill = GP)) + 
  geom_vline(xintercept = seq(0, 425, by = 25), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Belgium"="#DE4815","Hungary"="#49C988","Great Britain"="#AC60D4","Austria"="#00A6ED","Spain"="#D1B705","Canada"="#EA7D1C","Monaco"="#FF4455","Imola"="#009246","Miami"="#1BA3B7","China"="#EDCC10","Japan"="#ED79CC","Australia"="#00247D","Saudi Arabia"="#006C35","Bahrain" = "#CE1126"), name = "GP") +  # Define legend title and colors
  scale_x_continuous(breaks = seq(0, 425, by = 25)) +
  scale_y_discrete(expand = c(0,0)) +
  xlab("Points") +
  ylab("Team") +
  ggtitle("2024 Constructors Standings") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -30)),
        axis.ticks.y = element_blank(),
        legend.position = "right",
        legend.text=element_text(size = 12),
        legend.title =element_text(size = 12)) +
  labs(caption = "@f1.datascience", subtitle = "Summer Break (14 races)") +
  theme(legend.position = "right")  # Define legend position

PointsTP




pointsdriversSprint<-read.csv("Points2024.csv")
pointsdriversSprint$Driver<-factor(pointsdrivers$Driver,levels=c("SAR","BOT","GAS","RIC","ZHO",
                                                           "OCO","ALB","MAG","HUL","BEA",
                                                           "TSU","STR","HAM","ALO","RUS",
                                                           "PIA","NOR","SAI","LEC","PER","VER"))
pointsdriversSprint$GP<-factor(pointsdrivers$GP, levels = c("China Sprint"))


PointsSprintD <- ggplot(pointsdriversSprint, aes(x = Points, y = reorder(Driver,Points), fill = GP)) + 
  geom_vline(xintercept = seq(0, 110, by = 10), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("China Sprint"="#EDCC10"), name = "GP") +  # Define legend title and colors
  scale_x_continuous(breaks = seq(0, 110, by = 10)) +
  scale_y_discrete(expand = c(0,0)) +
  xlab("Points") +
  ylab("Driver") +
  ggtitle("2024 Drivers Standings") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -30)),
        axis.ticks.y = element_blank(),
        legend.position = "right",
        legend.text=element_text(size = 12),
        legend.title =element_text(size = 12)) +
  labs(caption = "@f1.datascience", subtitle = "After the Chinese GP") +
  theme(legend.position = "right")  # Define legend position

PointsSprintD

pointsteams<-read.csv("PointsTeams2024.csv")
pointsteams$Team<-factor(pointsteams$Team,levels=c("Kick Sauber","Alpine","Williams","Haas F1 Team","RB",
                                                   "Aston Martin","Mercedes","McLaren","Ferrari","Red Bull Racing"))
pointsteams$GP<-factor(pointsteams$GP, levels = c("China","Japan","Australia","Saudi Arabia","Bahrain"))

PointsTP <- ggplot(pointsteams, aes(x = Points, y = reorder(Team, Points), fill = GP)) + 
  geom_vline(xintercept = seq(0, 200, by = 10), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("China"="#EDCC10","Japan"="#ED79CC","Australia"="#00247D","Saudi Arabia"="#006C35","Bahrain" = "#CE1126"), name = "GP") +  # Define legend title and colors
  scale_x_continuous(breaks = seq(0, 200, by = 10)) +
  scale_y_discrete(expand = c(0,0)) +
  xlab("Points") +
  ylab("Team") +
  ggtitle("2024 Constructors Standings") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -30)),
        axis.ticks.y = element_blank(),
        legend.position = "right",
        legend.text=element_text(size = 12),
        legend.title =element_text(size = 12)) +
  labs(caption = "@f1.datascience", subtitle = "After the Chinese GP") +
  theme(legend.position = "right")  # Define legend position

PointsTP

