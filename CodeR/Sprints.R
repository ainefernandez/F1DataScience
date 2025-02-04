library(ggplot2)
library(dplyr)
library(forcats)
library(gganimate)
setwd("/Users/ainefernandez/documents/F1DataScience")
drivercolors <- c("VER" = "#3671C6","PER" = "#3671C6","ALO" = "#358C75","SAI" = "#F91536","HAM" = "#6CD3BF",
                  "STR" = "#358C75","RUS" = "#6CD3BF","BOT" = "#C92D4B","GAS" = "#2293D1","ALB" = "#37BEDD",
                  "TSU" = "#5E8FAA","SAR" = "#37BEDD","MAG" = "#B6BABD","DEV" = "#5E8FAA","HUL" = "#B6BABD",
                  "ZHO" = "#C92D4B", "NOR" = "#F58020","OCO" = "#2293D1","LEC" = "#F91536","PIA" = "#F58020", "RIC"="#5E8FAA")
teamscolors<-c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
               "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo"="#C92D4B")

pointsSprint<-read.csv("pointsSprint.csv")
pointsSprintp <- ggplot(pointsSprint, aes(x = Points, y = reorder(Driver, Points), fill = Driver)) + 
  geom_vline(xintercept = seq(5, 45, by = 5), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Points, 2)), hjust = -0.1, size = 5, color = "black") + 
  scale_fill_manual(values = drivercolors) +
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, by = 5)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Points") +
  ylab("Driver") +
  ggtitle("Points Scored in Sprints") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -30)),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  labs(caption = "@f1.datascience", subtitle = "2023 Season")

print(pointsSprintp)



racetype<-read.csv('SprintPos.csv')
racetype$Driver<-factor(racetype$Driver,levels=c("LAW","DEV","RIC","MAG","BOT","SAR","ZHO",
                                                 "ALB","OCO","HUL","TSU","STR",
                                                 "ALO","GAS","PIA","HAM","RUS",
                                                 "LEC","NOR","SAI","PER","VER"))
racetype$Finish<-factor(racetype$Finish,levels=c("Win","Podium","Points","Out of the points","DNF"))
racetypeplot <- ggplot(racetype, aes(x = Race, y = Driver, fill = Finish)) +
  geom_bar(stat="identity",color="black") + 
  scale_x_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 1)) +
  scale_y_discrete(expand = c(0, 0))+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B")) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Sprint outcomes by driver") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 14, hjust = 0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(margin = margin(r = -11),size=13),
        axis.title = element_text(size = 14),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15)) +
  labs(caption = "@f1.datascience", subtitle = "2023 Season")

print(racetypeplot)

pointsSprintTeams<-read.csv("Sprintteampoints.csv")
pointsSprintpT <- ggplot(pointsSprintTeams, aes(x = Points, y = reorder(Team, Points), fill = Team)) + 
  geom_vline(xintercept = seq(10, 70, by = 10), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Points, 2)), hjust = -0.1, size = 5, color = "black") + 
  scale_fill_manual(values = teamscolors) +
  scale_x_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 10)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Points") +
  ylab("Team") +
  ggtitle("Points scored in Sprints") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -15)),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  labs(caption = "@f1.datascience", subtitle = "2023 Season")

print(pointsSprintpT)


qualytype<-read.csv('SprintsQUALYtype.csv')
qualytype$Driver<-factor(qualytype$Driver,levels=c("LAW","DEV","RIC","MAG","BOT","SAR","ZHO",
                                                   "ALB","OCO","HUL","TSU","STR",
                                                   "ALO","GAS","PIA","HAM","RUS",
                                                   "LEC","NOR","SAI","PER","VER"))
qualytype$Q<-factor(qualytype$Q,levels=c("SQ3","SQ2","SQ1"))
qualytypeplot <- ggplot(qualytype, aes(x = Qualy, y = Driver, fill = Q)) +
  geom_bar(stat="identity",color="black") + 
  scale_x_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 2)) +
  scale_y_discrete(expand = c(0, 0))+
  scale_fill_manual(values = c( "SQ1"="#EE232C","SQ2"="gold", "SQ3"="#7ED957")) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Sprint Shootout exits by driver") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 14, hjust = 0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(margin = margin(r = -15),size=13),
        axis.title = element_text(size = 14),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15)) +
  labs(caption = "@f1.datascience", subtitle = "Season 2023")

print(qualytypeplot)



