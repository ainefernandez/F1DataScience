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
               "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo"="#C92D4B", "Alpine/Renault"="#2293D1","AlphaTauri/Toro Rosso"="#5E8FAA")


podiums<-read.csv("lapsled.csv")
podiumsplot <- ggplot(podiums, aes(x = reorder(Driver, -Laps.Led), y = Laps.Led)) +
  geom_hline(yintercept = c(2,4,6,8,10,12,14), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity", aes(fill = Driver)) +
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 2), expand = c(0,0)) +
  scale_fill_manual(values = drivercolors) +
  xlab("Driver") +
  ylab("Laps led") +
  ggtitle("Laps led (Non Red Bull Drivers)") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text=element_text(size = 15)) +
  labs(caption = "@f1.datascience", subtitle = "After the Italian GP") +
  theme(legend.position = "none")

podiumsplot


streak<-read.csv("PointsStreak.csv")
streakplot <- ggplot(streak, aes(x = Races, y =reorder(Driver,Races), fill = Driver)) +
  geom_vline(xintercept = c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34), linetype = "dashed", color = "grey")+
  geom_bar(stat = "identity") +
  scale_fill_manual(values = drivercolors)+
  scale_x_continuous(limits=c(0,34),breaks = seq(0, 34, by = 2))+
  geom_text(aes(label = Races, vjust = 0.5, hjust = -0.2)) +
  xlab("Consecutive points scoring races") +
  ylab("Driver") +
  ggtitle("Active Points Streaks")+
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
  labs(caption = "@f1.datascience",subtitle = "After the Italian GP")+theme(legend.position = "none")

streakplot

overtakes<-read.csv("overtakes.csv")
overtakesplot<-ggplot(overtakes, aes(x = reorder(Race, -Overtakes), y = Overtakes, fill = Race)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Netherlands 2023" = "salmon", "China 2016" = "skyblue",
                               "Brazil 2012" = "skyblue","Turkey 2011"="skyblue",
                               "Canada 2011"="skyblue")) +
  scale_y_continuous(limits=c(0,200),expand=c(0,0))+
  theme_bw() +
  xlab("Race")+
  ylab("Overtakes")+
  theme(
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title=element_text(size = 18, hjust = 0.5),
        plot.subtitle=element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust=0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.ticks.y =element_blank())+
  labs(caption = "@f1.datascience",subtitle = "After the Italian GP",title="Top 5 Races with most overtakes")+theme(legend.position = "none")+
  geom_text(aes(label = Overtakes), vjust = -0.5, color = "black")
overtakesplot


pointsdh<-read.csv("pointsasia.csv")
pointsdhp<-ggplot(pointsdh, aes(x = Points, y =reorder(Team,Points), fill = Team)) +
  geom_vline(xintercept = c(10,20,30,40,50,60), linetype = "dashed", color = "grey")+
  geom_bar(stat = "identity") +
  scale_fill_manual(values = teamscolors)+
  scale_x_continuous(limits=c(0,60),breaks = seq(0, 60, by = 10))+
  geom_text(aes(label = Points, vjust = 0.5, hjust = -0.2)) +
  xlab("Points") +
  ylab("Team") +
  ggtitle("Points scored in Singapore-Japan double header")+
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
  labs(caption = "@f1.datascience",subtitle = "After the Japanese GP")+theme(legend.position = "none")

pointsdhp

podiums<-read.csv("TeamPodiums.csv")
podiumsplot <- ggplot(podiums, aes(x = reorder(Team, -Podiums), y = Podiums, fill = Team)) +
  geom_hline(yintercept = seq(100, 900, by = 100), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = teamscolors) +
  scale_y_continuous(limits = c(0, 900), breaks = seq(0, 900, by = 100),expand = c(0,0)) +
  geom_text(aes(label = Podiums), vjust = -0.5, hjust = .5) +
  xlab("Team") +
  ylab("Podium") +
  ggtitle("All-time Podiums by Current Teams") +
  labs(subtitle = "After the Qatar GP", caption = "@f1.datascience") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.position = "none")

podiumsplot

least<-read.csv("leastfinishers.csv")
leastplot <- ggplot(least, aes(x =reorder(Race,Finishers), y =Finishers, fill = Race)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("US 2005" = "salmon", "Belgium 1998"="skyblue", "Australia 2023"="skyblue", "Monaco 1996"="skyblue","Canada 2014"= "skyblue")) +
  scale_y_continuous(limits = c(0,13),breaks = seq(0,13, by = 1),expand = c(0, 0)) +
  theme_bw() +
  xlab("Race") +
  ylab("Finishers") +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    axis.ticks.y = element_blank(),
    legend.position = "None"
  ) +
  labs(caption = "@f1.datascience", title = "Top 5 Races with the least finishers") +
  geom_text(aes(label = Finishers), vjust = -0.5, color = "black")
leastplot 











