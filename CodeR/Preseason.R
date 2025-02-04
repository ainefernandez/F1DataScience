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

andretti<-read.csv("andretti.csv")
andrettip <- ggplot(andretti, aes(x = factor(Year), y = Standings, group=Series, col = Series)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("IndyCar" = "#F91536", "Extreme E" = "#AFCF8C", "Formula E" = "#3671C6"),
                     breaks = c("IndyCar", "Extreme E", "Formula E")) +
  scale_y_reverse(limits = c(10, 1), breaks = seq(1, 10, by = 1)) +
  theme_bw() +
  labs(x = "Year", y = "Standings", title = "Andretti's Standings Across Series", subtitle = "2021-2023", caption = "@f1.datascience") +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5))

andrettip

andretti2<-read.csv("teamsf1andretti.csv")
andrettip2 <- ggplot(andretti2, aes(x = factor(Year), y = Standings, group=Team, col = Team)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values=teamscolors ) +
  scale_y_reverse(limits = c(10, 1), breaks = seq(1, 10, by = 1)) +
  theme_bw() +
  labs(x = "Year", y = "Standings", title = "Standings of Alfa Romeo, AlphaTauri, and Haas in F1", subtitle = "2021-2023", caption = "@f1.datascience") +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5))

andrettip2

ferrari<-read.csv("ferrarichampions.csv")
alonso<-ferrari[ferrari$Driver=="ALO",]
vettel<-ferrari[ferrari$Driver=="VET",]
alonso2 <- ggplot(alonso, aes(x = factor(Year), y = Standings,group=Driver)) +
  geom_line(size = 1,col="#F91536") +
  geom_point(size = 3,col="#F91536") +
  scale_y_reverse(limits = c(6, 1), breaks = seq(1, 6, by = 1)) +
  theme_bw() +
  labs(x = "Year", y = "Standings", title = "Alonso's Standings with Ferrari", subtitle = "2010-2014", caption = "@f1.datascience") +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5))

alonso2

vettel2 <- ggplot(vettel, aes(x = factor(Year), y = Standings,group=Driver)) +
  geom_line(size = 1,col="#F91536") +
  geom_point(size = 3,col="#F91536") +
  scale_y_reverse(limits = c(13, 1), breaks = seq(1, 13, by = 1)) +
  theme_bw() +
  labs(x = "Year", y = "Standings", title = "Vettel's Standings with Ferrari", subtitle = "2015-2020", caption = "@f1.datascience") +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5))

vettel2

hamilton<-read.csv("hamilton.csv")
streakplot <- ggplot(hamilton, aes(x = Q, y = reorder(Achievement, -Q) )) +
  geom_vline(xintercept = seq(25,225, by = 25), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity",fill ="#6CD3BF") +
  geom_text(aes(label = Q), hjust = -0.1, size = 4, color = "black")+
  scale_x_continuous(limits = c(0, 225), breaks = seq(0, 225, by = 25)) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Lewis Hamilton's Career with Mercedes") +
  labs(
    subtitle = "2013-2023",
    caption = "@f1.datascience"
  ) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    axis.text.y = element_text(margin = margin(r = -20)),
    axis.ticks.y = element_blank(),
    legend.title = element_text(size = 13),
    legend.text=element_text(size = 13)
  )

streakplot

hamilton2<-read.csv("hamilton2.csv")
hamilton2p <- ggplot(hamilton2, aes(x = "", y = Value, fill = Stat)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(
    aes(label = ifelse(Value > 10, paste0(Value, "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 5
  ) +
  coord_polar(theta = "y") +
  labs(
    title = "Lewis Hamilton's Career with Mercedes at a glance",
    fill = " ", subtitle = "2013-2023",
    caption = "@f1.datascience"
  ) +
  theme_void() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13)
  ) +
  scale_fill_manual(
    values = c("Points" = "#2ECC71", "Podiums" = "#3498DB", "No points" = "#E74C3C", "Wins" = "#F1C40F", "DNF/DSQ" = "purple")
  )

hamilton2p


closest<-read.csv("closestfinishes.csv")
streakplot <- ggplot(closest, aes(x=Gap, y = reorder(GP, -Gap) )) +
  geom_vline(xintercept = seq(.02,.2, by = .02), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity",fill ="#D1495B") +
  geom_text(aes(label = Gap), hjust = -0.1, size = 4, color = "black")+
  scale_x_continuous(limits = c(0, .2), breaks = seq(0, .2, by = .02)) +
  xlab("Gap") +
  ylab(" ") +
  ggtitle("Closest Finishes for the Win in F1") +
  labs(
    subtitle = " ",
    caption = "@f1.datascience"
  ) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    axis.text.y = element_text(margin = margin(r = -20)),
    axis.ticks.y = element_blank(),
    legend.title = element_text(size = 13),
    legend.text=element_text(size = 13)
  )

streakplot

circuitinfo<-read.csv("circuitinfo.csv")
streakplot <- ggplot(circuitinfo, aes(x=PitlaneTimeLoss, y = reorder(Circuit, -PitlaneTimeLoss) )) +
  geom_vline(xintercept = seq(5,30, by =5), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity",fill ="#D1495B") +
  geom_text(aes(label =paste0(PitlaneTimeLoss, "s")), hjust = -0.1, size = 4, color = "black")+
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5)) +
  xlab("Pitlane Time Loss") +
  ylab(" ") +
  ggtitle("Pitlane time loss by circuit") +
  labs(
    subtitle = "In seconds",
    caption = "@f1.datascience"
  ) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    axis.text.y = element_text(margin = margin(r = -20)),
    axis.ticks.y = element_blank(),
    legend.title = element_text(size = 13),
    legend.text=element_text(size = 13)
  )

streakplot

streakplot <- ggplot(circuitinfo, aes(x=PoleToWin, y = reorder(Circuit, -PoleToWin) )) +
  geom_vline(xintercept = seq(10,100, by =10), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity",fill ="#D1495B") +
  geom_text(aes(label =paste0(PoleToWin, "%")), hjust = -0.1, size = 3.5, color = "black")+
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  xlab("Pole To Win conversion rate") +
  ylab(" ") +
  ggtitle("Pole To Win conversion rate by circuit") +
  labs(
    subtitle = " ",
    caption = "@f1.datascience"
  ) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    axis.text.y = element_text(margin = margin(r = -30)),
    axis.ticks.y = element_blank(),
    legend.title = element_text(size = 13),
    legend.text=element_text(size = 13)
  )

streakplot


