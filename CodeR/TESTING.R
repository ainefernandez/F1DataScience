library(ggplot2)
library(dplyr)
library(gganimate)
setwd("/Users/ainefernandez/documents/F1DataScience")
drivercolors <- c("VER" = "#3671C6","PER" = "#3671C6","ALO" = "#358C75","SAI" = "#F91536","HAM" = "#6CD3BF",
                  "STR" = "#358C75","RUS" = "#6CD3BF","BOT" = "#04C404","GAS" = "#FF66C4","ALB" = "#37BEDD",
                  "TSU" = "#0335D3","SAR" = "#37BEDD","MAG" = "#B6BABD","DEV" = "#5E8FAA","HUL" = "#B6BABD",
                  "ZHO" = "#04C404", "NOR" = "#F58020","OCO" = "#FF66C4","LEC" = "#F91536","PIA" = "#F58020", "RIC"="#0335D3","LAW"="#5E8FAA")
teamscolors<-c("Red Bull Racing"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
               "Alpine"="#FF66C4","Haas F1 Team"="#B6BABD","RB"="#0335D3","McLaren"="#F58020","Kick Sauber"="#04C404")
NORQ <- read.csv("NORT1.csv")
VERQ <- read.csv("VERT1.csv")
SAIQ <- read.csv("SAIT1.csv")
VERQ$faster <- NA

for (i in 1:nrow(VERQ)) {
  if (!is.na(NORQ$Speed[i]) & !is.na(SAIQ$Speed[i]) & !is.na(VERQ$Speed[i])) {
    if (VERQ$Speed[i] > NORQ$Speed[i] & VERQ$Speed[i] > SAIQ$Speed[i]) {
      VERQ$faster[i] <- "Verstappen is faster"
    } else if (NORQ$Speed[i] > VERQ$Speed[i] & NORQ$Speed[i] > SAIQ$Speed[i]) {
      VERQ$faster[i] <- "Norris is faster"
    } else if (SAIQ$Speed[i] > VERQ$Speed[i] & SAIQ$Speed[i] > NORQ$Speed[i]) {
      VERQ$faster[i] <- "Sainz is faster"
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
    values = c("Verstappen is faster" = "#3671C6", "Norris is faster" = "#F58020", "Sainz is faster" = "#F91536"),
    breaks = c("Verstappen is faster", "Norris is faster", "Sainz is faster")
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
  labs(title = "VER, NOR, and SAI Fastest Lap Comparison", subtitle = "2024 Testing Day 1", caption = "@f1.datascience")

pSAIQ


gapSingaporeQ<-read.csv("fastestT1.csv")
pQualyS<-ggplot(gapSingaporeQ, aes(x=Gap,y=reorder(Driver,-Gap),fill=Driver))+ 
  geom_vline(xintercept = seq(0, 5, by = 0.5), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, 5, by = .5))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(Gap,3)), hjust = -0.1, size = 3) +
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Gap to best in Testing")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Testing Day 2")+theme(legend.position = "none")
pQualyS

library(ggplot2)

# Read data
laps <- read.csv("lapsT1.csv")

# Calculate total laps for each team
team_laps <- aggregate(LapsCount ~ Team, data = laps, FUN = sum)

# Sort teams by total laps
team_laps <- team_laps[order(team_laps$LapsCount), ]

# Reorder the 'Team' factor based on total laps
laps$Team <- factor(laps$Team, levels = team_laps$Team)

# Create the plot
mileage <- ggplot(laps, aes(x = LapsCount, y = Team, fill = Compound)) +
  geom_vline(xintercept = seq(0, 150, by = 25), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("HARD" = "#d2d2d2", "MEDIUM" = "#ffe541", "SOFT" = "#FF3333")) +
  scale_x_continuous(breaks = seq(0, 150, by = 25)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Laps") +
  ylab("Team") +
  ggtitle("Laps by compound") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -30)),
        axis.ticks.y = element_blank()) +
  labs(caption = "@f1.datascience", subtitle = "2024 Testing Day 2") +
  theme(legend.position = "none")

# Display the plot
mileage


racepaceT1<-read.csv("racepaceT1.csv")
hard<-racepaceT1[racepaceT1$Compound=="HARD",]
medium<-racepaceT1[racepaceT1$Compound=="MEDIUM",]
soft<-racepaceT1[racepaceT1$Compound=="SOFT",]



hardrp<-ggplot(hard, aes(x=Gap,y=reorder(Team,-Gap),fill=Team))+ 
  geom_vline(xintercept = seq(0, 2.5, by = 0.5), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=teamscolors)+
  scale_x_continuous(breaks = seq(0, 2.5, by = .5))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(Gap,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Race pace (Hard Compound)")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Testing Day 1")+theme(legend.position = "none")
hardrp

mediumrp<-ggplot(medium, aes(x=Gap,y=reorder(Team,-Gap),fill=Team))+ 
  geom_vline(xintercept = seq(0, 3.5, by = 0.5), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=teamscolors)+
  scale_x_continuous(breaks = seq(0, 3.5, by = .5))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(Gap,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Race pace (Medium Compound)")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Testing Day 1")+theme(legend.position = "none")
mediumrp

softrp<-ggplot(soft, aes(x=Gap,y=reorder(Team,-Gap),fill=Team))+ 
  geom_vline(xintercept = seq(0, 5, by = 0.5), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=teamscolors)+
  scale_x_continuous(breaks = seq(0, 5, by = .5))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(Gap,3)), hjust = -0.1, size = 3) +
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Race pace (Soft Compound)")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Testing Day 1")+theme(legend.position = "none")
softrp


#DAY 2

HAMQ <- read.csv("HAMT2.csv")
PERQ <- read.csv("PERT2.csv")
SAIQ <- read.csv("SAIT2.csv")
SAIQ$faster <- NA

for (i in 1:nrow(SAIQ)) {
  if (!is.na(HAMQ$Speed[i]) & !is.na(SAIQ$Speed[i]) & !is.na(PERQ$Speed[i])) {
    if (SAIQ$Speed[i] > HAMQ$Speed[i] & SAIQ$Speed[i] > PERQ$Speed[i]) {
      SAIQ$faster[i] <- "Sainz is faster"
    } else if (PERQ$Speed[i] > SAIQ$Speed[i] & PERQ$Speed[i] > HAMQ$Speed[i]) {
      SAIQ$faster[i] <- "Pérez is faster"
    } else if (HAMQ$Speed[i] > SAIQ$Speed[i] & HAMQ$Speed[i] > PERQ$Speed[i]) {
      SAIQ$faster[i] <- "Hamilton is faster"
    } else {
      SAIQ$faster[i] <- "Sainz is faster"
    }
  } else {
    SAIQ$faster[i] <- "Sainz is faster"
  }
}

LapDataVERJAP<- data.frame(X = SAIQ$X, Y = SAIQ$Y, Speed=SAIQ$Speed, Faster=SAIQ$faster)
pSAIQ <- ggplot(LapDataVERJAP, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Faster, group = Faster),
               size = 3, lineend = "round") +
  scale_color_manual(
    values = c("Pérez is faster" = "#3671C6", "Hamilton is faster" = "#6CD3BF", "Sainz is faster" = "#F91536"),
    breaks = c("Sainz is faster","Pérez is faster", "Hamilton is faster")
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
  labs(title = "SAI, PER, and HAM Fastest Lap Comparison", subtitle = "2024 Testing Day 2", caption = "@f1.datascience")

pSAIQ



laps <- read.csv("lapsT2C.csv")

# Calculate total laps for each team
team_laps <- aggregate(LapsCount ~ Team, data = laps, FUN = sum)

# Sort teams by total laps
team_laps <- team_laps[order(team_laps$LapsCount), ]

# Reorder the 'Team' factor based on total laps
laps$Team <- factor(laps$Team, levels = team_laps$Team)

# Create the plot
mileage <- ggplot(laps, aes(x = LapsCount, y = Team, fill = Compound)) +
  geom_vline(xintercept = seq(0, 150, by = 25), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("HARD" = "#d2d2d2", "MEDIUM" = "#ffe541", "SOFT" = "#FF3333")) +
  scale_x_continuous(breaks = seq(0, 150, by = 25)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Laps") +
  ylab("Team") +
  ggtitle("Laps by compound") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -30)),
        axis.ticks.y = element_blank()) +
  labs(caption = "@f1.datascience", subtitle = "2024 Testing Day 1") +
  theme(legend.position = "none")

# Display the plot
mileage

gapSingaporeQ<-read.csv("fastestT2.csv")
pQualyS<-ggplot(gapSingaporeQ, aes(x=Gap,y=reorder(Driver,-Gap),fill=Driver))+ 
  geom_vline(xintercept = seq(0, 8.5, by = 0.5), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, 8.5, by = .5))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(Gap,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Gap to best in Testing")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Testing Day 2")+theme(legend.position = "none")
pQualyS

racepaceT2<-read.csv("RacePaceT2.csv")
racepace<-ggplot(racepaceT2, aes(x=Gap,y=reorder(Team,-Gap),fill=Team))+ 
  geom_vline(xintercept = seq(0, 4, by = 0.5), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=teamscolors)+
  scale_x_continuous(breaks = seq(0, 4, by = .5))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(Gap,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Team") +
  ggtitle("Race pace")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Testing Day 2")+theme(legend.position = "none")
racepace




#Day 3 



LECQ <- read.csv("LECT3.csv")
RUSQ <- read.csv("RUST3.csv")
ZHOQ <- read.csv("RUST3.csv")
LECQ$faster <- NA

for (i in 1:nrow(LECQ)) {
  if (!is.na(LECQ$Speed[i]) & !is.na(RUSQ$Speed[i]) & !is.na(PERQ$Speed[i])) {
    if (LECQ$Speed[i] > ZHOQ$Speed[i] & LECQ$Speed[i] > RUSQ$Speed[i]) {
      LECQ$faster[i] <- "Leclerc is faster"
    } else if (RUSQ$Speed[i] > LECQ$Speed[i] & RUSQ$Speed[i] > ZHOQ$Speed[i]) {
      LECQ$faster[i] <- "Russell is faster"
    } else if (ZHOQ$Speed[i] > LECQ$Speed[i] & ZHOQ$Speed[i] > RUSQ$Speed[i]) {
      LECQ$faster[i] <- "Zhou is faster"
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
    values = c("Zhou is faster" = "#04C404", "Russell is faster" = "#6CD3BF", "Leclerc is faster" = "#F91536"),
    breaks = c("Leclerc is faster","Russell is faster","Zhou is faster" )
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
  labs(title = "SAI, PER, and HAM Fastest Lap Comparison", subtitle = "2024 Testing Day 2", caption = "@f1.datascience")

pSAIQ



laps <- read.csv("lapsT3C.csv")

# Calculate total laps for each team
team_laps <- aggregate(LapsCount ~ Team, data = laps, FUN = sum)

# Sort teams by total laps
team_laps <- team_laps[order(team_laps$LapsCount), ]

# Reorder the 'Team' factor based on total laps
laps$Team <- factor(laps$Team, levels = team_laps$Team)

# Create the plot
mileage <- ggplot(laps, aes(x = LapsCount, y = Team, fill = Compound)) +
  geom_vline(xintercept = seq(0, 175, by = 25), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("HARD" = "#d2d2d2", "MEDIUM" = "#ffe541", "SOFT" = "#FF3333","nan"="#ffe541")) +
  scale_x_continuous(breaks = seq(0, 175, by = 25)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Laps") +
  ylab("Team") +
  ggtitle("Laps by compound") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -30)),
        axis.ticks.y = element_blank()) +
  labs(caption = "@f1.datascience", subtitle = "2024 Testing Day 3") +
  theme(legend.position = "none")

# Display the plot
mileage

gapSingaporeQ<-read.csv("fastestT3.csv")
pQualyS<-ggplot(gapSingaporeQ, aes(x=Gap,y=reorder(Driver,-Gap),fill=Driver))+ 
  geom_vline(xintercept = seq(0, 7, by = 0.5), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, 7, by = .5))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(Gap,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Gap to best in Testing")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Testing Day 3")+theme(legend.position = "none")
pQualyS

racepaceT3<-read.csv("RacePaceT3.csv")
racepace<-ggplot(racepaceT3, aes(x=Gap,y=reorder(Team,-Gap),fill=Team))+ 
  geom_vline(xintercept = seq(0, 4, by = 0.5), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=teamscolors)+
  scale_x_continuous(breaks = seq(0, 4, by = .5))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(Gap,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Team") +
  ggtitle("Race pace")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Testing Day 2")+theme(legend.position = "none")
racepace

hard<-read.csv("average_laptimes_hardT3.csv")
medium<-read.csv("average_laptimes_mediumT3.csv")
soft<-read.csv("average_laptimes_softT3.csv")


hardrp<-ggplot(hard, aes(x=Gap,y=reorder(Team,-Gap),fill=Team))+ 
  geom_vline(xintercept = seq(0, 2.5, by = 0.5), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=teamscolors)+
  scale_x_continuous(breaks = seq(0, 2.5, by = .5))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(Gap,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Race pace (Hard Compound)")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Testing Day 3")+theme(legend.position = "none")
hardrp

mediumrp<-ggplot(medium, aes(x=Gap,y=reorder(Team,-Gap),fill=Team))+ 
  geom_vline(xintercept = seq(0, 4, by = 0.5), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=teamscolors)+
  scale_x_continuous(breaks = seq(0, 4, by = .5))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(Gap,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Race pace (Medium Compound)")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Testing Day 3")+theme(legend.position = "none")
mediumrp

softrp<-ggplot(soft, aes(x=Gap,y=reorder(Team,-Gap),fill=Team))+ 
  geom_vline(xintercept = seq(0, 5.5, by = 0.5), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=teamscolors)+
  scale_x_continuous(breaks = seq(0, 5.5, by = .5))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(Gap,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Race pace (Soft Compound)")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 Testing Day 3")+theme(legend.position = "none")
softrp

