library(ggplot2)
library(dplyr)
library(gganimate)
setwd("/Users/ainefernandez/documents/F1DataScience")
drivercolors <- c("VER" = "#3671C6","PER" = "#3671C6","ALO" = "#358C75","SAI" = "#F91536","HAM" = "#6CD3BF",
                  "STR" = "#358C75","RUS" = "#6CD3BF","BOT" = "#04C404","GAS" = "#FF66C4","ALB" = "#37BEDD",
                  "TSU" = "#0335D3","SAR" = "#37BEDD","MAG" = "#B6BABD","DEV" = "#5E8FAA","HUL" = "#B6BABD",
                  "ZHO" = "#04C404", "NOR" = "#F58020","OCO" = "#FF66C4","LEC" = "#F91536","PIA" = "#F58020", "RIC"="#0335D3","LAW"="#5E8FAA","BEA"="#F91536")
teamscolors<-c("Red Bull Racing"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
               "Alpine"="#FF66C4","Haas F1 Team"="#B6BABD","RB"="#0335D3","McLaren"="#F58020","Kick Sauber"="#04C404","AlphaTauri"="#5E8FAA","Alfa Romeo"="#C92D4B")


VERQ <- read.csv("RUSQUK.csv")
HAMQ <- read.csv("HAMQUK.csv")
NORQ <- read.csv("NORQUK.csv")
VERQ$faster <- NA

for (i in 1:nrow(VERQ)) {
  if (!is.na(VERQ$Speed[i]) & !is.na(NORQ$Speed[i]) & !is.na(HAMQ$Speed[i])) {
    if (HAMQ$Speed[i] > VERQ$Speed[i] & HAMQ$Speed[i] > NORQ$Speed[i]) {
      VERQ$faster[i] <- "Hamilton is faster"
    } else if (VERQ$Speed[i] > HAMQ$Speed[i] & VERQ$Speed[i] > NORQ$Speed[i]) {
      VERQ$faster[i] <- "Russell is faster"
    } else if (NORQ$Speed[i] > HAMQ$Speed[i] & NORQ$Speed[i] > VERQ$Speed[i]) {
      VERQ$faster[i] <- "Norris is faster"
    } else {
      VERQ$faster[i] <- "Russell is faster"
    }
  } else {
    VERQ$faster[i] <- "Russell is faster"
  }
}


LapDataVERJAP<- data.frame(X = VERQ$X, Y = VERQ$Y, Speed=VERQ$Speed, Faster=VERQ$faster)
pSAIQ <- ggplot(LapDataVERJAP, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Faster, group = Faster),
               size = 3, lineend = "round") +
  scale_color_manual(
    values = c("Russell is faster" = "#6CD3BF", "Norris is faster" = "#F58020", "Hamilton is faster" = "purple"),
    breaks = c("Russell is faster", "Hamilton is faster","Norris is faster" )
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
  labs(title = "RUS, HAM, and NOR fastest lap comparison", subtitle = "2024 British GP", caption = "@f1.datascience")

pSAIQ


gapSingaporeQ<-read.csv("GapUK.csv")
pQualyS<-ggplot(gapSingaporeQ, aes(x=Gap,y=reorder(Driver,-Gap),fill=Driver))+ 
  geom_vline(xintercept = seq(0, 14, by = 1), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, 14, by = 1))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(Gap,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Gap to best in Qualy")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 British GP (WET Q1)")+theme(legend.position = "none")
pQualyS




gapSingaporeQ<-read.csv("idealgapUK.csv")
gapSingaporeQ<-gapSingaporeQ[gapSingaporeQ$Driver!="BOT",]
gapSingaporeQ<-gapSingaporeQ[gapSingaporeQ$Driver!="PER",]
gapSingaporeQ<-gapSingaporeQ[gapSingaporeQ$Driver!="GAS",]
gapSingaporeQ<-gapSingaporeQ[gapSingaporeQ$Driver!="OCO",]
gapSingaporeQ<-gapSingaporeQ[gapSingaporeQ$Driver!="MAG",]

pQualyS<-ggplot(gapSingaporeQ, aes(x=Gap,y=reorder(Driver,-Gap),fill=Driver))+ 
  geom_vline(xintercept = seq(0, 2.2, by = .2), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, 2.2, by = .2))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(Gap,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Ideal Qualy Laps Gaps")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 British GP (Q1 EXCLUDED)")+theme(legend.position = "none")
pQualyS

gapSingaporeQ<-read.csv("idealgapsTUK.csv")
#gapSingaporeQ<-gapSingaporeQ[gapSingaporeQ$Team!="Alpine",]
pQualyS<-ggplot(gapSingaporeQ, aes(x=Gap,y=reorder(Team,-Gap),fill=Team))+ 
  geom_vline(xintercept = seq(0, 8, by = 1), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=teamscolors)+
  scale_x_continuous(breaks = seq(0, 8, by = 1))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(Gap,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Team") +
  ggtitle("Ideal Qualy Laps Gaps (Teams)")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 British GP")+theme(legend.position = "none")
pQualyS


gapSingaporeQ<-read.csv("bestsectorUK.csv")
gapSingaporeQ<-gapSingaporeQ[gapSingaporeQ$Driver!="BOT",]
gapSingaporeQ<-gapSingaporeQ[gapSingaporeQ$Driver!="PER",]
gapSingaporeQ<-gapSingaporeQ[gapSingaporeQ$Driver!="GAS",]
gapSingaporeQ<-gapSingaporeQ[gapSingaporeQ$Driver!="OCO",]
gapSingaporeQ<-gapSingaporeQ[gapSingaporeQ$Driver!="MAG",]
pQualyS<-ggplot(gapSingaporeQ, aes(x=GapSector1,y=reorder(Driver,-GapSector1),fill=Driver))+ 
  geom_vline(xintercept = seq(0, .45, by = 0.05), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0,.45, by = 0.05))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(GapSector1,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Gap to fastest Sector 1")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 British GP")+theme(legend.position = "none")
pQualyS


pQualyS<-ggplot(gapSingaporeQ, aes(x=GapSector2,y=reorder(Driver,-GapSector2),fill=Driver))+ 
  geom_vline(xintercept = seq(0, 1.1, by = .1), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, 1.1, by = .1))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(GapSector2,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Gap to fastest Sector 2")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 British GP")+theme(legend.position = "none")
pQualyS

pQualyS<-ggplot(gapSingaporeQ, aes(x=GapSector3,y=reorder(Driver,-GapSector3),fill=Driver))+ 
  geom_vline(xintercept = seq(0,.75, by = .05), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, .75, by = .05))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(GapSector3,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Gap To fastest Sector 3")+
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
  labs(caption = "@f1.datascience",subtitle = "2024 British GP")+theme(legend.position = "none")
pQualyS
