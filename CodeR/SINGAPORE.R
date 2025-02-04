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
SAIQ <- read.csv("telSAIQSIN.csv")
LECQ <- read.csv("telLECQSIN.csv")
RUSQ <- read.csv("telRUSQSIN.csv")
SAIQ$faster <- NA

for (i in 1:nrow(SAIQ)) {
  if (!is.na(SAIQ$Speed[i]) & !is.na(RUSQ$Speed[i]) & !is.na(LECQ$Speed[i])) {
    if (SAIQ$Speed[i] > RUSQ$Speed[i] & SAIQ$Speed[i] > LECQ$Speed[i]) {
      SAIQ$faster[i] <- "Sainz is faster"
    } else if (RUSQ$Speed[i] > SAIQ$Speed[i] & RUSQ$Speed[i] > LECQ$Speed[i]) {
      SAIQ$faster[i] <- "Russell is faster"
    } else if (LECQ$Speed[i] > SAIQ$Speed[i] & LECQ$Speed[i] > RUSQ$Speed[i]) {
      SAIQ$faster[i] <- "Leclerc is faster"
    } else {
      SAIQ$faster[i] <- "Sainz is faster"
    }
  } else {
    SAIQ$faster[i] <- "Sainz is faster"
  }
}

LapDataSAISingapore<- data.frame(X = SAIQ$X, Y = SAIQ$Y, Speed=SAIQ$Speed, Faster=SAIQ$faster)
pSAIQ <- ggplot(LapDataSAISingapore, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Faster, group = Faster),
               size = 3, lineend = "round") +
  scale_color_manual(
    values = c("Leclerc is faster" = "gold", "Sainz is faster" = "#F91536", "Russell is faster" = "#6CD3BF"),
    breaks = c("Sainz is faster", "Russell is faster", "Leclerc is faster")
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
  labs(title = "SAI, RUS, and LEC Fastest Lap Comparison", subtitle = "2023 Singapore GP", caption = "@f1.datascience")

pSAIQ
gapSingaporeQ<-read.csv("GapSinagpore.csv")
pQualyS<-ggplot(gapSingaporeQ, aes(x=Gap,y=reorder(Driver,-Gap),fill=Driver))+ 
  geom_vline(xintercept = c(.2,.4,.6,.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6), linetype = "dashed", color = "grey")+
  
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, 2.8, by = .2))+
  scale_y_discrete(expand = c(0,0))+
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
  labs(caption = "@f1.datascience",subtitle = "2023 Singapore GP")+theme(legend.position = "none")
pQualyS

VERQ<-read.csv("VERQSIN.csv")
LAWQ<-read.csv("LAWQSIN.csv")
LAWQ$faster <- NA

for (i in 1:nrow(LAWQ)) {
  if (!is.na(LAWQ$Speed[i]) & !is.na(VERQ$Speed[i]) & !is.na(LECQ$Speed[i])) {
    if (VERQ$Speed[i] > LAWQ$Speed[i]) {
      LAWQ$faster[i] <- "Verstappen is faster"
    } else if (LAWQ$Speed[i] > VERQ$Speed[i]) {
      LAWQ$faster[i] <- "Lawson is faster"
    } else {
      LAWQ$faster[i] <- "Verstappen is faster"
    }
  } else {
    LAWQ$faster[i] <-"Verstappen is faster"
  }
}

LapDataLAWSingapore<- data.frame(X = LAWQ$X, Y = LAWQ$Y, Speed=LAWQ$Speed, Faster=LAWQ$faster)
pLAWQ <- ggplot(LapDataLAWSingapore, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Faster, group = Faster),
               size = 3, lineend = "round") +
  scale_color_manual(
    values = c("Lawson is faster" = "#5E8FAA", "Verstappen is faster" = "#FF004C"))+
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
  labs(title = "LAW and VER Fastest Lap Comparison", subtitle = "2023 Singapore GP", caption = "@f1.datascience")

pLAWQ

results<-read.csv("ResultsSingaporeR2023.csv")
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
  labs(title="Grid and race position",subtitle="2023 Singapore GP",caption = "@f1.datascience")
p

pitstop<-read.csv("pitsSingapore.csv")
pp <- ggplot(pitstop, aes(x = reorder(Driver,Pitstop), y =Pitstop, fill = Driver)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 0.5), expand = c(0, 0)) +
  scale_fill_manual(values=drivercolors)+
  labs(title = "Pitstop Times by Driver",
       x = "Driver",
       y = "Pitstop Time (seconds)") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        legend.position = "None") +
  labs(subtitle = "2023 Singapore GP", caption = "@f1.datascience")

pp
singapur<-read.csv("laptimesSIN.csv")
monlando<-ggplot(singapur, aes(x=LapNumber, y=LapTime, col=Driver)) +
  geom_line(size=1) +
  geom_point(size=3)+
  scale_color_manual(values=c("HAM"="#6CD3BF","NOR"="#F58020","SAI"="#F91536","RUS"="black"))+
  scale_y_continuous(limits=c(95,101),breaks=seq(95, 101, by = 1))+
  scale_x_continuous(breaks=seq(46,62, by = 1))+
  theme_bw()+
  labs(x = "Lap", y = "Lap Time (s)",title="SAI, NOR, HAM and RUS laptimes comparison",subtitle ="2023 Sinagapore GP (46-62)",caption = "@f1.datascience" )+
  theme(panel.background=element_rect(fill="white", color="white"),
        plot.background=element_rect(fill="white", color="white"),
        plot.title=element_text(size=18, hjust=0.5),
        plot.subtitle=element_text(size=15, hjust=0.5),
        plot.caption=element_text(size=11, hjust=0.5))

monlando

singapur2<-read.csv("GapLandoSIN.csv")
monlando2 <- ggplot(singapur2, aes(x = Lap, y = Gap)) +
  geom_line(size = 1, color = "#F58020") +
  geom_point(size = 3, color = "#F58020") +
  geom_hline(yintercept = 1, color = "red", linetype = "solid",size=1.5) +  # Add a red dashed line at 1 second
  scale_y_continuous(limits = c(0.5, 1.6), breaks = seq(0.5, 1.6, by = 0.1)) +
  scale_x_continuous(breaks = seq(52, 62, by = 1)) +
  theme_bw() +
  labs(x = "Lap", y = "Gap (s)", title = "Norris's gap to Sainz", subtitle = "2023 Singapore GP (52-62)", caption = "@f1.datascience") +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5))

monlando2

