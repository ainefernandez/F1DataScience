library(ggplot2)
library(dplyr)
setwd("/Users/ainefernandez/documents/F1DataScience")
VERQ<-read.csv("telVERHUN.csv")
HAMQ<-read.csv("telHAMHUN.csv")
NORQ<-read.csv("telNORHUN.csv")
HAMQ$faster <- NA
for (i in 1:nrow(HAMQ)) {
  if (!is.na(VERQ$Speed[i]) & !is.na(HAMQ$Speed[i]) & !is.na(NORQ$Speed[i])) {
    if (VERQ$Speed[i] > HAMQ$Speed[i] & VERQ$Speed[i] > NORQ$Speed[i]) {
      HAMQ$faster[i] <- "Verstappen is faster"
    } else if (HAMQ$Speed[i] > VERQ$Speed[i] & HAMQ$Speed[i] > NORQ$Speed[i]) {
      HAMQ$faster[i] <- "Hamilton is faster"
    } else if (NORQ$Speed[i] > VERQ$Speed[i] & NORQ$Speed[i] >HAMQ$Speed[i]) {
      HAMQ$faster[i] <- "Norris is faster"
    } else {
      HAMQ$faster[i] <- "Hamilton is faster"
    }
  } else {
    HAMQ$faster[i] <- "Hamilton is faster"
  }
}
LapDataHAMHUN<- data.frame(X = HAMQ$X, Y = HAMQ$Y, Speed=HAMQ$Speed, Faster=HAMQ$faster)
pVERS <- ggplot(LapDataHAMHUN, aes(x = Y, y = X)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(Y), y = lag(X), xend = Y, yend = X, color = Faster, group = Faster),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("Verstappen is faster" = "#3671C6", "Hamilton is faster" = "#6CD3BF", "Norris is faster" = "#F58020"))  +
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
  labs(title = "HAM, VER and NOR fastest lap comparison",
       subtitle = "2023 Hungarian GP",
       caption = "@f1.datascience")
pVERS



results<-read.csv("ResultsHUNR2023.csv")
left_label <- paste(results$Abbreviation, round(results$GridPosition),sep=", ")
right_label <- paste(results$Abbreviation, round(results$Position),sep=", ")
p <- ggplot(results) + 
  geom_segment(aes(x=1, xend=2, y=GridPosition, yend=Position, col=Abbreviation), size=1, show.legend=F) +
  geom_vline(xintercept=1, linetype="dashed", size=.1) +
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(values = c("VER"="#3671C6","PER"="#3671C6","ALO"="#358C75","SAI"="#F91536","HAM"="#6CD3BF","STR"="#358C75","RUS"="#6CD3BF","BOT"="#C92D4B","GAS"="#2293D1","ALB"="#37BEDD",
                                "TSU"="#5E8FAA","SAR"="#37BEDD","MAG"="#B6BABD","DEV"="#5E8FAA","HUL"="#B6BABD","ZHO"="#C92D4B","NOR"="#F58020","OCO"="#2293D1","LEC"="#F91536","PIA"="#F58020")) +
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
  labs(title="Grid and race position",subtitle="2023 Hungarian GP",caption = "@f1.datascience")
p





pointsdrivers<-read.csv("Points.csv")
pointsdrivers$GP<-factor(pointsdrivers$GP, levels = c("Hungary","Great Britain","Austria","Austria Sprint ","Canada","Spain","Monaco","Miami","Azerbaijan ",
                                                      "Azerbaijan Sprint ","Australia","Saudi Arabia","Bahrain")) 
pointsdrivers$Driver<-factor(pointsdrivers$Driver,levels=c("RIC","DEV","SAR","MAG","TSU","ZHO",
                                                           "BOT","HUL","ALB","GAS","PIA",
                                                           "OCO","STR","NOR","LEC","SAI",
                                                           "RUS","HAM","ALO","PER","VER"))
pointsplot<-ggplot(pointsdrivers,aes(reorder(Driver,Points),Points,fill=GP))+
  geom_hline(yintercept = c(20,40,60,80,100,120,140,160,180,200,220,240,260,280,300), linetype = "dashed",color = "grey")+
  geom_bar(stat="identity")+
  scale_y_continuous(limits=c(0,300),breaks = seq(0, 300, by = 20))+
  coord_flip()+
  scale_fill_manual(values = c("Hungary"="#a15360","Great Britain"="#307BC9","Austria"="#9C9EFF","Austria Sprint "="#9EBFB4", 
                               "Canada"="#ff8f00","Spain"="#AA151B","Monaco"="#CB9F18",
                               "Miami"="#FF69B4","Azerbaijan "="#00AF66","Azerbaijan Sprint "="#0092BC",
                               "Australia"="#012169","Bahrain"="#CE1126","Saudi Arabia"="#165d31"))+
  labs(title="Drivers Standings after the Hungarian GP",caption = "@f1.datascience")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title=element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust=0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -15),size=12),
        axis.ticks.y =element_blank(),
        axis.title.y=element_blank(),
        legend.text = element_text(size=14), 
        legend.title = element_text(size=14))
pointsplot




pointsteams<-read.csv("PointsTeams.csv")
pointsteams$GP<-factor(pointsteams$GP, levels = c("Hungary","Great Britain","Austria","Austria Sprint ","Canada","Spain","Monaco","Miami","Azerbaijan ",
                                                  "Azerbaijan Sprint","Australia","Saudi Arabia","Bahrain")) 
pointsteams$Team[pointsteams$Team=="Alpine "]<-"Alpine"
pointsteams$Team<-factor(pointsteams$Team,levels=c("AlphaTauri","Alfa Romeo","Haas","Williams","Alpine",
                                                   "McLaren","Ferrari","Mercedes","Aston Martin","Red Bull"))
pointsplot2<-ggplot(pointsteams,aes(reorder(Team,Points),Points,fill=GP))+
  geom_hline(yintercept = c(50,100,150,200,250,300,350,400,450,500), linetype = "dashed",color = "grey")+
  geom_bar(stat="identity")+
  scale_y_continuous(limits=c(0,500),breaks = seq(0, 500, by = 50))+
  coord_flip()+
  scale_fill_manual(values = c("Hungary"="#a15360","Great Britain"="#307BC9","Austria"="#9C9EFF","Austria Sprint "="#9EBFB4", 
                               "Canada"="#ff8f00","Spain"="#AA151B","Monaco"="#CB9F18",
                               "Miami"="#FF69B4","Azerbaijan "="#00AF66","Azerbaijan Sprint"="#0092BC",
                               "Australia"="#012169","Bahrain"="#CE1126","Saudi Arabia"="#165d31"))+
  labs(title="Constructors Standings after the Hungarian GP",caption = "@f1.datascience")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title=element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust=0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -15),size=12),
        axis.ticks.y =element_blank(),
        axis.title.y=element_blank(),
        legend.text = element_text(size=14), 
        legend.title = element_text(size=14))
pointsplot2







racepace<-ggplot(race_pace_data, aes(x = Driver, y =LapTimeSeconds, fill = Driver)) +
  geom_boxplot() +
  scale_y_continuous(limits=c(80,110),breaks = seq(80, 110, by = 2))+
  scale_fill_manual(values = c("VER"="#3671C6","PER"="#3671C6","SAI"="#F91536","LEC"="#F91536",
                               "HAM"="#6CD3BF","RUS"="#6CD3BF", "NOR"="#F58020", "PIA"="#F58020")) +
  labs(title = "Race Pace Comparison",
       x = "Driver",
       y = "LapTime (s)",
       subtitle = "2023 Hungarian GP",
       caption = "@f1.datascience") +
  theme_bw() +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "NONE")
racepace



race_pace_data<-read.csv("HUN2023.csv")
# Calculate the median race pace for each driver
driver_median_race_pace <- aggregate(LapTimeSeconds ~ Driver, data = race_pace_data, median)

# Sort the "Driver" factor levels based on median race pace
race_pace_data$Driver <- factor(race_pace_data$Driver, levels = driver_median_race_pace[order(driver_median_race_pace$`LapTimeSeconds`), "Driver"])

# Create the boxplot using ggplot
racepace <- ggplot(race_pace_data, aes(x = Driver, y = LapTimeSeconds, fill = Driver)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(80, 110), breaks = seq(80, 110, by = 2)) +
  scale_fill_manual(values = c("VER" = "#3671C6", "PER" = "#3671C6", "SAI" = "#F91536", "LEC" = "#F91536",
                               "HAM" = "#6CD3BF", "RUS" = "#6CD3BF", "NOR" = "#F58020", "PIA" = "#F58020")) +
  labs(title = "Race Pace Comparison",
       x = "Driver",
       y = "LapTime (s)",
       subtitle = "2023 Hungarian GP",
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

racepace_violin <- ggplot(race_pace_data, aes(x = Driver, y = LapTimeSeconds, fill = Driver)) +
  geom_violin() +  # Use geom_violin() instead of geom_boxplot()
  scale_y_continuous(limits = c(80, 110), breaks = seq(80, 110, by = 2)) +
  scale_fill_manual(values = c("VER" = "#3671C6", "PER" = "#3671C6", "SAI" = "#F91536", "LEC" = "#F91536",
                               "HAM" = "#6CD3BF", "RUS" = "#6CD3BF", "NOR" = "#F58020", "PIA" = "#F58020")) +
  labs(title = "Race Pace Comparison",
       x = "Driver",
       y = "LapTime (s)",
       subtitle = "2023 Hungarian GP",
       caption = "@f1.datascience") +
  theme_bw() +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "NONE")

racepace_violin



laps<-read.csv("HUN2023.csv")
lapsb<-laps[laps$LapNumber<21, ]
lapsby<-lapsb[lapsb$LapNumber>14, ]
driversa <- c( "NOR", "PIA")
lapsm <- lapsby[lapsby$Driver %in% driversa, ]       
mcl<-ggplot(lapsm, aes(x=LapNumber, y=LapTimeSeconds, col=Driver)) +
  geom_line(size=1) +
  geom_point(size=3)+
  scale_color_manual(values=c("NOR"="#F58020","PIA"="lightblue"))+
  scale_y_continuous(limits=c(83,103),breaks=seq(83, 103, by =1))+
  scale_x_continuous(breaks=seq(15,20, by = 1))+
  theme_bw()+
  labs(x = "Lap", y = "Lap Time (s)",title="NOR and PIA laptimes comparison",subtitle ="2023 Hungarian GP (15-20)",caption = "@f1.datascience" )+
  theme(panel.background=element_rect(fill="white", color="white"),
        plot.background=element_rect(fill="white", color="white"),
        plot.title=element_text(size=18, hjust=0.5),
        plot.subtitle=element_text(size=15, hjust=0.5),
        plot.caption=element_text(size=11, hjust=0.5))

mcl



gapHUNQ<-read.csv("GapHUN.csv")
pQualyS<-ggplot(gapHUNQ, aes(x=Gap,y=reorder(Driver,-Gap),fill=Team))+ 
  geom_vline(xintercept = c(0.1,0.2,0.3,0.4,0.5,0.6), linetype = "dashed", color = "grey")+
  
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
                               "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo"="#C92D4B"))+
  scale_x_continuous(breaks = seq(0, 0.6, by = 0.1))+
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
        axis.text.y = element_text(margin = margin(r = -20)),
        axis.ticks.y =element_blank())+
  labs(caption = "@f1.datascience",subtitle = "2023 Hungarian GP")+theme(legend.position = "none")
pQualyS
