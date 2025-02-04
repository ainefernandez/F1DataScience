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

VERQ<-read.csv("telVERQITA.csv")
LECQ<-read.csv("telLECQITA.csv")
SAIQ<-read.csv("telSAIQITA.csv")
SAIQ$faster <- NA

for (i in 1:nrow(SAIQ)) {
  if (!is.na(SAIQ$Speed[i]) & !is.na(VERQ$Speed[i]) & !is.na(LECQ$Speed[i])) {
    if (VERQ$Speed[i] > SAIQ$Speed[i]) {
      SAIQ$faster[i] <- "Verstappen is faster"
    } else if (SAIQ$Speed[i] > VERQ$Speed[i]) {
      SAIQ$faster[i] <- "Sainz is faster"
    } else {
      SAIQ$faster[i] <- "Sainz is faster"
    }
  } else {
    SAIQ$faster[i] <-"Verstappen is faster"
  }
}
LapDataSAIitaly<- data.frame(X = SAIQ$X, Y = SAIQ$Y, Speed=SAIQ$Speed, Faster=SAIQ$faster)
pVERM<-ggplot(LapDataSAIitaly, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Faster,group=Faster),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("Verstappen is faster" = "#3671C6", "Sainz is faster" = "#F91536","Leclerc is faster"="gold"))+
  coord_flip()+coord_equal()+
  theme_classic()+theme(panel.background = element_rect(fill = "white", color = "white"),
                        plot.background = element_rect(fill = "white", color = "white"),
                        axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.title = element_blank(),
                        plot.title=element_text(size = 18, hjust = 0.5),
                        plot.subtitle=element_text(size = 15, hjust = 0.5),
                        plot.caption = element_text(size = 11, hjust=0.5),
                        legend.text=element_text(size=13),
                        legend.title=element_text(size=13))+
  
  labs(title="SAI and VER fastest lap comparison",subtitle="2023 Italian GP",caption = "@f1.datascience")
pVERM



gapMonzaQ<-read.csv("GapMonza.csv")
pQualyM<-ggplot(gapMonzaQ, aes(x=Gap,y=reorder(Driver,-Gap),fill=Driver))+ 
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
  labs(caption = "@f1.datascience",subtitle = "2023 Italian GP")+theme(legend.position = "none")
pQualyM



monzalap<-read.csv("lap_dataMonza.csv")
monzalapplot<-ggplot(monzalap, aes(x=LapNumber, y=LapTime, col=Driver)) +
  geom_line(size=1) +
  geom_point(size=3)+
  scale_color_manual(values=c("VER"="#3671C6","SAI"="#F91536"))+
  scale_y_continuous(limits=c(85.5,88.5),breaks=seq(85.5, 88.5, by = .5))+
  scale_x_continuous(breaks=seq(1,15, by = 1))+
  theme_bw()+
  labs(x = "Lap", y = "Lap Time (s)",title="VER and SAI laptimes comparison",subtitle ="2023 Italian GP (1-15)",caption = "@f1.datascience" )+
  theme(panel.background=element_rect(fill="white", color="white"),
        plot.background=element_rect(fill="white", color="white"),
        plot.title=element_text(size=18, hjust=0.5),
        plot.subtitle=element_text(size=15, hjust=0.5),
        plot.caption=element_text(size=11, hjust=0.5))
monzalapplot

speeds<-read.csv("MaxSpeedsMonza.csv")

# Assuming 'speed' is your DataFrame with 'Driver' column

# Create a vector to map drivers to their teams
driver_to_team <- c(
  NOR = "McLaren",
  PIA = "McLaren",
  HAM = "Mercedes",
  VER = "Red Bull",
  RUS = "Mercedes",
  LEC = "Ferrari",
  SAI = "Ferrari",
  PER = "Red Bull",
  STR = "Aston Martin",
  GAS = "Alpine",
  ALO = "Aston Martin",
  OCO = "Alpine",
  ALB = "Williams",
  SAR = "Williams",
  BOT = "Alfa Romeo",
  ZHO = "Alfa Romeo",
  HUL = "Haas",
  MAG = "Haas",
  TSU = "AlphaTauri",
  LAW=  "AlphaTauri"
)

# Use the 'Driver' column to map to the 'Team' column
speeds$Team <- driver_to_team[speeds$Driver]
speeds <- speeds[speeds$Driver != "TSU", ]


# Assuming you have a DataFrame 'speeds' with columns 'Team' and 'MaxSpeed'
# You should replace 'speeds' with your actual DataFrame name

# Use the aggregate function to calculate the average speed by team
average_speeds <- aggregate(MaxSpeed ~ Team, data = speeds, FUN = mean)

# Create a new dataframe to store the results
team_average_speeds <- data.frame(
  Team = average_speeds$Team,
  AverageSpeed = average_speeds$MaxSpeed
)

# Print the new dataframe
print(team_average_speeds)







speedsplot <- ggplot(team_average_speeds, aes(x = reorder(Team, AverageSpeed), y = AverageSpeed, fill = Team)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = teamscolors) +
  scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, by = 25), expand = c(0, 0)) +  # Adjust the expand argument
  xlab("Team") +
  ylab("Avg Top Speed") +
  ggtitle("Average Top Speed during the race") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  ) +
  labs(
    caption = "@f1.datascience",
    subtitle = "2023 Italian GP"
  )

# Display the plot
print(speedsplot)




results<-read.csv("ResultsMonzaR2023.csv")
results$GridPosition[results$Abbreviation=="MAG"]<-20
results$GridPosition[results$Abbreviation=="ZHO"]<-15
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
  labs(title="Grid and race position",subtitle="2023 Italian GP",caption = "@f1.datascience")
p

library(ggplot2)

lapscatter<-read.csv("lap_dataMonza2.csv")
scatter_plot <- ggplot(lapscatter, aes(x = LapNumber, y = LapTime, color = Driver)) +
  geom_point(size=3) +
  scale_y_continuous(limits = c(85.5,88.5), breaks = seq(85.5,88.5, by =.5))+
  scale_x_continuous(limits = c(35,51), breaks = seq(35,51, by =1))+
  scale_color_manual(values = drivercolors) +
  xlab("Lap Number") +
  ylab("Lap Time") +
  ggtitle("Scatter Plot of Lap Time vs. Max Speed") +
  theme_bw()+
  theme(plot.title=element_text(size = 18, hjust = 0.5),
        plot.subtitle=element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust=0.5),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13))+
  labs(title="HAM, ALB and NOR Laptimes comparison",subtitle="2023 Italian GP (35-51)",caption = "@f1.datascience")

# Display the scatter plot
print(scatter_plot)




