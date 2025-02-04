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

gapsTeamates<-read.csv("GapsTeamatesFULL.csv")
View(gapsTeamates)
gapsTeamates$Driver<-factor(gapsTeamates$Driver,levels=c("SAR","ALB","PER","VER","ZHO","BOT","STR","ALO",
                                                         "RUS","HAM","DEV","TSU","GAS","OCO","PIA","NOR","MAG","HUL","SAI","LEC"))
pQualyT<-ggplot(gapsTeamates, aes(x=AvgGap,y=Driver,fill=Team))+ 
  geom_vline(xintercept = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
                               "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo"="#C92D4B"))+
  scale_x_continuous(breaks = seq(0, 1.1, by = 0.1))+
  scale_y_discrete(expand = c(0,0))+
  xlab("Average gap (seconds)") +
  ylab("Driver") +
  ggtitle("Gap between teammates in Qualy")+
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
  labs(caption = "@f1.datascience",subtitle = "Mid-Season (12 races)")+theme(legend.position = "none")
pQualyT

avgposgained<-read.csv("AvgPOSMS.csv")
avgposplot <- ggplot(avgposgained, aes(Diff, reorder(Driver, Diff), fill = Diff >= 0)) +
  geom_vline(xintercept = c(-6,-5,-4,-3,-2,-1,1,2,3,4,5,6), linetype = "dashed", color = "grey")+
  geom_bar(stat = "identity",color="black") +
  geom_text(aes(label = Driver), vjust = 0.5, hjust = ifelse(avgposgained$Diff >= 0, 1.2, -0.2), color = "white", size = 4) +
  scale_fill_manual(values = c("#CF0012", "#63B234")) +
  geom_vline(xintercept = 0, color = "black", size = 1, linetype = "solid") +
  scale_x_continuous(limits = c(-6,6),breaks = seq(-6,6, by = 1))+
  theme(panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()) +
  labs(title = "Average positions gained/lost",
    caption = "@f1.datascience",
    subtitle = "Mid-Season (12 races)",
    x="Avg positions gained/lost",
    y=" ") +
  theme(legend.position = "none")


print(avgposplot)


sumposgained<-read.csv("SUMPOSMS.csv")
sumposplot <- ggplot(sumposgained, aes(Diff, reorder(Driver, Diff), fill = Diff >= 0)) +
  geom_vline(xintercept = c(-70,-60,-50,-40,-30,-20,-10,10,20,30,40,50,60,70), linetype = "dashed", color = "grey")+
  geom_bar(stat = "identity",color="black") +
  geom_text(aes(label = Driver), vjust = 0.5, hjust = ifelse(avgposgained$Diff >= 0, 1.2, -0.2), color = "white", size = 4) +
  scale_fill_manual(values = c("#CF0012", "#63B234")) +
  geom_vline(xintercept = 0, color = "black", size = 1, linetype = "solid") +
  scale_x_continuous(limits = c(-70,70),breaks = seq(-70,70, by = 10))+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(title = "Positions gained/lost",
       caption = "@f1.datascience",
       subtitle = "Mid-Season (12 races)",
       x="Positions gained/lost",
       y=" ") +
  theme(legend.position = "none")


print(sumposplot)

pointsteams<-read.csv("PointsTeams.csv")
teams <- c("Red Bull", "Ferrari", "Mercedes","Aston Martin")

champ<-pointsteams[pointsteams$Team %in% teams, ]

champplot <- ggplot(champ, aes(x = GP, y = Points, col = Team)) +
  geom_line(size = 1, aes(group = Team)) +  # Add 'aes(group = Team)' to specify the grouping
  geom_point(size = 3) +
  scale_color_manual(values = c("Red Bull" = "#3671C6", "Ferrari" = "#F91536", "Aston Martin" = "#358C75", "Mercedes" = "#6CD3BF")) +
  scale_y_continuous(limits = c(0, 44), breaks = seq(0, 44, by = 2)) +
  labs(x = "GP", y = "Points", title = "Championship Battle", subtitle = "Mid-Season (12 Races)", caption = "@f1.datascience") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5)
  )

# Print the plot
print(champplot)


avgqualypos<-read.csv("AVGQUALYMS.csv")
avgqualyplot <- ggplot(avgqualypos, aes(x = Position, y = reorder(Driver, -Position), fill = Driver)) + 
  geom_vline(xintercept = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Position, 2)), hjust = -0.1, size = 5, color = "black") +  # Add round() function for labels
  scale_fill_manual(values = drivercolors) +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 1)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Avg Qualy position") +
  ylab("Driver") +
  ggtitle("Average qualifying position") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -30)),
        axis.ticks.y = element_blank()) +
  labs(caption = "@f1.datascience", subtitle = "Mid-Season (12 races)") +
  theme(legend.position = "none")

avgqualyplot

avgracepos<-read.csv("AVGRaceMS.csv")
avgraceplot <- ggplot(avgracepos, aes(x = Position, y = reorder(Driver, -Position), fill = Driver)) + 
  geom_vline(xintercept = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Position, 2)), hjust = -0.1, size = 5, color = "black") +  # Add round() function for labels
  scale_fill_manual(values = drivercolors) +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 1)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Avg finishing position") +
  ylab("Driver") +
  ggtitle("Average race finishing position") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -30)),
        axis.ticks.y = element_blank()) +
  labs(caption = "@f1.datascience", subtitle = "Mid-Season (12 races)") +
  theme(legend.position = "none")

avgraceplot

avgpoints<-read.csv("AVGPointsMS.csv")
avgpoints<-avgpoints[avgpoints$Driver!="nan",]
avgpointsplot <- ggplot(avgpoints, aes(x = Points, y = reorder(Driver, Points), fill = Driver)) + 
  geom_vline(xintercept = c(2,4,6,8,10,12,14,16,18,20,22,24,26,28), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Points, 2)), hjust = -0.1, size = 5, color = "black") +  # Add round() function for labels
  scale_fill_manual(values = drivercolors) +
  scale_x_continuous(limits = c(0, 28), breaks = seq(0, 28, by = 2)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Avg points per GP") +
  ylab("Driver") +
  ggtitle("Average points per GP") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -30)),
        axis.ticks.y = element_blank()) +
  labs(caption = "@f1.datascience", subtitle = "Mid-Season (12 races)") +
  theme(legend.position = "none")

avgpointsplot


podiums<-read.csv("Podiums.csv")
podiums[podiums$Team == "Ferrari ", "Team"] <- "Ferrari"
podiumsplot <- ggplot(podiums, aes(x = reorder(Team, -Podiums), y = Podiums)) +
  geom_hline(yintercept = c(2,4,6,8,10,12,14,16,18,20), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity", aes(fill = Team)) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 2)) +
  scale_fill_manual(values = teamscolors) +
  xlab(" ") +
  ylab("Podiums") +
  ggtitle("Podiums by team") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text.x = element_blank()) +
  labs(caption = "@f1.datascience", subtitle = "Mid-Season (12 races)") +
  theme(legend.position = "none")

podiumsplot


podiumsdrivers<-read.csv("PodiumsMS.csv")
podiumsdrivers$Driver<-factor(podiumsdrivers$Driver,levels=c("OCO","RUS","NOR","LEC","HAM","ALO","PER","VER"))
podiumsdriversplot <- ggplot(podiumsdrivers, aes(x = Podiums, y = reorder(Driver, Podiums), fill = Position)) +
  geom_vline(xintercept = c(2,4,6,8,10,12), linetype = "dashed", color = "grey") +
  geom_bar(stat="identity",color="black") + 
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 2)) +
  scale_y_discrete(expand = c(0, 0))+
  scale_fill_manual(values = c( "gold","#C0C0C0", "#CD7F32")) +
  xlab("Podiums") +
  ylab("Driver") +
  ggtitle("Podiums by driver") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 14, hjust = 0.5),
        axis.ticks.y=element_blank(),
        axis.text.y = element_text(margin = margin(r = -25),size=13),
        axis.title = element_text(size = 14),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15)) +
  labs(caption = "@f1.datascience", subtitle = "Mid-Season (12 races)")

print(podiumsdriversplot)

dnfs<-read.csv("DNFSMS.csv")
dnfs$Driver<-factor(dnfs$Driver,levels=c("HUL","ZHO","SAI","ALB","DEV",
                                                             "PIA","LEC","STR","RUS","SAR",
                                                             "GAS","MAG","OCO"))
dnfsplot<-ggplot(dnfs, aes(x = DNF, y = reorder(Driver, DNF), fill = Type))+
  geom_vline(xintercept = c(2,4), linetype = "dashed", color = "grey") +
  geom_bar(stat="identity",color="black") + 
  scale_x_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 2)) +
  scale_y_discrete(expand = c(0, 0))+
  scale_fill_manual(values = c("#007AC3","#EF5080")) +
  xlab("DNF's") +
  ylab("Driver") +
  ggtitle("DNF's by driver") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 14, hjust = 0.5),
        axis.ticks.y=element_blank(),
        axis.text.y = element_text(margin = margin(r = -25),size=13),
        axis.title = element_text(size = 14),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15)) +
  labs(caption = "@f1.datascience", subtitle = "Mid-Season (12 races)")
dnfsplot

dnfs  

percentage<-read.csv("pointspercentage.csv")
percentage$Driver<-factor(percentage$Driver,levels=c("RIC","DEV","TSU","ZHO","BOT","MAG","HUL","SAR","ALB",
                                                     "GAS","OCO","PIA","NOR","SAI","LEC","STR","ALO","RUS","HAM","PER","VER"))
plotpercentage <- ggplot(percentage, aes(x = Driver, y = Percentage, fill = Team)) +
  geom_hline(yintercept = c(10,20,30,40,50,60,70,80,90, 100), linetype = "dashed", color = "grey")+
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
                               "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo"="#C92D4B"))+
  scale_y_continuous(limits=c(0,100),breaks = seq(0, 100, by = 10))+
  geom_text(aes(label = paste0(Percentage, "%")), vjust = 0.5, hjust = -0.2) +
  coord_flip()+
  xlab("Driver") +
  ylab("Percentage") +
  ggtitle("Team points by driver")+
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
  labs(caption = "@f1.datascience",subtitle = "Mid-Season (12 Races)")+theme(legend.position = "none")

plotpercentage

qualytype<-read.csv('MIDSEASONQUALYtype.csv')
qualytype$Driver<-factor(qualytype$Driver,levels=c("RIC","DEV","SAR","MAG","TSU","ZHO",
                                                   "BOT","HUL","ALB","GAS","PIA",
                                                   "OCO","STR","NOR","SAI","RUS",
                                                   "LEC","HAM","ALO","PER","VER"))
qualytype$Q<-factor(qualytype$Q,levels=c("Q3","Q2","Q1"))
qualytypeplot <- ggplot(qualytype, aes(x = Qualy, y = Driver, fill = Q)) +
  geom_bar(stat="identity",color="black") + 
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 2)) +
  scale_y_discrete(expand = c(0, 0))+
  scale_fill_manual(values = c( "Q1"="#EE232C","Q2"="gold", "Q3"="#7ED957")) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Qualifying exits by driver") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 14, hjust = 0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(margin = margin(r = -25),size=13),
        axis.title = element_text(size = 14),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15)) +
  labs(caption = "@f1.datascience", subtitle = "Mid-Season (12 races)")

print(qualytypeplot)


racetype<-read.csv('MIDSEASONRT.csv')
racetype$Driver<-factor(racetype$Driver,levels=c("RIC","DEV","SAR","MAG","TSU","ZHO",
                                                   "BOT","HUL","ALB","GAS","PIA",
                                                   "OCO","STR","NOR","SAI","RUS",
                                                   "LEC","HAM","ALO","PER","VER"))
racetype$Finish<-factor(racetype$Finish,levels=c("Win","Podium","Points","Out of the points","DNF"))
racetypeplot <- ggplot(racetype, aes(x = Race, y = Driver, fill = Finish)) +
  geom_bar(stat="identity",color="black") + 
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 2)) +
  scale_y_discrete(expand = c(0, 0))+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B")) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Race outcomes by driver") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 14, hjust = 0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(margin = margin(r = -25),size=13),
        axis.title = element_text(size = 14),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15)) +
  labs(caption = "@f1.datascience", subtitle = "Mid-Season (12 races)")

print(racetypeplot)







diffpoints<-read.csv("diffpoints.csv")
diffpoints<-na.omit(diffpoints)
diffpoints$Driver<-factor(diffpoints$Driver,levels=c("MAG","TSU","ZHO",
                                                 "BOT","ALB","GAS",
                                                 "OCO","STR","NOR","SAI","RUS",
                                                 "LEC","HAM","ALO","PER","VER"))

diffpointsplot <- ggplot(diffpoints, aes(Diff, Driver, fill = Diff >= 0)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = Diff), vjust = 0.5, hjust = ifelse(diffpoints$Diff >= 0, 1.2, -0.2), color = "white", size = 5) +
  scale_fill_manual(values = c("#CF0012", "#63B234")) +
  geom_vline(xintercept = 0, color = "black", size = 1, linetype = "solid") +
  scale_x_continuous(limits = c(-125, 125), breaks = seq(-125, 125, by = 25)) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 13),
    axis.ticks.y = element_blank()
  ) +
  labs(
    title = "Points difference 2022 vs 2023 by driver",
    caption = "@f1.datascience",
    subtitle = "Mid-Season (12 races)",
    x = "Points difference",
    y = " "
  ) +
  theme(legend.position = "none")


# Print the modified plot
print(diffpointsplot)



diffpointsteams<-read.csv("diffpointsteams.csv")
diffpointsteams<-na.omit(diffpointsteams)
diffpointsteams$Team<-factor(diffpointsteams$Team,levels=c("AlphaTauri","Alfa Romeo","Haas",
                                                     "Williams","Alpine","McLaren",
                                                     "Ferrari","Aston Martin","Mercedes","Red Bull"))
diffpointsplotteams <- ggplot(diffpointsteams, aes(Diff, Team, fill = Diff >= 0)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = Diff), vjust = 0.5, hjust = ifelse(diffpoints$Diff >= 0, 1.2, -0.2), color = "white", size = 5) +
  scale_fill_manual(values = c("#CF0012", "#63B234")) +
  geom_vline(xintercept = 0, color = "black", size = 1, linetype = "solid") +
  scale_x_continuous(limits = c(-200, 200), breaks = seq(-200, 200, by = 25)) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 13),
    axis.ticks.y = element_blank()
  ) +
  labs(
    title = "Points difference 2022 vs 2023 by team",
    caption = "@f1.datascience",
    subtitle = "Mid-Season (12 races)",
    x = "Points difference",
    y = " "
  ) +
  theme(legend.position = "none")


print(diffpointsplotteams)


pointssum<-read.csv("PointsSUM.csv")
pointssum$GP<-factor(pointssum$GP, levels = c("Bahrain","Saudi Arabia","Australia","Azerbaijan Sprint ","Azerbaijan ",
                                              "Miami","Monaco","Spain","Canada","Austria Sprint ","Austria","Great Britain",
                                              "Hungary","Belgium Sprint","Belgium"))
pointssum$Driver<-factor(pointssum$Driver, levels = c("VER","PER","ALO","HAM","LEC",
                                                  "RUS","SAI","NOR","STR","OCO"))

drivers_to_select <- c('NOR', 'VER', 'PER', 'ALO', 'STR', 'RUS', 'HAM', 'SAI', 'LEC', 'OCO')
top10<-pointssum %>%
  filter(Driver %in% drivers_to_select)


mcl2 <- ggplot(top10, aes(x = GP, y = Points, color = Driver,group = Driver)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values =c("VER"="#3671C6","PER"="#4CA2FA","ALO"="#358C75","HAM" = "#6CD3BF","LEC" = "#F91536",
                                 "RUS"="#11AFAF","SAI"="#EF8181","NOR"="#F58020","STR"="#CFCE48","OCO"="#2293D1")) +
  scale_y_continuous(limits = c(0, 315), breaks = seq(0, 315, by = 15)) +
  theme_bw() +
  labs(
    x = " ",
    y = "Points",
    title = "Top 10 battle",
    subtitle = "Mid-Season (12 races)",
    caption = "@f1.datascience"
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    plot.caption = element_text(size = 14, hjust = 0.5),
    axis.text.x  = element_text(angle = 45, hjust = 1,size = 12),
    axis.text.y = element_text(size=12)
  )

print(mcl2)


laps_raced <- read.csv("LapsRacedMS.csv")
laps_raced <- laps_raced[laps_raced$Driver != "nan", ]
laps_racedplot<- ggplot(laps_raced, aes(x = Percentage, y = reorder(Driver,Laps), fill = Driver)) +
  geom_vline(xintercept = c(10,20,30,40,50,60,70,80,90,100), linetype = "dashed", color = "grey")+
  geom_bar(stat = "identity") +
  scale_fill_manual(values = drivercolors)+
  scale_x_continuous(limits=c(0,100),breaks = seq(0, 100, by = 10))+
  geom_text(aes(label = paste0(round(Percentage,2), "%")), vjust = 0.5, hjust = -0.2,size=3) +
  xlab("Percentage of laps raced (Sprints included)") +
  ylab("Driver") +
  ggtitle("Laps raced by driver (percentage)")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title=element_text(size = 18, hjust = 0.5),
        plot.subtitle=element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 13, hjust=0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -30)),
        axis.ticks.y =element_blank())+
  labs(caption = "@f1.datascience",subtitle = "Mid-Season (12 Races)")+theme(legend.position = "none")

laps_racedplot








laptype <- read.csv("LapType.csv")
laptype$Track.Status <- factor(laptype$Track.Status, levels = c("Red Flag", "SC/VSC", "Yellow Flag", "Green Flag"))


lapstypeplot <- ggplot(laptype, aes(x = Laps, y = reorder(GP, -Laps), fill = Track.Status)) +
  geom_vline(xintercept = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Green Flag" = "#78D07E", "Yellow Flag" = "gold", "SC/VSC" = "orange", "Red Flag" = "#E55B4A")) +
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 5)) +
  xlab("Laps") +
  ylab("GP") +
  ggtitle("Laps by Track Status") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 13, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -20)),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15)) +
  labs(caption = "@f1.datascience", subtitle = "Mid-Season (12 Races)") +
  theme(legend.position = "right") +
  labs(fill = "Track Status")  # Change the legend title

lapstypeplot







library(ggplot2)

# Read the fastestpit data
fastestpit <- read.csv("fastestpit.csv")

# Create the fastestpitplot
fastestpitplot <- ggplot(fastestpit, aes(x = FastestPit, y = reorder(GP, FastestPit), fill = TeamFP)) +
  geom_vline(xintercept = c(.2, .4, .6, .8, 1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(FastestPit, "s")), vjust = -.5, hjust = .5,size=4.5) +
  scale_fill_manual(values = teamscolors) +
  scale_x_continuous(limits = c(0, 2.4), breaks = seq(0, 2.4, by = .2),expand = c(0,0)) +
  xlab("Time (s)") +
  ylab("GP") +
  ggtitle("Fastest pitstops of the season") +
  coord_flip()+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 13, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.ticks.y = element_blank()) +
  labs(caption = "@f1.datascience", subtitle = "Mid-Season (12 Races)") +
  theme(legend.position = "none")

fastestpitplot

gapplot <- ggplot(fastestpit, aes(x = Gap.to.first, y = reorder(GP, -Gap.to.first), fill = DriverSecond)) +
  geom_vline(xintercept = c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label =DriverSecond),vjust = 0.5, hjust = -0.2,size=4) +
  scale_fill_manual(values = drivercolors) +
  scale_x_continuous(limits = c(0, 34), breaks = seq(0, 34, by = 2)) +
  xlab("Time (s)") +
  ylab("GP") +
  ggtitle("Race Finishing Gap to First Place") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 13, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text.y = element_text(margin = margin(r = -20)),
        axis.text = element_text(size = 12),
        axis.ticks.y = element_blank()) +
  labs(caption = "@f1.datascience", subtitle = "Mid-Season (12 Races)") +
  theme(legend.position = "none")

gapplot

avgpole<-read.csv("AvgGaptopole.csv")
avgpole<-avgpole[avgpole$Driver != "RIC", ]
poleplot <- ggplot(avgpole, aes(x = Gap, y = reorder(Driver, -Gap), fill = Driver)) +
  geom_vline(xintercept = c(.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = drivercolors) +
  scale_x_continuous(limits = c(0, 7), breaks = seq(0, 7, by = .5)) +
  xlab("Avg gap (s)") +
  ylab("Driver") +
  ggtitle("Average gap to pole") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 13, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text.y = element_text(margin = margin(r = -20)),
        axis.text = element_text(size = 12),
        axis.ticks.y = element_blank()) +
  labs(caption = "@f1.datascience", subtitle = "Mid-Season (12 Races)") +
  theme(legend.position = "none")

poleplot

secondfastest<-read.csv("SecondfastestMS.csv")
secondplot <- ggplot(secondfastest, aes(x = Gap, y = reorder(GP, -Gap), fill = Driver)) +
  geom_vline(xintercept = c(.05,.1,.15,.2,.25,.3,.35,.4,.45,.5,.55,.6,.65,.70,.75,.8,.85), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label =Driver),vjust = 0.5, hjust = -0.2,size=4) +
  scale_fill_manual(values = drivercolors) +
  scale_x_continuous(limits = c(0, .85), breaks = seq(0, .85, by = .05)) +
  xlab("Gap (s)") +
  ylab("GP") +
  ggtitle("Second place gap to pole") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 13, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text.y = element_text(margin = margin(r = -20)),
        axis.text = element_text(size = 12),
        axis.ticks.y = element_blank()) +
  labs(caption = "@f1.datascience", subtitle = "Mid-Season (12 Races)") +
  theme(legend.position = "none")

secondplot

teampointssum<-read.csv("teampointssum.csv")
teampointssum$Team[teampointssum$Team == "Alpine "] <- "Alpine"
teampointssum$GP<-factor(teampointssum$GP, levels = c("Bahrain","Saudi Arabia","Australia","Azerbaijan Sprint","Azerbaijan ",
                                              "Miami","Monaco","Spain","Canada","Austria Sprint ","Austria","Great Britain",
                                              "Hungary","Belgium Sprint","Belgium"))
teampointssum$Team<-factor(teampointssum$Team, levels = c("Red Bull","Mercedes","Aston Martin","Ferrari","McLaren",
                                                      "Alpine","Williams","Haas","Alfa Romeo","AlphaTauri"))
teamsumplot <- ggplot(teampointssum, aes(x = GP, y = Points, color = Team,group = Team)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values =teamscolors) +
  scale_y_continuous(limits = c(0, 520), breaks = seq(0, 520, by = 20)) +
  theme_bw() +
  labs(
    x = " ",
    y = "Points",
    title = "Team points evolution",
    subtitle = "Mid-Season (12 races)",
    caption = "@f1.datascience"
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    plot.caption = element_text(size = 14, hjust = 0.5),
    axis.text.x  = element_text(angle = 45, hjust = 1,size = 12),
    axis.text.y = element_text(size=12)
  )

print(teamsumplot)

avgpitstop<-read.csv("avgpitstop.csv")
avgpitstopplot <- ggplot(avgpitstop, aes(x = AvgFP, y = reorder(Team, AvgFP), fill = Team)) +
  geom_vline(xintercept = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = teamscolors) +
  scale_x_continuous(limits = c(0, 3.5), breaks = seq(0, 3.5, by = 0.5),expand=c(0,0)) +
  xlab("Time (s)") +
  ylab("Team") +
  ggtitle("Average fastest pitstop by team") +
  coord_flip() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 13, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.ticks.y = element_blank()) +
  labs(caption = "@f1.datascience", subtitle = "Mid-Season (12 Races)") +
  theme(legend.position = "none")

avgpitstopplot

