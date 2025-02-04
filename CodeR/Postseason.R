library(ggplot2)
library(dplyr)
library(gganimate)
setwd("/Users/ainefernandez/documents/F1DataScience")
drivercolors <- c("VER" = "#3671C6","PER" = "#3671C6","ALO" = "#358C75","SAI" = "#F91536","HAM" = "#6CD3BF",
                  "STR" = "#358C75","RUS" = "#6CD3BF","BOT" = "#C92D4B","GAS" = "#2293D1","ALB" = "#37BEDD",
                  "TSU" = "#5E8FAA","SAR" = "#37BEDD","MAG" = "#B6BABD","DEV" = "#5E8FAA","HUL" = "#B6BABD",
                  "ZHO" = "#C92D4B", "NOR" = "#F58020","OCO" = "#2293D1","LEC" = "#F91536","PIA" = "#F58020", "RIC"="#5E8FAA","LAW"="#5E8FAA")

drivercolors2022 <- c("VER" = "#3671C6","PER" = "#3671C6","ALO" = "#2293D1","SAI" = "#F91536","HAM" = "#6CD3BF",
                  "STR" = "#358C75","RUS" = "#6CD3BF","BOT" = "#C92D4B","GAS" = "#5E8FAA","ALB" = "#37BEDD",
                  "TSU" = "#5E8FAA","SAR" = "#37BEDD","MAG" = "#B6BABD","DEV" = "#5E8FAA","HUL" = "#B6BABD",
                  "ZHO" = "#C92D4B", "NOR" = "#F58020","OCO" = "#2293D1","LEC" = "#F91536","PIA" = "#F58020", "RIC"="#F58020","LAW"="#5E8FAA")

teamscolors<-c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
               "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo"="#C92D4B")

qualytype<-read.csv('FSQUALYtype.csv')
qualytype <- qualytype[qualytype$Driver != 'DEV', ]
qualytype <- na.omit(qualytype)
qualytype$Driver<-factor(qualytype$Driver,levels=c("RIC","DEV","SAR","MAG","TSU","ZHO",
                                                   "BOT","HUL","ALB","GAS","PIA",
                                                   "OCO","STR","NOR","SAI","RUS",
                                                   "LEC","HAM","ALO","PER","VER"))

qualytype$Q<-factor(qualytype$Q,levels=c("Q3","Q2","Q1"))
qualytypeplot <- ggplot(qualytype, aes(x = Qualy, y = Driver, fill = Q)) +
  geom_bar(stat="identity",color="black") + 
  scale_x_continuous(limits = c(0, 22), breaks = seq(0, 22, by = 2)) +
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


diffpoints<-read.csv("pointsdiffdriversFS.csv")
diffpoints<-na.omit(diffpoints)
diffpoints$Driver<-factor(diffpoints$Driver,levels=c("LEC","RUS",
                                                     "SAI","BOT", "OCO","RIC",
                                                     "MAG","PER","HAM","ZHO","TSU","ALB",
                                                     "GAS","STR","NOR","VER","ALO"))

diffpointsplot <- ggplot(diffpoints, aes(Gap, Driver, fill = Gap >= 0)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = Gap), vjust = 0.5, hjust = ifelse(diffpoints$Gap >= 0, 1.2, -0.2), color = "white", size = 5) +
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
    subtitle = "Full season",
    x = "Points difference",
    y = " "
  ) +
  theme(legend.position = "none")


# Print the modified plot
print(diffpointsplot)



diffpointsteams<-read.csv("pointsdiffteamsFS.csv")
diffpointsteams<-na.omit(diffpointsteams)
diffpointsteams$Team<-factor(diffpointsteams$Team,levels=c("Ferrari",
                                                           "Mercedes","Alpine","Alfa Romeo","Haas",
                                                           "AlphaTauri","Williams","Red Bull","McLaren","Aston Martin"))
diffpointsplotteams <- ggplot(diffpointsteams, aes(Gap, Team, fill = Gap >= 0)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = Gap), vjust = 0.5, hjust = ifelse(diffpointsteams$Gap >= 0, 1.2, -0.2), color = "white", size = 5) +
  scale_fill_manual(values = c("#CF0012", "#63B234")) +
  geom_vline(xintercept = 0, color = "black", size = .5, linetype = "solid") +
  scale_x_continuous(limits = c(-250, 250), breaks = seq(-250, 250, by = 25)) +
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
    subtitle = "2023 Season",
    x = "Points difference",
    y = " "
  ) +
  theme(legend.position = "none")

print(diffpointsplotteams)


podiumsdrivers<-read.csv("PodiumsFS.csv")
podiumsdrivers$Driver<-factor(podiumsdrivers$Driver,levels=c("GAS","OCO","RUS","PIA","SAI","HAM","LEC","NOR","ALO","PER","VER"))
podiumsdriversplot <- ggplot(podiumsdrivers, aes(x = Podiums, y = reorder(Driver, Podiums), fill = Position)) +
  geom_vline(xintercept = c(2,4,6,8,10,12,14,16,18,20,22), linetype = "dashed", color = "grey") +
  geom_bar(stat="identity",color="black") + 
  scale_x_continuous(limits = c(0, 22), breaks = seq(0, 22, by = 2)) +
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
  labs(caption = "@f1.datascience", subtitle = "2023 Season")

print(podiumsdriversplot)



percentage<-read.csv("pointsdiffdriversFS.csv")
percentage$Driver<-factor(percentage$Driver,levels=c("MAG","HUL","ZHO","BOT","LAW","RIC","TSU","SAR","ALB",
                                                     "OCO","GAS","STR","ALO","PIA","NOR","SAI","LEC","RUS","HAM","PER","VER"))
plotpercentage <- ggplot(percentage, aes(x = Driver, y = Percentage, fill = Driver)) +
  geom_hline(yintercept = c(10,20,30,40,50,60,70,80,90, 100), linetype = "dashed", color = "grey")+
  geom_bar(stat = "identity") +
  scale_fill_manual(values =drivercolors )+
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
  labs(caption = "@f1.datascience",subtitle = "2023 Season")+theme(legend.position = "none")

plotpercentage

podiums<-read.csv("podiumsteamsFS.csv")
podiums[podiums$Team == "Ferrari ", "Team"] <- "Ferrari"
podiumsplot <- ggplot(podiums, aes(x = reorder(Team, -Podiums), y = Podiums)) +
  geom_hline(yintercept = c(5,10,15,20,25,30), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity", aes(fill = Team)) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5)) +
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
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank()) +
  labs(caption = "@f1.datascience", subtitle = "2023 Season") +
  theme(legend.position = "none")

podiumsplot

avgracepos<-read.csv("FSAVGRace.csv")
avgracepos<-na.omit(avgracepos)
avgracepos<-avgracepos[avgracepos$Driver!="nan",]
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
  labs(caption = "@f1.datascience", subtitle = "2023 Season") +
  theme(legend.position = "none")

avgraceplot


racetype<-read.csv('FSRaceType.csv')

racetype$Driver<-factor(racetype$Driver,levels=c("LAW","RIC","DEV","SAR","MAG","ZHO","HUL",
                                                   "BOT","TSU","ALB","OCO","GAS",
                                                   "STR","PIA","RUS","SAI","NOR",
                                                   "LEC","ALO","HAM","PER","VER"))
racetype<-na.omit(racetype)
racetype$Finish<-factor(racetype$Finish,levels=c("Win","Podium","Points","Out of the points","DNF","DNS","DSQ"))

racetypeplot <- ggplot(racetype, aes(x = Race, y = Driver, fill = Finish)) +
  geom_bar(stat="identity",color="black") + 
  scale_x_continuous(limits = c(0, 22), breaks = seq(0, 22, by = 1)) +
  scale_y_discrete(expand = c(0, 0))+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B","DNS"="#9655A6","DSQ"="black")) +
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
  labs(caption = "@f1.datascience", subtitle = "2023 Season")

print(racetypeplot)

avgqualypos<-read.csv("FSAVGQUALY.csv")
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
  labs(caption = "@f1.datascience", subtitle = "2023 Season") +
  theme(legend.position = "none")

avgqualyplot

qualytype<-read.csv('FSQUALYtype.csv')
qualytype$Driver<-factor(qualytype$Driver,levels=c("LAW","RIC","DEV","SAR","MAG","ZHO","HUL",
                                                  "BOT","TSU","ALB","OCO","GAS",
                                                  "STR","PIA","RUS","SAI","NOR",
                                                  "LEC","ALO","HAM","PER","VER"))
qualytype$Q<-factor(qualytype$Q,levels=c("Q3","Q2","Q1"))
qualytypeplot <- ggplot(qualytype, aes(x = Qualy, y = Driver, fill = Q)) +
  geom_bar(stat="identity",color="black") + 
  scale_x_continuous(limits = c(0, 23), breaks = seq(0, 23, by = 2)) +
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
  labs(caption = "@f1.datascience", subtitle = "2023 Season")

print(qualytypeplot)


pointsteams<-read.csv('MclarenAston.csv')
# Assuming GP is a factor variable
pointsteams$GP <- factor(pointsteams$GP, levels = c("Bahrain", "Saudi Arabia","Australia","Azerbaijan","Miami",
                                                    "Monaco","Spain","Canada","Austria","Great Britain","Hungary", 
                                                    "Belgium","Netherlands","Italy","Singapore","Japan","Qatar",
                                                    "Austin","Mexico","Brazil","Las Vegas", "Abu Dhabi"))

# Angle of x-axis labels
angle <- 45  # Adjust the angle as needed

# Rest of your code remains unchanged
champplot <- ggplot(pointsteams, aes(x = GP, y = Points, col = Team)) +
  geom_line(size = 1, aes(group = Team)) +
  geom_point(size = 3) +
  scale_color_manual(values = teamscolors) +
  scale_y_continuous(limits = c(0, 325), breaks = seq(0, 325, by = 25)) +
  labs(x = " ", y = "Points", title = "McLaren vs Aston Martin", subtitle = "2023 Season", caption = "@f1.datascience") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    axis.text.x = element_blank()
  )

# Print the plot
print(champplot)

MaxVer<-racetype[racetype$Driver=="VER",]
MaxVer2 <-MaxVer  %>%
  group_by(Finish) %>%
  summarise(count = n())
Maxplot<-ggplot(MaxVer2, aes(x = "", y = count, fill = Finish)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  ggtitle("Max Verstappen's Season") +
  geom_text(aes(label = sprintf("%.1f%%", (count / sum(count)) * 100)),
            position = position_stack(vjust = 0.5),size=4.5)+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B","DNS"="#9655A6","DSQ"="black"))

Maxplot

Checo<-racetype[racetype$Driver=="PER",]
Checo2 <-Checo  %>%
  group_by(Finish) %>%
  summarise(count = n())

checoplot<-ggplot(Checo2, aes(x = "", y = count, fill = Finish)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  ggtitle("Max Verstappen's Season") +
  geom_text(aes(label = sprintf("%.1f%%", (count / sum(count)) * 100)),
            position = position_stack(vjust = 0.5),size=4.5)+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B","DNS"="#9655A6","DSQ"="black"))

checoplot



Ham<-racetype[racetype$Driver=="HAM",]
Ham2 <-Ham  %>%
  group_by(Finish) %>%
  summarise(count = n())

Hamplot<-ggplot(Ham2, aes(x = "", y = count, fill = Finish)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  ggtitle("Max Verstappen's Season") +
  geom_text(aes(label = sprintf("%.1f%%", (count / sum(count)) * 100)),
            position = position_stack(vjust = 0.5),size=4.5)+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B","DNS"="#9655A6","DSQ"="gray"))

Hamplot


Rus<-racetype[racetype$Driver=="RUS",]
Rus2 <-Rus  %>%
  group_by(Finish) %>%
  summarise(count = n())

Hamplot<-ggplot(Rus2, aes(x = "", y = count, fill = Finish)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  ggtitle("Max Verstappen's Season") +
  geom_text(aes(label = sprintf("%.1f%%", (count / sum(count)) * 100)),
            position = position_stack(vjust = 0.5),size=4.5)+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B","DNS"="#9655A6","DSQ"="gray"))

Hamplot


Lec<-racetype[racetype$Driver=="LEC",]
Lec2 <-Lec  %>%
  group_by(Finish) %>%
  summarise(count = n())

Hamplot<-ggplot(Lec2, aes(x = "", y = count, fill = Finish)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  ggtitle("Max Verstappen's Season") +
  geom_text(aes(label = sprintf("%.1f%%", (count / sum(count)) * 100)),
            position = position_stack(vjust = 0.5),size=4.5)+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B","DNS"="#9655A6","DSQ"="gray"))

Hamplot



Sai<-racetype[racetype$Driver=="SAI",]
Sai$Finish[Sai$Status=="Fuel leak"]<-"DNS"
Sai2 <-Sai %>%
  group_by(Finish) %>%
  summarise(count = n())

Hamplot<-ggplot(Sai2, aes(x = "", y = count, fill = Finish)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  ggtitle("Max Verstappen's Season") +
  geom_text(aes(label = sprintf("%.1f%%", (count / sum(count)) * 100)),
            position = position_stack(vjust = 0.5),size=4.5)+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B","DNS"="#9655A6","DSQ"="gray"))

Hamplot


Nor<-racetype[racetype$Driver=="NOR",]
Nor2 <-Nor %>%
  group_by(Finish) %>%
  summarise(count = n())

Hamplot<-ggplot(Nor2, aes(x = "", y = count, fill = Finish)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  ggtitle("Max Verstappen's Season") +
  geom_text(aes(label = sprintf("%.1f%%", (count / sum(count)) * 100)),
            position = position_stack(vjust = 0.5),size=4.5)+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B","DNS"="#9655A6","DSQ"="gray"))

Hamplot



Pia<-racetype[racetype$Driver=="PIA",]
Pia2 <-Pia %>%
  group_by(Finish) %>%
  summarise(count = n())

Hamplot<-ggplot(Pia2, aes(x = "", y = count, fill = Finish)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  ggtitle("Max Verstappen's Season") +
  geom_text(aes(label = sprintf("%.1f%%", (count / sum(count)) * 100)),
            position = position_stack(vjust = 0.5),size=4.5)+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B","DNS"="#9655A6","DSQ"="gray"))

Hamplot


Alo<-racetype[racetype$Driver=="ALO",]
Alo2 <-Alo %>%
  group_by(Finish) %>%
  summarise(count = n())

Hamplot<-ggplot(Alo2, aes(x = "", y = count, fill = Finish)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  ggtitle("Max Verstappen's Season") +
  geom_text(aes(label = sprintf("%.1f%%", (count / sum(count)) * 100)),
            position = position_stack(vjust = 0.5),size=4.5)+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B","DNS"="#9655A6","DSQ"="gray"))

Hamplot


Str<-racetype[racetype$Driver=="STR",]
Str2 <-Str %>%
  group_by(Finish) %>%
  summarise(count = n())

Hamplot<-ggplot(Str2, aes(x = "", y = count, fill = Finish)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  ggtitle("Max Verstappen's Season") +
  geom_text(aes(label = sprintf("%.1f%%", (count / sum(count)) * 100)),
            position = position_stack(vjust = 0.5),size=4.5)+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B","DNS"="#9655A6","DSQ"="gray"))

Hamplot


Gas<-racetype[racetype$Driver=="GAS",]
Gas2 <-Gas %>%
  group_by(Finish) %>%
  summarise(count = n())

Hamplot<-ggplot(Gas2, aes(x = "", y = count, fill = Finish)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  ggtitle("Max Verstappen's Season") +
  geom_text(aes(label = sprintf("%.1f%%", (count / sum(count)) * 100)),
            position = position_stack(vjust = 0.5),size=4.5)+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B","DNS"="#9655A6","DSQ"="gray"))

Hamplot


Oco<-racetype[racetype$Driver=="OCO",]
Oco2 <-Oco %>%
  group_by(Finish) %>%
  summarise(count = n())

Hamplot<-ggplot(Oco2, aes(x = "", y = count, fill = Finish)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  ggtitle("Max Verstappen's Season") +
  geom_text(aes(label = sprintf("%.1f%%", (count / sum(count)) * 100)),
            position = position_stack(vjust = 0.5),size=4.5)+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B","DNS"="#9655A6","DSQ"="gray"))

Hamplot


Alb<-racetype[racetype$Driver=="ALB",]
Alb2 <-Alb %>%
  group_by(Finish) %>%
  summarise(count = n())

Hamplot<-ggplot(Alb2, aes(x = "", y = count, fill = Finish)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  ggtitle("Max Verstappen's Season") +
  geom_text(aes(label = sprintf("%.1f%%", (count / sum(count)) * 100)),
            position = position_stack(vjust = 0.5),size=4.5)+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B","DNS"="#9655A6","DSQ"="gray"))

Hamplot


Sar<-racetype[racetype$Driver=="SAR",]
Sar2 <-Sar %>%
  group_by(Finish) %>%
  summarise(count = n())

Hamplot<-ggplot(Sar2, aes(x = "", y = count, fill = Finish)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  ggtitle("Max Verstappen's Season") +
  geom_text(aes(label = sprintf("%.1f%%", (count / sum(count)) * 100)),
            position = position_stack(vjust = 0.5),size=4.5)+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B","DNS"="#9655A6","DSQ"="gray"))

Hamplot

Tsu<-racetype[racetype$Driver=="TSU",]
Tsu2 <-Tsu %>%
  group_by(Finish) %>%
  summarise(count = n())

Hamplot<-ggplot(Tsu2, aes(x = "", y = count, fill = Finish)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  ggtitle("Max Verstappen's Season") +
  geom_text(aes(label = sprintf("%.1f%%", (count / sum(count)) * 100)),
            position = position_stack(vjust = 0.5),size=4.5)+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B","DNS"="#9655A6","DSQ"="gray"))

Hamplot

Ric<-racetype[racetype$Driver=="RIC",]
Ric2 <-Ric %>%
  group_by(Finish) %>%
  summarise(count = n())

Hamplot<-ggplot(Ric2, aes(x = "", y = count, fill = Finish)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  ggtitle("Max Verstappen's Season") +
  geom_text(aes(label = sprintf("%.1f%%", (count / sum(count)) * 100)),
            position = position_stack(vjust = 0.5),size=4.5)+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B","DNS"="#9655A6","DSQ"="gray"))

Hamplot


Bot<-racetype[racetype$Driver=="BOT",]
Bot2 <-Bot %>%
  group_by(Finish) %>%
  summarise(count = n())

Hamplot<-ggplot(Bot2, aes(x = "", y = count, fill = Finish)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  ggtitle("Max Verstappen's Season") +
  geom_text(aes(label = sprintf("%.1f%%", (count / sum(count)) * 100)),
            position = position_stack(vjust = 0.5),size=4.5)+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B","DNS"="#9655A6","DSQ"="gray"))

Hamplot


Zho<-racetype[racetype$Driver=="ZHO",]
Zho2 <-Zho %>%
  group_by(Finish) %>%
  summarise(count = n())

Hamplot<-ggplot(Zho2, aes(x = "", y = count, fill = Finish)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  ggtitle("Max Verstappen's Season") +
  geom_text(aes(label = sprintf("%.1f%%", (count / sum(count)) * 100)),
            position = position_stack(vjust = 0.5),size=4.5)+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B","DNS"="#9655A6","DSQ"="gray"))

Hamplot

Hul<-racetype[racetype$Driver=="HUL",]
Hul2 <-Hul %>%
  group_by(Finish) %>%
  summarise(count = n())

Hamplot<-ggplot(Hul2, aes(x = "", y = count, fill = Finish)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  ggtitle("Max Verstappen's Season") +
  geom_text(aes(label = sprintf("%.1f%%", (count / sum(count)) * 100)),
            position = position_stack(vjust = 0.5),size=4.5)+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B","DNS"="#9655A6","DSQ"="gray"))

Hamplot

Mag<-racetype[racetype$Driver=="MAG",]
Mag2 <-Mag %>%
  group_by(Finish) %>%
  summarise(count = n())

Hamplot<-ggplot(Mag2, aes(x = "", y = count, fill = Finish)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  ggtitle("Max Verstappen's Season") +
  geom_text(aes(label = sprintf("%.1f%%", (count / sum(count)) * 100)),
            position = position_stack(vjust = 0.5),size=4.5)+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B","DNS"="#9655A6","DSQ"="gray"))

Hamplot




avgqualygap<-read.csv("Gaptopole2022vs2023.csv")
avgqualyplot <- ggplot(avgqualygap, aes(x = Gap2022, y = reorder(Team, -Gap2022), fill = Team)) + 
  geom_vline(xintercept = seq(.5,3, by = .5), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Gap2022, 3)), hjust = -0.1, size = 5, color = "black") +  # Add round() function for labels
  scale_fill_manual(values = teamscolors) +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, by = .5)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Avg Gap to pole") +
  ylab("Team") +
  ggtitle("Average gap to pole") +
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
  labs(caption = "@f1.datascience", subtitle = "2022 Season") +
  theme(legend.position = "none")

avgqualyplot

avgqualyplot2 <- ggplot(avgqualygap, aes(x = Gap2023, y = reorder(Team, -Gap2023), fill = Team)) + 
  geom_vline(xintercept = seq(.5,3, by = .5), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Gap2023, 3)), hjust = -0.1, size = 5, color = "black") +  # Add round() function for labels
  scale_fill_manual(values = teamscolors) +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, by = .5)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Avg Gap to pole") +
  ylab("Team") +
  ggtitle("Average gap to pole") +
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
  labs(caption = "@f1.datascience", subtitle = "2023 Season") +
  theme(legend.position = "none")

avgqualyplot2



avgrace<-read.csv("RACEPACEMSVSES.csv")
avgqualyplot <- ggplot(avgrace, aes(x = GapMS, y = reorder(Team, -GapMS), fill = Team)) + 
  geom_vline(xintercept = seq(.5,3, by = .5), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(GapMS, 3)), hjust = -0.1, size = 5, color = "black") +  # Add round() function for labels
  scale_fill_manual(values = teamscolors) +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, by = .5)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Avg Gap to fastest") +
  ylab("Team") +
  ggtitle("Average gap to fastest race pace") +
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
  labs(caption = "@f1.datascience", subtitle = "Mid-Season") +
  theme(legend.position = "none")

avgqualyplot

avgqualyplot2 <- ggplot(avgrace, aes(x = GapES, y = reorder(Team, -GapES), fill = Team)) + 
  geom_vline(xintercept = seq(.5,3, by = .5), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(GapES, 3)), hjust = -0.1, size = 5, color = "black") +  # Add round() function for labels
  scale_fill_manual(values = teamscolors) +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, by = .5)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Avg Gap to fastest") +
  ylab("Team") +
  ggtitle("Average gap to fastest race pace") +
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
  labs(caption = "@f1.datascience", subtitle = "End of Season") +
  theme(legend.position = "none")

avgqualyplot2


avgqualyplot <- ggplot(avgrace, aes(x = GapoleMS, y = reorder(Team, -GapoleMS), fill = Team)) + 
  geom_vline(xintercept = seq(.5,3, by = .5), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(GapoleMS, 3)), hjust = -0.1, size = 5, color = "black") +  # Add round() function for labels
  scale_fill_manual(values = teamscolors) +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, by = .5)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Avg Gap to fastest") +
  ylab("Team") +
  ggtitle("Average gap to pole") +
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
  labs(caption = "@f1.datascience", subtitle = "Mid-Season") +
  theme(legend.position = "none")

avgqualyplot

avgqualyplot2 <- ggplot(avgrace, aes(x = GapoleES, y = reorder(Team, -GapoleES), fill = Team)) + 
  geom_vline(xintercept = seq(.5,3, by = .5), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(GapoleES, 3)), hjust = -0.1, size = 5, color = "black") +  # Add round() function for labels
  scale_fill_manual(values = teamscolors) +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, by = .5)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Avg Gap to fastest") +
  ylab("Team") +
  ggtitle("Average gap to pole") +
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
  labs(caption = "@f1.datascience", subtitle = "End of Season") +
  theme(legend.position = "none")

avgqualyplot2


penalties<-read.csv("timepenalties.csv")
avgqualyplot2 <- ggplot(penalties, aes(x = Number, y = reorder(Driver, Number), fill = Driver)) + 
  geom_vline(xintercept = seq(1,7, by = 1), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(Time, "s")), hjust = -0.1, size = 5, color = "black") +  # Add round() function for labels
  scale_fill_manual(values = drivercolors) +
  scale_x_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Number of time penalties") +
  ylab("Driver") +
  ggtitle("Number of time penalties by driver") +
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
  labs(caption = "@f1.datascience", subtitle = "2023 Season") +
  theme(legend.position = "none")

avgqualyplot2




avgqualygap<-read.csv("Gapteammates2023vs2022.csv")
avgqualyplot <- ggplot(avgqualygap, aes(x = Gap2022, y = reorder(Team, -Gap2022), fill = Team)) + 
  geom_vline(xintercept = seq(.1,.6, by = .1), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(Gap2022, 3), " ", DriverA2022)), hjust = -0.1, size = 4, color = "black")+
  scale_fill_manual(values = teamscolors) +
  scale_x_continuous(limits = c(0, .6), breaks = seq(0, .6, by =.1)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Gap between Teammates") +
  ylab("Team") +
  ggtitle("Average gap between teammates in qualy") +
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
  labs(caption = "@f1.datascience", subtitle = "2022 Season") +
  theme(legend.position = "none")

avgqualyplot

avgqualyplot2 <- ggplot(avgqualygap, aes(x = Gap2023, y = reorder(Team, -Gap2023), fill = Team)) + 
  geom_vline(xintercept = seq(.1,.6, by = .1), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(Gap2023, 3), " ", DriverA2023)), hjust = -0.1, size = 3.5, color = "black")+
  scale_fill_manual(values = teamscolors) +
  scale_x_continuous(limits = c(0, .6), breaks = seq(0, .6, by =.1)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Gap between Teammates") +
  ylab("Team") +
  ggtitle("Average gap between teammates in qualy") +
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
  labs(caption = "@f1.datascience", subtitle = "2023 Season") +
  theme(legend.position = "none")

avgqualyplot2


avgqualyplot <- ggplot(avgqualygap, aes(x = GapR2022, y = reorder(Team, -GapR2022), fill = Team)) + 
  geom_vline(xintercept = seq(.1,.6, by = .1), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(GapR2022, 3), " ", DriverAR2022)), hjust = -0.1, size = 4, color = "black")+
  scale_fill_manual(values = teamscolors) +
  scale_x_continuous(limits = c(0, .6), breaks = seq(0, .6, by =.1)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Gap between Teammates") +
  ylab("Team") +
  ggtitle("Average gap between teammates Race Pace") +
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
  labs(caption = "@f1.datascience", subtitle = "2022 Season") +
  theme(legend.position = "none")

avgqualyplot

avgqualyplot2 <- ggplot(avgqualygap, aes(x = GapR2023, y = reorder(Team, -GapR2023), fill = Team)) + 
  geom_vline(xintercept = seq(.1,.6, by = .1), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(GapR2023, 3), " ", DriverAR2023)), hjust = -0.1, size = 3.5, color = "black")+
  scale_fill_manual(values = teamscolors) +
  scale_x_continuous(limits = c(0, .6), breaks = seq(0, .6, by =.1)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Gap between Teammates") +
  ylab("Team") +
  ggtitle("Average gap between teammates Race Pace") +
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
  labs(caption = "@f1.datascience", subtitle = "2023 Season") +
  theme(legend.position = "none")

avgqualyplot2



avgqualygap<-read.csv("FSAVGGaptopole2022.csv")
avgqualyplot <- ggplot(avgqualygap, aes(x = Gap, y = reorder(Driver, -Gap), fill = Driver)) + 
  geom_vline(xintercept = seq(.5,3.5, by = .5), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Gap, 3)), hjust = -0.1, size = 4, color = "black")+
  scale_fill_manual(values =drivercolors2022 ) +
  scale_x_continuous(limits = c(0, 3.5), breaks = seq(0, 3.5, by =.5)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Gap to pole") +
  ylab("Team") +
  ggtitle("Average gap to pole") +
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
  labs(caption = "@f1.datascience", subtitle = "2022 Season") +
  theme(legend.position = "none")

avgqualyplot

avgqualygap<-read.csv("FSAVGGaptopole.csv")
avgqualyplot2 <- ggplot(avgqualygap, aes(x = Gap, y = reorder(Driver, -Gap), fill = Driver)) + 
  geom_vline(xintercept = seq(.5,3.5, by = .5), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Gap, 3)), hjust = -0.1, size = 4, color = "black")+
  scale_fill_manual(values =drivercolors ) +
  scale_x_continuous(limits = c(0, 3.5), breaks = seq(0, 3.5, by =.5)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Gap to pole") +
  ylab("Team") +
  ggtitle("Average gap to pole") +
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
  labs(caption = "@f1.datascience", subtitle = "2023 Season") +
  theme(legend.position = "none")

avgqualyplot2


avgqualygap<-read.csv("FSAVGGapRacePace2022.csv")
avgqualyplot <- ggplot(avgqualygap, aes(x = Gap, y = reorder(Driver, -Gap), fill = Driver)) + 
  geom_vline(xintercept = seq(.5,3.5, by = .5), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Gap, 3)), hjust = -0.1, size = 4, color = "black")+
  scale_fill_manual(values =drivercolors2022 ) +
  scale_x_continuous(limits = c(0, 3.5), breaks = seq(0, 3.5, by =.5)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Gap to fastest") +
  ylab("Team") +
  ggtitle("Average gap to fastest: Race") +
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
  labs(caption = "@f1.datascience", subtitle = "2022 Season") +
  theme(legend.position = "none")

avgqualyplot

avgqualygap<-read.csv("FSAVGGapRacePace2023.csv")
avgqualyplot2 <- ggplot(avgqualygap, aes(x = Gap, y = reorder(Driver, -Gap), fill = Driver)) + 
  geom_vline(xintercept = seq(.5,3.5, by = .5), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Gap, 3)), hjust = -0.1, size = 4, color = "black")+
  scale_fill_manual(values =drivercolors ) +
  scale_x_continuous(limits = c(0, 3.5), breaks = seq(0, 3.5, by =.5)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Gap to fastest") +
  ylab("Team") +
  ggtitle("Average gap to fastest: Race") +
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
  labs(caption = "@f1.datascience", subtitle = "2023 Season") +
  theme(legend.position = "none")

avgqualyplot2





costperpoint<-read.csv("costperpoint.csv")
costperpointc <- ggplot(costperpoint, aes(x = Cost.per.point, y = reorder(Driver, -Cost.per.point), fill = Driver)) + 
  geom_vline(xintercept = seq(.025,.35, by = .025), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Cost.per.point, 3)), hjust = -0.1, size = 4, color = "black")+
  scale_fill_manual(values =drivercolors ) +
  scale_x_continuous(limits = c(0, .35), breaks = seq(0, .35, by =.025)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Cost per point") +
  ylab("Driver") +
  ggtitle("Cost per point (in millions)") +
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
  labs(caption = "@f1.datascience", subtitle = "2023 Season") +
  theme(legend.position = "none")

costperpointc

landocharles<-read.csv("landocharles.csv")
lando<-landocharles[landocharles$Driver=="Lando",]
charles<-landocharles[landocharles$Driver=="Charles",]

landoc<-ggplot(lando, aes(x =Points, y = as.factor(Year))) +
  geom_vline(xintercept = seq(25,225, by = 25), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity",fill="#F58020")+
  geom_text(aes(label = Points), hjust = -0.1, size = 4, color = "black")+
  scale_x_continuous(limits = c(0, 225), breaks = seq(0, 225, by =25)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Points") +
  ylab("Year") +
  ggtitle("Lando Norris's points per season") +
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
  labs(caption = "@f1.datascience", subtitle = "With McLaren ") +
  theme(legend.position = "none")
landoc

charlesc<-ggplot(charles, aes(x =Points, y = as.factor(Year))) +
  geom_vline(xintercept = seq(50,350, by = 50), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity",fill="#F91536")+
  geom_text(aes(label = Points), hjust = -0.1, size = 4, color = "black")+
  scale_x_continuous(limits = c(0, 350), breaks = seq(0, 350, by =50)) +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("Points") +
  ylab("Year") +
  ggtitle("Charles Leclerc's points per season") +
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
  labs(caption = "@f1.datascience", subtitle = "With Ferrari") +
  theme(legend.position = "none")
charlesc
