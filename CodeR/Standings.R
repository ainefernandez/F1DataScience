library(ggplot2)
library(dplyr)
library(forcats)
library(gganimate)
setwd("/Users/ainefernandez/documents/F1DataScience")
pointsdrivers<-read.csv("Points.csv")
pointsdrivers$GP<-factor(pointsdrivers$GP, levels = c("Abu Dhabi","Las Vegas","Brazil","Brazil Sprint","Mexico","Austin","Austin Sprint","Qatar","Qatar Sprint","Japan","Singapore","Italy","Netherlands","Belgium","Belgium Sprint","Hungary","Great Britain","Austria",
                                                      "Austria Sprint","Canada","Spain","Monaco","Miami","Azerbaijan",
                                                      "Azerbaijan Sprint","Australia","Saudi Arabia","Bahrain")) 
pointsdrivers$Driver<-factor(pointsdrivers$Driver,levels=c("DEV","SAR","LAW","MAG","ZHO",
                                                           "RIC","HUL","BOT","TSU","ALB",
                                                           "OCO","GAS","STR","PIA","RUS",
                                                           "LEC","NOR","ALO","SAI","HAM","PER","VER"))
pointsplot <- ggplot(pointsdrivers, aes(reorder(Driver,Points), Points, Points, fill = GP)) +
  geom_hline(yintercept = seq(50, 600, by = 50), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 50)) +
  coord_flip() +
  scale_fill_manual(values = c("Abu Dhabi"="#01A39B","Las Vegas"="black","Brazil"="#009B3B","Brazil Sprint"="#FFE500","Mexico"="#CE86D1","Austin"="#BF0D3E","Austin Sprint"="#00205B","Qatar" = "#8D1B3D", "Qatar Sprint" = "#E6C28B", "Japan" = "#C64293", "Singapore" = "#07D2E6", "Italy" = "#81A969", "Netherlands" = "#7C068B", "Belgium" = "#EECC0E", "Belgium Sprint" = "#e12031", "Hungary" = "#a15360",
                               "Great Britain" = "#307BC9", "Austria" = "#9C9EFF", "Austria Sprint" = "#9EBFB4",
                               "Canada" = "#ff8f00", "Spain" = "#AA151B", "Monaco" = "#CB9F18",
                               "Miami" = "#FF69B4", "Azerbaijan" = "#00AF66", "Azerbaijan Sprint" = "#02708F",
                               "Australia" = "#012169", "Bahrain" = "#CE1126", "Saudi Arabia" = "#165d31")) +
  labs(title = "Drivers Standings 2023 Season", caption = "@f1.datascience") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -15), size = 11),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11)) +
  guides(fill = guide_legend(title = "GP", ncol = 1, title.position = "top"))

pointsplot



pointsteams<-read.csv("PointsTeams.csv")
pointsteams$GP<-factor(pointsteams$GP, levels = c("Abu Dhabi","Las Vegas","Brazil","Brazil Sprint","Mexico","Austin","Austin Sprint","Qatar","Qatar Sprint","Japan","Singapore","Italy","Netherlands","Belgium","Belgium Sprint","Hungary","Great Britain","Austria",
                                                  "Austria Sprint ","Canada","Spain","Monaco","Miami","Azerbaijan ",
                                                  "Azerbaijan Sprint","Australia","Saudi Arabia","Bahrain")) 
pointsteams$Team<-factor(pointsteams$Team,levels=c("Haas","Alfa Romeo","AlphaTauri","Williams","Alpine",
                                                   "Aston Martin","McLaren","Ferrari","Mercedes","Red Bull"))
pointsplot2<-ggplot(pointsteams,aes(reorder(Team,Points),Points,fill=GP))+
  geom_hline(yintercept = seq(50, 900, by = 50), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_y_continuous(limits=c(0,900),breaks = seq(0, 900, by = 50))+
  coord_flip()+
  scale_fill_manual(values = c("Abu Dhabi"="#01A39B","Las Vegas"="black","Brazil"="#009B3B","Brazil Sprint"="#FFE500","Mexico"="#CE86D1","Austin"="#BF0D3E","Austin Sprint"="#00205B","Qatar" = "#8D1B3D", "Qatar Sprint" = "#E6C28B","Japan"="#C64293","Singapore"="#07D2E6","Italy"="#81A969","Netherlands"="#7C068B","Belgium"="#EECC0E","Belgium Sprint"="#e12031","Hungary"="#a15360",
                               "Great Britain"="#307BC9","Austria"="#9C9EFF","Austria Sprint "="#9EBFB4", 
                               "Canada"="#ff8f00","Spain"="#AA151B","Monaco"="#CB9F18",
                               "Miami"="#FF69B4","Azerbaijan "="#00AF66","Azerbaijan Sprint"="#02708F",
                               "Australia"="#012169","Bahrain"="#CE1126","Saudi Arabia"="#165d31"))+
  labs(title="Constructors Standings 2023 Season",caption = "@f1.datascience")+
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
        legend.text = element_text(size=12), 
        legend.title = element_text(size=12))+
  guides(fill = guide_legend(title = "GP", ncol = 1, title.position = "top"))
pointsplot2


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


percentage<-read.csv("pointspercentage.csv")
percentage$Driver<-factor(percentage$Driver,levels=c("SAR","ALB","DEV","TSU","ZHO","BOT","MAG","HUL","PIA","NOR",
                                                         "GAS","OCO","LEC","SAI","RUS","HAM","STR","ALO","PER","VER"))
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
  labs(caption = "@f1.datascience",subtitle = "After the Monaco GP")+theme(legend.position = "none")
 
plotpercentage


