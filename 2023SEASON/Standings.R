library(ggplot2)
library(dplyr)
library(forcats)
library(gganimate)
setwd("/Users/ainefernandez/documents/F1DataScience")
pointsdrivers<-read.csv("Points.csv")
pointsdrivers$GP<-factor(pointsdrivers$GP, levels = c("Spain","Monaco","Miami","Azerbaijan ",
                                                      "Azerbaijan Sprint ","Australia","Saudi Arabia","Bahrain")) 
pointsdrivers$Driver<-factor(pointsdrivers$Driver,levels=c("SAR","DEV","ALB","MAG","TSU",
                                                           "ZHO","BOT","PIA","HUL","NOR",
                                                           "GAS","OCO","STR","LEC","SAI",
                                                           "RUS","HAM","ALO","PER","VER"))
pointsplot<-ggplot(pointsdrivers,aes(reorder(Driver,Points),Points,fill=GP))+
  geom_hline(yintercept = c(10,20,30,40,50,60,70,80,90, 100,110,120,130,140,150,160,170), linetype = "dashed",color = "grey")+
  geom_bar(stat="identity")+
  scale_y_continuous(limits=c(0,170),breaks = seq(0, 170, by = 10))+
  coord_flip()+
  scale_fill_manual(values = c("Spain"="#AA151B","Monaco"="#CB9F18","Miami"="#FF69B4","Azerbaijan "="#00AF66","Azerbaijan Sprint "="#0092BC",
                               "Australia"="#012169","Bahrain"="#CE1126","Saudi Arabia"="#165d31"))+
  labs(title="Drivers Standings after the Spanish GP",caption = "@f1.datascience")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title=element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust=0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -20),size=12),
        axis.ticks.y =element_blank(),
        axis.title.y=element_blank(),
        legend.text = element_text(size=14), 
        legend.title = element_text(size=14))
  
  
pointsplot



pointsteams<-read.csv("PointsTeams.csv")
pointsteams$GP<-factor(pointsteams$GP, levels = c("Spain","Monaco","Miami","Azerbaijan ","Azerbaijan Sprint",
                                                  "Australia","Saudi Arabia","Bahrain")) 
pointsteams$Team[pointsteams$Team=="Alpine "]<-"Alpine"
pointsteams$Team<-factor(pointsteams$Team,levels=c("Williams","AlphaTauri","Alfa Romeo","Haas","McLaren",
                                                   "Alpine","Ferrari","Mercedes","Aston Martin","Red Bull"))
pointsplot2<-ggplot(pointsteams,aes(reorder(Team,Points),Points,fill=GP))+
  geom_hline(yintercept = c(20,40,60,80,100,120,140,160,180,200,220,240,260,280,300), linetype = "dashed",color = "grey")+
  geom_bar(stat="identity")+
  scale_y_continuous(limits=c(0,300),breaks = seq(0, 300, by = 20))+
  coord_flip()+
  scale_fill_manual(values = c("Spain"="#AA151B","Monaco"="#CB9F18","Miami"="#FF69B4","Azerbaijan "="#00AF66",
                               "Azerbaijan Sprint"="#0092BC","Australia"="#012169",
                               "Bahrain"="#CE1126","Saudi Arabia"="#165d31"))+
  labs(title="Constructors Standings after the Spanish GP",caption = "@f1.datascience")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title=element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust=0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -20),size=12),
        axis.ticks.y =element_blank(),
        axis.title.y=element_blank(),
        legend.text = element_text(size=14), 
        legend.title = element_text(size=14))
pointsplot2


gapsTeamates<-read.csv("GapsTeamates.csv")
View(gapsTeamates)
gapsTeamates$Driver<-factor(gapsTeamates$Driver,levels=c("GAS","OCO","SAR","ALB","STR","ALO","MAG","HUL","SAI","LEC",
                            "DEV","TSU","PIA","NOR","ZHO","BOT","VER","PER","HAM","RUS"))
pQualyT<-ggplot(gapsTeamates, aes(x=AvgGap,y=Driver,fill=Team))+ 
  geom_vline(xintercept = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
                               "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo"="#C92D4B"))+
  scale_x_continuous(breaks = seq(0, 1, by = 0.05))+
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
  labs(caption = "@f1.datascience",subtitle = "After the Miami GP")+theme(legend.position = "none")
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


