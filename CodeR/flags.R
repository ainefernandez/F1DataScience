library(ggplot2)
library(dplyr)
library(gganimate)
setwd("/Users/ainefernandez/documents/F1DataScience")
rfsc<-read.csv("RFSC.csv")
names(rfsc)

Redflags <- ggplot(rfsc, aes(reorder(Circuit, RedFlags), RedFlags, fill = Type)) +
  geom_hline(yintercept = c(1,2,3,4,5,6,7), linetype = "dashed",color = "grey")+
  geom_bar(stat = "identity") +
  scale_y_continuous(limits=c(0,7),breaks = seq(0, 7, by = 1),expand=c(0,0))+
  coord_flip()+
  scale_fill_manual(values = c("Permanent"="skyblue","Street"="coral"))+
  labs(title="Number of Red Flags by circuit", y="Number of Red Flags", x="", caption = "@f1.datascience")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title=element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust=0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.ticks.y =element_blank(),
        axis.title.y=element_blank(),
        legend.text = element_text(size=14), 
        legend.title = element_text(size=14))


Redflags

safetycars<-ggplot(rfsc, aes(reorder(Circuit, SafetyCars), SafetyCars, fill = Type)) +
  geom_hline(yintercept = c(5,10,15,20,25,30), linetype = "dashed",color = "grey")+
  geom_bar(stat = "identity") +
  scale_y_continuous(limits=c(0,30),breaks = seq(0, 30, by = 5),expand=c(0,0))+
  coord_flip()+
  scale_fill_manual(values = c("Permanent"="skyblue","Street"="coral","Temporary"="lightgreen"))+
  labs(title="Number of Safety Cars by circuit", y="Circuit", x="Safety Cars", caption = "@f1.datascience")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title=element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust=0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.ticks.y =element_blank(),
        axis.title.y=element_blank(),
        legend.text = element_text(size=14), 
        legend.title = element_text(size=14))
safetycars

sumdf <- rfsc %>%
  group_by(Type) %>%
  summarise(total_redflags = sum(RedFlags),
            total_safetycars = sum(SafetyCars))
filtered_df <- sumdf %>%
  filter(Type %in% c("Permanent", "Street"))


redall<-ggplot(filtered_df, aes(reorder(Type, total_redflags), total_redflags, fill = Type)) +
  geom_hline(yintercept = c(10,20,30,40), linetype = "dashed",color = "grey")+
  geom_bar(stat = "identity") +
  scale_y_continuous(limits=c(0,40),breaks = seq(0, 40, by = 10),expand=c(0,0))+
  scale_fill_manual(values = c("Permanent" = "skyblue", "Street" = "coral")) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.position = "None") +
  labs(title = "Number of Red Flags by Circuit Type", y = "Number of Red Flags", x = "Circuit Type", caption = "@f1.datascience")
redall

safetyall<-ggplot(sumdf, aes(reorder(Type, total_safetycars), total_safetycars, fill = Type)) +
  geom_hline(yintercept = c(50,100,150), linetype = "dashed",color = "grey")+
  geom_bar(stat = "identity") +
  scale_y_continuous(limits=c(0,150),breaks = seq(0, 150, by = 10),expand=c(0,0))+
  scale_fill_manual(values = c("Permanent" = "skyblue", "Street" = "coral","Temporary"="lightgreen")) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.position = "None") +
  labs(title = "Number of Safety Cars by Circuit Type", y = "Number of Safety Cars", x = "Circuit Type", caption = "@f1.datascience")
safetyall

lapped<-read.csv("Lapped.csv")
Ldrivers<-ggplot(lapped, aes(reorder(Driver., Lapped), Lapped, fill = Team)) +
  geom_hline(yintercept = c(1,2,3,4,5), linetype = "dashed",color = "grey")+
  geom_bar(stat="identity")+
  scale_y_continuous(expand=c(0,0))+
  coord_flip()+
  scale_fill_manual(values = c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
                               "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo"="#C92D4B"))+
  labs(title="Drivers lapped this season", y="Number of times", x="", caption = "@f1.datascience")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title=element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust=0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.ticks.y =element_blank(),
        axis.title.y=element_blank(),
        legend.text = element_text(size=14), 
        legend.title = element_text(size=14),
        legend.position = "None")
Ldrivers


Lteams<-ggplot(lapped, aes(reorder(Team, Lapped), Lapped, fill = Team)) +
  geom_hline(yintercept = c(1,2,3,4,5,6,7), linetype = "dashed",color = "grey")+
  geom_bar(stat="identity")+
  scale_y_continuous(limits=c(0,7),breaks = seq(0, 7, by = 1),expand=c(0,0))+
  coord_flip()+
  scale_fill_manual(values = c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
                               "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo"="#C92D4B"))+
  labs(title="Teams lapped this season", y="Driver", x="Lapped", caption = "@f1.datascience")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title=element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust=0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.ticks.y =element_blank(),
        axis.title.y=element_blank(),
        legend.text = element_text(size=14), 
        legend.title = element_text(size=14),
        legend.position = "None")
Lteams




