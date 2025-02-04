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





singapur<-read.csv("laptimesMEX2021.csv")
monlando<-ggplot(singapur, aes(x=LapNumber, y=LapTime, col=Driver)) +
  geom_line(size=1) +
  geom_point(size=3)+
  scale_color_manual(values=c("HAM"="#6CD3BF","PER"="#3671C6"))+
  scale_y_continuous(limits=c(79.75,81.25),breaks=seq(79.75, 81.25, by = 0.25))+
  scale_x_continuous(breaks=seq(60,71, by = 1))+
  theme_bw()+
  labs(x = "Lap", y = "Lap Time (s)",title="HAM and PER laptimes comparison",subtitle ="2021 Mexican GP (60-71)",caption = "@f1.datascience" )+
  theme(panel.background=element_rect(fill="white", color="white"),
        plot.background=element_rect(fill="white", color="white"),
        plot.title=element_text(size=18, hjust=0.5),
        plot.subtitle=element_text(size=15, hjust=0.5),
        plot.caption=element_text(size=11, hjust=0.5))

monlando

posMex<-read.csv("POSMEX2021.csv")
posMex<-posMex[, c("Driver", "Position", "LapNumber")]
new_data <- data.frame(
  Driver = c("BOT", "HAM", "VER", "PER", "GAS"),
  LapNumber = c(0,0,0,0,0),
  Position = c(1, 2, 3, 4, 5)
)

posMex<- rbind(posMex[, c("Driver", "Position", "LapNumber")], new_data)
posMex <- posMex[!duplicated(posMex), ]
monlando2 <- ggplot(posMex, aes(x = LapNumber, y = Position, col = Driver)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("HAM" = "#6CD3BF", "PER" = "gold", "VER" = "#3671C6", "BOT" = "black", "GAS" = "#5E8FAA")) +
  scale_y_reverse(limits = c(20, 1), breaks = seq(1, 20, by = 1)) +
  scale_x_continuous(breaks = seq(0, 72, by = 4)) +
  theme_bw() +
  labs(x = "Lap", y = "Position", title = "Top 5 Driver Position Changes", subtitle = "2021 Mexican GP", caption = "@f1.datascience") +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5))

monlando2







