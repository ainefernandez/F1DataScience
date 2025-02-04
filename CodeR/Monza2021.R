library(ggplot2)
library(dplyr)
library(gganimate)
setwd("/Users/ainefernandez/documents/F1DataScience")
monza<-read.csv("lap_data.csv")
monzaplot<-ggplot(monza, aes(x=LapNumber, y=LapTime, col=Driver)) +
  geom_line(size=1) +
  geom_point(size=3)+
  scale_color_manual(values=c("VER"="#3671C6","RIC"="#F58020","NOR"="#F35497","HAM"="#6CD3BF"))+
  scale_y_continuous(limits=c(85.5,91),breaks=seq(85.5, 91, by = .5))+
  scale_x_continuous(breaks=seq(2,22, by = 1))+
  theme_bw()+
  labs(x = "Lap", y = "Lap Time (s)",title="RIC,VER, NOR and HAM laptimes comparison",subtitle ="2021 Monza GP (2-22)",caption = "@f1.datascience" )+
  theme(panel.background=element_rect(fill="white", color="white"),
        plot.background=element_rect(fill="white", color="white"),
        plot.title=element_text(size=18, hjust=0.5),
        plot.subtitle=element_text(size=15, hjust=0.5),
        plot.caption=element_text(size=11, hjust=0.5))

monzaplot

monza2<-read.csv("lap_dataAS.csv")
monza2<- monza2[, c("Driver", "LapNumber", "Position")]
new_row1 <- data.frame(Driver = "PER", LapNumber = 54, Position = 5)
new_row2 <- data.frame(Driver = "NOR", LapNumber = 54, Position = 2)
new_row3 <- data.frame(Driver = "LEC", LapNumber = 54, Position = 4)
new_row4 <- data.frame(Driver = "RIC", LapNumber = 54, Position = 1)
new_row5 <- data.frame(Driver = "BOT", LapNumber = 54, Position = 3)

# Append the new row to the dataframe
monza2 <- rbind(monza2, new_row1)
monza2 <- rbind(monza2, new_row2)
monza2 <- rbind(monza2, new_row3)
monza2 <- rbind(monza2, new_row4)
monza2 <- rbind(monza2, new_row5)
anim <- ggplot(monza2) +
  geom_line(size = 1, aes(x = LapNumber, y = Position, col = Driver, group=Driver)) +
  geom_point(size = 3, aes(x = LapNumber, y = Position, col = Driver)) +
  scale_color_manual(values = c("RIC" = "#F58020","NOR" = "#F35497", "PER" = "#3671C6",
                                "BOT" = "#6CD3BF", "LEC" = "#F91536"))+
  
  scale_y_reverse(breaks = seq(1, 6, by = 1))+
  scale_x_continuous(breaks=seq(29,53, by = 1))+
  theme_bw() +
  labs(x = "Lap", y = "Position") +
  transition_reveal(LapNumber) +
  theme(plot.title=element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust=0.5),
        plot.subtitle=element_text(size=15, hjust=0.5))+
  labs(title = "2021 Monza GP Race",subtitle= "Laps 29-53", caption = "@f1.datascience")
anim
