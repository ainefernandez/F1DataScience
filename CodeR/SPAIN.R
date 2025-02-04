library(ggplot2)
library(dplyr)
library(gganimate)
setwd("/Users/ainefernandez/documents/F1DataScience")
VERQ<-read.csv("telVERQSpain.csv")
SAIQ<-read.csv("telSAIQSpain.csv")
NORQ<-read.csv("telNORQSpain.csv")
VERQ$faster <- NA

for (i in 1:nrow(VERQ)) {
  if (!is.na(VERQ$Speed[i]) & !is.na(SAIQ$Speed[i]) & !is.na(NORQ$Speed[i])) {
    if (VERQ$Speed[i] > SAIQ$Speed[i] & VERQ$Speed[i] > NORQ$Speed[i]) {
      VERQ$faster[i] <- "Verstappen is faster"
    } else if (SAIQ$Speed[i] > VERQ$Speed[i] & SAIQ$Speed[i] > NORQ$Speed[i]) {
      VERQ$faster[i] <- "Sainz is faster"
    } else if (NORQ$Speed[i] > VERQ$Speed[i] & NORQ$Speed[i] >SAIQ$Speed[i]) {
      VERQ$faster[i] <- "Norris is faster"
    } else {
      VERQ$faster[i] <- "Verstappen is faster"
    }
  } else {
    VERQ$faster[i] <- "Verstappen is faster"
  }
}


LapDataVERSpain<- data.frame(X = VERQ$X, Y = VERQ$Y, Speed=VERQ$Speed, Faster=VERQ$faster)
pVERS <- ggplot(LapDataVERSpain, aes(x = Y, y = X)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(Y), y = lag(X), xend = Y, yend = X, color = Faster, group = Faster),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("Verstappen is faster" = "#3671C6", "Sainz is faster" = "#F91536", "Norris is faster" = "#F58020"))  +
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
  labs(title = "VER, SAI and NOR fastest lap comparison",
       subtitle = "2023 Spanish GP",
       caption = "@f1.datascience")
pVERS

SAIQ$faster <- NA
for (i in 1:nrow(SAIQ)) {
  if (!is.na(SAIQ$Speed[i]) & !is.na(NORQ$Speed[i])) {
    if (SAIQ$Speed[i] > NORQ$Speed[i]) {
      SAIQ$faster[i] <- "Sainz is faster"
    } else if (NORQ$Speed[i] > SAIQ$Speed[i]) {
      SAIQ$faster[i] <- "Norris is faster"
    } else {
      SAIQ$faster[i] <- "Sainz is faster"
    }
  } else {
    SAIQ$faster[i] <- "Sainz is faster"
  }
}

LapDataSAISpain<- data.frame(X = SAIQ$X, Y = SAIQ$Y, Speed=SAIQ$Speed, Faster=SAIQ$faster)
pSAIS <- ggplot(LapDataSAISpain, aes(x = Y, y = X)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(Y), y = lag(X), xend = Y, yend = X, color = Faster, group = Faster),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("Sainz is faster" = "#F91536", "Norris is faster" = "#F58020"))  +
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
  labs(title = "SAI and NOR fastest lap comparison",
       subtitle = "2023 Spanish GP",
       caption = "@f1.datascience")
pSAIS

PIAQ<-read.csv("telPIAQSpain.csv")
NORQ$faster <- NA
for (i in 1:nrow(NORQ)) {
  if (!is.na(PIAQ$Speed[i]) & !is.na(NORQ$Speed[i])) {
    if (PIAQ$Speed[i] > NORQ$Speed[i]) {
      NORQ$faster[i] <- "Piastri is faster"
    } else if (NORQ$Speed[i] > PIAQ$Speed[i]) {
      NORQ$faster[i] <- "Norris is faster"
    } else {
      NORQ$faster[i] <- "Norris is faster"
    }
  } else {
    NORQ$faster[i] <- "Norris is faster"
  }
}

LapDataNORSpain<- data.frame(X = NORQ$X, Y = NORQ$Y, Speed=NORQ$Speed, Faster=NORQ$faster)
pNORS <- ggplot(LapDataNORSpain, aes(x = Y, y = X)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(Y), y = lag(X), xend = Y, yend = X, color = Faster, group = Faster),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("Piastri is faster" = "#47C7FC", "Norris is faster" = "#F58020"))  +
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
  labs(title = "NOR and PIA fastest lap comparison",
       subtitle = "2023 Spanish GP",
       caption = "@f1.datascience")
pNORS

gapSpainQ<-read.csv("SpainQGaps.csv")
pQualyS<-ggplot(gapSpainQ, aes(x=Gap,y=reorder(Driver,-Gap),fill=Team))+ 
  geom_vline(xintercept = c(0.5,1, 1.5,2.0,2.5), linetype = "dashed", color = "grey")+
  
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
                               "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo "="#C92D4B"))+
  scale_x_continuous(breaks = seq(0, 2.5, by = 0.1))+
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
  labs(caption = "@f1.datascience",subtitle = "2023 Spanish GP")+theme(legend.position = "none")
pQualyS


SpainQualy<-read.csv("QSpain.csv")
driver_order <- SpainQualy %>% 
  filter(Q == 3) %>% 
  arrange(Position) %>% 
  pull(Driver)
anim <- ggplot(SpainQualy) +
  geom_line(size = 1, aes(x = Q, y = Position, col = Driver, group=Driver)) +
  geom_point(size = 3, aes(x = Q, y = Position, col = Driver)) +
  scale_color_manual(values = c("VER" = "#3671C6", "SAI" = "#F91536","NOR" = "#F58020", "GAS" = "#2293D1", "HAM" = "#6CD3BF",
                                "STR" = "#CEDC00", "OCO" = "#FD4BC7","HUL" = "#B6BABD","ALO" = "#358C75","PIA" = "#47C7FC",
                                "PER" = "#FDD900",  "RUS" = "#565F64","ZHO" = "#C92D4B","DEV" = "#5E8FAA","TSU" = "#DD0740", 
                                "BOT" = "#1E5341","MAG" = "#DA291C",  "ALB" = "#37BEDD","LEC" = "#00A551", "SAR" = "#041E42", breaks=driver_order)) +
  scale_y_reverse(breaks = seq(1, 20, by = 1))+
  scale_x_continuous(breaks=seq(0,3, by = 1))+
  theme_bw() +
  labs(x = "Q", y = "Position") +
  transition_reveal(Q) +
  theme(plot.title=element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust=0.5))+
  labs(title = "2023 Spanish GP Qualy",caption = "@f1.datascience")
anim

stints<-read.csv("StintsSpain2023.csv")
stints$Driver<-factor(stints$Driver,levels=c("SAR","BOT","MAG","NOR","ALB",
                                             "HUL","DEV","PIA","TSU","LEC",
                                             "GAS","ZHO","OCO","ALO","STR",
                                             "SAI","PER","RUS","HAM","VER"))

stints$Stint_Compound <- paste(stints$Stint, stints$Compound)
stints$Stint_Compound<-factor(stints$Stint_Compound,levels=c("4 HARD","4 SOFT","4 MEDIUM","3 SOFT","3 HARD","3 MEDIUM","2 SOFT","2 MEDIUM","2 HARD",
                                       "1 SOFT","1 MEDIUM","1 HARD"))
stintsSpain <- ggplot(stints, aes(Driver, StintLength, fill = Stint_Compound)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 66), breaks = seq(0, 66, by = 2)) +
  scale_fill_manual(values = c("1 SOFT" = "#FF3333", "2 SOFT" = "#FF3333", "3 SOFT" = "#FF3333", "4 SOFT" = "#FF3333", "1 MEDIUM" = "#ffe541", "2 MEDIUM" = "#ffe541",
                               "3 MEDIUM" = "#ffe541", "4 MEDIUM" = "#ffe541",
                               "1 HARD" = "#d2d2d2", "2 HARD" = "#d2d2d2", "3 HARD" = "#d2d2d2", "4 HARD" = "#d2d2d2")) +
  labs(title = "Tyre strategies", subtitle = "2023 Spanish GP", caption = "@f1.datascience", y = "Lap") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -20), size = 12),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  theme(legend.position = "none") 

stintsSpain




resultsSpain<-read.csv("ResultsSpainR2023.csv")
resultsSpain<-resultsSpain[c("Abbreviation","GridPosition","Position")]
left_label <- paste(resultsSpain$Abbreviation, round(resultsSpain$GridPosition),sep=", ")
right_label <- paste(resultsSpain$Abbreviation, round(resultsSpain$Position),sep=", ")
p <- ggplot(resultsSpain) + 
  geom_segment(aes(x=1, xend=2, y=GridPosition, yend=Position, col=Abbreviation), size=1, show.legend=F) +
  geom_vline(xintercept=1, linetype="dashed", size=.1) +
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(values = c("VER"="#3671C6","PER"="#3671C6","ALO"="#358C75","SAI"="#F91536","HAM"="#6CD3BF","STR"="#358C75","RUS"="#6CD3BF","BOT"="#C92D4B","GAS"="#2293D1","ALB"="#37BEDD",
                                "TSU"="#5E8FAA","SAR"="#37BEDD","MAG"="#B6BABD","DEV"="#5E8FAA","HUL"="#B6BABD","ZHO"="#C92D4B","NOR"="#F58020","OCO"="#2293D1","LEC"="#F91536","PIA"="#F58020")) +
  scale_y_reverse() + 
  geom_text(aes(y = GridPosition, x = 1, label = left_label), hjust = 1, size = 4) +
  geom_text(aes(y =Position, x = 2, label = right_label), hjust = 0, size = 4) +
  geom_text(label = "Grid Position", x = 1, y = max(resultsSpain$GridPosition, resultsSpain$Position) * 1.1, hjust = 1.2, size = 5) +
  geom_text(label = "Race Position", x = 2, y = max(resultsSpain$GridPosition, resultsSpain$Position) * 1.1, hjust = -0.1, size = 5) +
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
  labs(title="Grid and race position",subtitle="2023 Spanish GP",caption = "@f1.datascience")

p
spain<-read.csv("racepaceSpainLEC.csv")
spainLec<-ggplot(spain, aes(x=LapNumber, y=LapTimeSeconds, col=Driver)) +
  geom_line(size=1) +
  geom_point(size=3)+
  scale_color_manual(values=c("GAS"="#2293D1","LEC"="#F91536","ZHO"="#C92D4B"))+
  scale_y_continuous(limits=c(78.5,79.6),breaks=seq(78.5, 79.6, by = 0.1))+
  scale_x_continuous(breaks=seq(56,66, by = 1))+
  theme_bw()+
  labs(x = "Lap", y = "Lap Time (s)",title="ZHO, GAS and LEC laptimes comparison",subtitle ="2023 Spanish GP (56-66)",caption = "@f1.datascience" )+
  theme(panel.background=element_rect(fill="white", color="white"),
        plot.background=element_rect(fill="white", color="white"),
        plot.title=element_text(size=18, hjust=0.5),
        plot.subtitle=element_text(size=15, hjust=0.5),
        plot.caption=element_text(size=11, hjust=0.5))

spainLec


spainMER<-read.csv("racepaceSpainRUS.csv")
spainRUS<-ggplot(spainMER, aes(x=LapNumber, y=LapTimeSeconds, col=Driver)) +
  geom_line(size=1) +
  geom_point(size=3)+
  scale_color_manual(values=c("VER"="#3671C6","PER" = "#FDD900","HAM" = "#6CD3BF","RUS" = "#565F64"))+
  scale_y_continuous(limits=c(76,100),breaks=seq(76, 100, by = 1))+
  scale_x_continuous(breaks=seq(44,66, by = 1))+
  theme_bw()+
  labs(x = "Lap", y = "Lap Time (s)",title="VER, PER,HAM and RUS laptimes comparison",subtitle ="2023 Spanish GP (44-66)",caption = "@f1.datascience" )+
  theme(panel.background=element_rect(fill="white", color="white"),
        plot.background=element_rect(fill="white", color="white"),
        plot.title=element_text(size=18, hjust=0.5),
        plot.subtitle=element_text(size=15, hjust=0.5),
        plot.caption=element_text(size=11, hjust=0.5))

spainRUS







gapSpainR<-read.csv("SpainRGaps2.csv")
gapSpainR$Team[gapSpainR$Team=="Alfa Romeo "]<-"Alfa Romeo"
pSpainR<-ggplot(gapSpainR, aes(x=Gap.,y=reorder(Driver.,-Gap.),fill=Team))+ 
  geom_vline(xintercept = c(0.5,1, 1.5,2.0,2.5,3.0), linetype = "dashed", color = "grey")+
  
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
                               "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo"="#C92D4B"))+
  scale_x_continuous(breaks = seq(0, 3.0, by = 0.1))+
  scale_y_discrete(expand = c(0,0))+
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Gap to fastest lap in the race")+
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
  labs(caption = "@f1.datascience",subtitle = "2023 Spanish GP")+theme(legend.position = "none")
pSpainR

