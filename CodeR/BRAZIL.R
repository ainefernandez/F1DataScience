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


gapSingaporeQ<-read.csv("idealgap.csv")
pQualyS<-ggplot(gapSingaporeQ, aes(x=Gap,y=reorder(Driver,-Gap),fill=Driver))+ 
  geom_vline(xintercept = seq(.1, 1.4, by = .1), linetype = "dashed", color = "grey") +
  
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, 1.4, by = .1))+
  scale_y_discrete(expand = c(0,0))+
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Ideal qualy laps")+
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
  labs(caption = "@f1.datascience",subtitle = "2023 Brazilian GP")+theme(legend.position = "none")
 
pQualyS

results<-read.csv("ResultsBrazilR2023.csv")
results <- results[results$Abbreviation != "nan", ]
results$GridPosition[results$Abbreviation=="LEC"]<-2
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
  labs(title="Grid and race position",subtitle="2023 Brazilian GP",caption = "@f1.datascience")
p


stints<-read.csv("StintsBrazil.csv")
stints$Stint_Compound <- paste(stints$Stint, stints$Compound)
stints$Stint_Compound<-factor(stints$Stint_Compound,levels=c("6 SOFT","5 SOFT","4 HARD","4 MEDIUM","4 SOFT","3 SOFT","3 HARD","3 MEDIUM","2 SOFT","2 MEDIUM","2 HARD",
                                                             "1 SOFT","1 MEDIUM","1 HARD"))
stints$Driver<-factor(stints$Driver,levels=c("LEC","ALB","MAG","ZHO","BOT","RUS",
                                             "PIA","RIC","HUL","SAR","OCO",
                                             "TSU","HAM","GAS","SAI","STR",
                                             "PER","ALO","NOR","VER"))
stintsSpain <- ggplot(stints, aes(Driver, StintLength, fill = Stint_Compound)) +
  geom_bar(stat = "identity",color="black") +
  coord_flip() +
  scale_y_continuous(limits = c(0,72), breaks = seq(0, 72, by = 2)) +
  scale_fill_manual(values = c("6 SOFT"="#FF3333","5 SOFT"="#FF3333","1 SOFT" = "#FF3333", "2 SOFT" = "#FF3333", "3 SOFT" = "#FF3333", "4 SOFT" = "#FF3333", "1 MEDIUM" = "#ffe541", "2 MEDIUM" = "#ffe541",
                               "3 MEDIUM" = "#ffe541", "4 MEDIUM" = "#ffe541",
                               "1 HARD" = "#d2d2d2", "2 HARD" = "#d2d2d2", "3 HARD" = "#d2d2d2", "4 HARD" = "#d2d2d2")) +
  labs(title = "Tyre strategies", subtitle = "2023 Brazilian GP", caption = "@f1.datascience", y = "Lap") +
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


singapur<-read.csv("gapchecoalonso.csv")
monlando <- ggplot(singapur, aes(x = Lap, y = Gap)) +
  geom_line(size = 1, col = "#358C75") +
  geom_point(size = 3, col = "#358C75") +
  geom_hline(yintercept = 0, col = "red", linetype = "solid") +  # Add the red vertical line
  scale_y_continuous(limits = c(-0.5, 0.8), breaks = seq(-0.5, 0.8, by = 0.1)) +
  scale_x_continuous(breaks = seq(66, 71, by = 1)) +
  theme_bw() +
  labs(
    x = "Lap",
    y = "Gap (s)",
    title = "PER Gap to ALO",
    subtitle = "2023 Brazilian GP (66-71)",
    caption = "@f1.datascience"
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5)
  )

monlando

VERQ <- read.csv("VERQBrazil.csv")
NORQ <- read.csv("NORQBrazil.csv")
NORQ$faster <- NA

for (i in 1:nrow(NORQ)) {
  if (!is.na(VERQ$Speed[i]) & !is.na(NORQ$Speed[i])) {
    if (VERQ$Speed[i] > NORQ$Speed[i]) {
      NORQ$faster[i] <- "Verstappen is faster"
    } else if (NORQ$Speed[i] > VERQ$Speed[i]) {
      NORQ$faster[i] <- "Norris is faster"
    } else {
      NORQ$faster[i] <- "Norris is faster"
    }
  } else {
    NORQ$faster[i] <- "Norris is faster"
  }
}

LapDataNORBra<- data.frame(X = NORQ$X, Y = NORQ$Y, Speed=NORQ$Speed, Faster=NORQ$faster)
pSAIQ <- ggplot(LapDataNORBra, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Faster, group = Faster),
               size = 3, lineend = "round") +
  scale_color_manual(
    values = c("Norris is faster" = "#F58020","Verstappen is faster" = "#3671C6"),
    breaks = c("Norris is faster","Verstappen is faster")
  ) +
  coord_equal()+coord_flip()+
  theme_classic() + theme(
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 13)
  ) +
  labs(title = "VER and NOR Fastest Race Lap Comparison", subtitle = "2023 Brazilian GP", caption = "@f1.datascience")

pSAIQ


