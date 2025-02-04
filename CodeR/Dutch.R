library(ggplot2)
library(dplyr)
library(gganimate)
setwd("/Users/ainefernandez/documents/F1DataScience")
drivercolors <- c("VER" = "#3671C6","PER" = "#3671C6","ALO" = "#358C75","SAI" = "#F91536","HAM" = "#6CD3BF",
                  "STR" = "#358C75","RUS" = "#6CD3BF","BOT" = "#C92D4B","GAS" = "#2293D1","ALB" = "#37BEDD",
                  "TSU" = "#5E8FAA","SAR" = "#37BEDD","MAG" = "#B6BABD","DEV" = "#5E8FAA","HUL" = "#B6BABD",
                  "ZHO" = "#C92D4B", "NOR" = "#F58020","OCO" = "#2293D1","LEC" = "#F91536","PIA" = "#F58020", "RIC"="#5E8FAA","LAW"="#5E8FAA")
monacorain <- read.csv("Dutch2023.csv")

L1 <- filter(monacorain, Lap == 1)
L2 <- filter(monacorain, Lap == 2)
L3 <- filter(monacorain, Lap == 3)
L4 <- filter(monacorain, Lap == 4)
L5 <- filter(monacorain, Lap == 5)
L6 <- filter(monacorain, Lap == 6)
L7 <- filter(monacorain, Lap == 7)
L8 <- filter(monacorain, Lap == 8)
L9 <- filter(monacorain, Lap == 9)
L10 <- filter(monacorain, Lap == 10)
L11 <- filter(monacorain, Lap == 11)
L12 <- filter(monacorain, Lap == 12)


p1 <- ggplot(L1, aes(x = X, y =Y)) +
  geom_segment(aes(x =X, y = Y, xend = X, yend = Y, color = Fastest_compound),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("SLICK" = "gray", "INTERMEDIATE" = "#4E841A")) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 13, hjust = 0.5),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13)) +
  labs(title = "Slicks vs Intermediates", subtitle = "Lap 1", caption = "@f1.datascience") +
  theme(legend.position = "none")
p1


p2 <- ggplot(L2, aes(x = X, y =Y)) +
  geom_segment(aes(x =X, y = Y, xend = X, yend = Y, color = Fastest_compound),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("SLICK" = "gray", "INTERMEDIATE" = "#4E841A")) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 13, hjust = 0.5),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13)) +
  labs(title = "Slicks vs Intermediates", subtitle = "Lap 2", caption = "@f1.datascience") +
  theme(legend.position = "none")
p2


p3 <- ggplot(L3, aes(x = X, y =Y)) +
  geom_segment(aes(x =X, y = Y, xend = X, yend = Y, color = Fastest_compound),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("SLICK" = "gray", "INTERMEDIATE" = "#4E841A")) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 13, hjust = 0.5),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13)) +
  labs(title = "Slicks vs Intermediates", subtitle = "Lap 3", caption = "@f1.datascience") +
  theme(legend.position = "none")
p3

p4 <- ggplot(L4, aes(x = X, y =Y)) +
  geom_segment(aes(x =X, y = Y, xend = X, yend = Y, color = Fastest_compound),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("SLICK" = "gray", "INTERMEDIATE" = "#4E841A")) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 13, hjust = 0.5),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13)) +
  labs(title = "Slicks vs Intermediates", subtitle = "Lap 4", caption = "@f1.datascience") +
  theme(legend.position = "none")
p4

p5 <- ggplot(L5, aes(x = X, y =Y)) +
  geom_segment(aes(x =X, y = Y, xend = X, yend = Y, color = Fastest_compound),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("SLICK" = "gray", "INTERMEDIATE" = "#4E841A")) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 13, hjust = 0.5),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13)) +
  labs(title = "Slicks vs Intermediates", subtitle = "Lap 5", caption = "@f1.datascience") +
  theme(legend.position = "none")
p5

p6 <- ggplot(L6, aes(x = X, y =Y)) +
  geom_segment(aes(x =X, y = Y, xend = X, yend = Y, color = Fastest_compound),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("SLICK" = "gray", "INTERMEDIATE" = "#4E841A")) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 13, hjust = 0.5),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13)) +
  labs(title = "Slicks vs Intermediates", subtitle = "Lap 6", caption = "@f1.datascience") +
  theme(legend.position = "none")
p6

p7 <- ggplot(L7, aes(x = X, y =Y)) +
  geom_segment(aes(x =X, y = Y, xend = X, yend = Y, color = Fastest_compound),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("SLICK" = "gray", "INTERMEDIATE" = "#4E841A")) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 13, hjust = 0.5),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13)) +
  labs(title = "Slicks vs Intermediates", subtitle = "Lap 7", caption = "@f1.datascience") +
  theme(legend.position = "none")
p7


p8 <- ggplot(L8, aes(x = X, y =Y)) +
  geom_segment(aes(x =X, y = Y, xend = X, yend = Y, color = Fastest_compound),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("SLICK" = "gray", "INTERMEDIATE" = "#4E841A")) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 13, hjust = 0.5),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13)) +
  labs(title = "Slicks vs Intermediates", subtitle = "Lap 8", caption = "@f1.datascience") +
  theme(legend.position = "none")
p8

p9 <- ggplot(L9, aes(x = X, y =Y)) +
  geom_segment(aes(x =X, y = Y, xend = X, yend = Y, color = Fastest_compound),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("SLICK" = "gray", "INTERMEDIATE" = "#4E841A")) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 13, hjust = 0.5),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13)) +
  labs(title = "Slicks vs Intermediates", subtitle = "Lap 9", caption = "@f1.datascience") +
  theme(legend.position = "none")
p9

p10 <- ggplot(L10, aes(x = X, y =Y)) +
  geom_segment(aes(x =X, y = Y, xend = X, yend = Y, color = Fastest_compound),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("SLICK" = "gray", "INTERMEDIATE" = "#4E841A")) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 13, hjust = 0.5),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13)) +
  labs(title = "Slicks vs Intermediates", subtitle = "Lap 10", caption = "@f1.datascience") +
  theme(legend.position = "none")
p10

p11 <- ggplot(L11, aes(x = X, y =Y)) +
  geom_segment(aes(x =X, y = Y, xend = X, yend = Y, color = Fastest_compound),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("SLICK" = "gray", "INTERMEDIATE" = "#4E841A")) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 13, hjust = 0.5),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13)) +
  labs(title = "Slicks vs Intermediates", subtitle = "Lap 11", caption = "@f1.datascience") +
  theme(legend.position = "none")
p11

p12 <- ggplot(L12, aes(x = X, y =Y)) +
  geom_segment(aes(x =X, y = Y, xend = X, yend = Y, color = Fastest_compound),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("SLICK" = "gray", "INTERMEDIATE" = "#4E841A")) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 13, hjust = 0.5),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13)) +
  labs(title = "Slicks vs Intermediates", subtitle = "Lap 12", caption = "@f1.datascience") +
  theme(legend.position = "none")
p12


gapDutchQ<-read.csv("GapDut.csv")
pQualyM<-ggplot(gapDutchQ, aes(x=Gap,y=reorder(Driver,-Gap),fill=Driver))+ 
  geom_vline(xintercept = c(1,2,3,4,5,6,7,8,9,10,11,12,13), linetype = "dashed", color = "grey")+
  
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, 13, by = 1))+
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
  labs(caption = "@f1.datascience",subtitle = "2023 Dutch GP")+theme(legend.position = "none")
pQualyM

VERQ<-read.csv("telVERQDut.csv")
NORQ<-read.csv("telNORQDut.csv")
RUSQ<-read.csv("telRUSQDut.csv")
VERQ$faster <- NA

for (i in 1:nrow(VERQ)) {
  if (!is.na(VERQ$Speed[i]) & !is.na(NORQ$Speed[i]) & !is.na(RUSQ$Speed[i])) {
    if (VERQ$Speed[i] > NORQ$Speed[i] & VERQ$Speed[i] > RUSQ$Speed[i]) {
      VERQ$faster[i] <- "Verstappen is faster"
    } else if (NORQ$Speed[i] >= VERQ$Speed[i] & NORQ$Speed[i] >= RUSQ$Speed[i]) {
      VERQ$faster[i] <- "Norris is faster"
    } else if (RUSQ$Speed[i] > VERQ$Speed[i] & RUSQ$Speed[i] > NORQ$Speed[i]) {
      VERQ$faster[i] <- "Russell is faster"
    } else {
      VERQ$faster[i] <- "Norris is faster"
    }
  } else {
    VERQ$faster[i] <-"Norris is faster"
  }
}
LapDataVERDutch<- data.frame(X = VERQ$X, Y = VERQ$Y, Speed=VERQ$Speed, Faster=VERQ$faster)
pVERM<-ggplot(LapDataVERDutch, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Faster,group=Faster),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("Verstappen is faster" = "#3671C6", "Norris is faster" = "#F58020","Russell is faster"="#6CD3BF"))+
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
  
  labs(title="VER, NOR and RUS fastest lap comparison",subtitle="2023 Dutch GP",caption = "@f1.datascience")
pVERM

results<-read.csv("ResultsDUTR2023.csv")
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
  labs(title="Grid and race position",subtitle="2023 Dutch GP",caption = "@f1.datascience")
p
pos<-read.csv("Dutchpositions.csv")
# Filter the dataset for the top ten drivers
top10_drivers <- c("VER", "PER", "ALO", "SAI", "HAM", 
                   "GAS", "NOR", "ALB", "PIA", "OCO")  # Adjust with your top ten drivers

top10pos <- pos[pos$Driver %in% top10_drivers, ]

# Create a plot for top ten drivers
top10_plot <- ggplot(top10pos, aes(x = Lap, y = Position, color = Driver)) +
  geom_line(size = 1) +
  scale_y_reverse(breaks = seq(0, 20, by = 1)) +
  scale_x_continuous(limits = c(0, 72), breaks = seq(0, 72, by = 4)) +
  scale_color_manual(values = c("VER" = "#0600ef", "PER" = "#C92D4B", "ALO" = "#006f62", "SAI" = "#ff8181", "HAM" = "#00d2be",
                                "GAS" = "#0090ff", "NOR" = "#ff8700", "ALB" = "#37BEDD", "PIA" = "#eeb370", "OCO" = "#5E8FAA")) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
  ) +
  labs(caption = "@f1.datascience", subtitle = "2023 Dutch GP", title = "Position changes for the Top 10 in the race")

# Combine the plots using grid.arrange or other methods as needed
# Example: grid.arrange(ppos, top10_plot, ncol = 2)
top10_plot
