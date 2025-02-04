library(ggplot2)
library(dplyr)
setwd("/Users/ainefernandez/documents/F1DataScience")
pos2008<-read.csv("Pos2008.csv")
left_label <- paste(pos2008$Driver, round(pos2008$PosBefore),sep=", ")
right_label <- paste(pos2008$Driver, round(pos2008$PosAfter),sep=", ")
p <- ggplot(pos2008) + 
  geom_segment(aes(x=1, xend=2, y=PosBefore, yend=PosAfter, col=Driver), size=1, show.legend=F) +
  geom_vline(xintercept=1, linetype="dashed", size=.1) +
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(values = c("RAI"="#C92D4B","GLO"="#2293D1","ROS"="#37BEDD",
                                "HAM"="#F58020","ALO"="#2293D1","MAS"="#F91536")) +
  scale_y_reverse() + 
  geom_text(aes(y = PosBefore, x = 1, label = left_label), hjust = 1, size = 3) +
  geom_text(aes(y = PosAfter, x = 2, label = right_label), hjust = 0, size = 3) +
  geom_text(label = "Position before SC", x = 1, y = max(pos2008$PosBefore, pos2008$PosAfter) * 1.1, hjust = 1.2, size = 5) +
  geom_text(label = "Position after SC", x = 2, y = max(pos2008$PosBefore, pos2008$PosAfter) * 1.1, hjust = -0.1, size = 5) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y=element_blank(),
        plot.title=element_text(size = 18, hjust = 0.5),
        plot.subtitle=element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust=0.5),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13))+
  labs(title="Position before and after SC",subtitle="2008 Singapore GP",caption = "@f1.datascience")
p 





monaco<-read.csv("laptimeSin2008.csv")
monlando<-ggplot(monaco, aes(x=Lap, y=LapTime, col=Driver)) +
  geom_line(size=1) +
  geom_point(size=3)+
  scale_color_manual(values=c("RAI"="#3671C6","HAM"="#F58020","MAS"="#F91536"))+
  scale_y_continuous(limits=c(105,116),breaks=seq(106, 116, by = 2))+
  scale_x_continuous(breaks=seq(1,12, by = 1))+
  theme_bw()+
  labs(x = "Lap", y = "Lap Time (s)",title="MAS,HAM and RAI laptimes comparison",subtitle ="2008 Singapore GP (1-12)",caption = "@f1.datascience" )+
  theme(panel.background=element_rect(fill="white", color="white"),
        plot.background=element_rect(fill="white", color="white"),
        plot.title=element_text(size=18, hjust=0.5),
        plot.subtitle=element_text(size=15, hjust=0.5),
        plot.caption=element_text(size=11, hjust=0.5))

monlando
