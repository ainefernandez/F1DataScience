library(ggplot2)
library(dplyr)
setwd("/Users/ainefernandez/documents/F1DataScience")
gapBAKUQ<-read.csv("BAKUQualy2.csv")

pQualy<-ggplot(gapBAKUQ, aes(x=Gap,y=reorder(Driver,-Gap),fill=Team))+ 
  geom_vline(xintercept = c(0.5,1, 1.5,2.0,2.5,3), linetype = "dashed", color = "grey")+
  geom_vline(xintercept = c(0.5,1, 1.5,2.0,2.5,3), linetype = "dashed", color = "grey")+
  
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
                               "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo "="#C92D4B"))+
  scale_x_continuous(breaks = seq(0, 3, by = 0.1))+
  scale_y_discrete(expand = c(0,0))+
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Gap to pole in Qualy")+
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
  labs(caption = "@f1.datascience",subtitle = "2023 Azerbaijan GP")+theme(legend.position = "none")
pQualy

LECBaku<-read.csv("telqualyLECBAKU2023.csv")
VERBaku<-read.csv("telqualyVERBAKU2023.csv")
PERBaku<-read.csv("telqualyPERBAKU2023.csv")
SAIBaku<-read.csv("telqualySAIBAKU2023.csv")
HAMBaku<-read.csv("telqualyHAMBAKU2023.csv")
ALOBaku<-read.csv("telqualyALOBAKU2023.csv")
NORBaku<-read.csv("telqualyNORBAKU2023.csv")
TSUBaku<-read.csv("telqualyTSUBAKU2023.csv")
RUSBaku<-read.csv("telqualyRUSBAKU2023.csv")
OCOBaku<-read.csv("telqualyOCOBAKU2023.csv")
ALBBaku<-read.csv("telqualyALBBAKU2023.csv")
BOTBaku<-read.csv("telqualyBOTBAKU2023.csv")
SARBaku<-read.csv("telqualySARBAKU2023.csv")
ZHOBaku<-read.csv("telqualyZHOBAKU2023.csv")
HULBaku<-read.csv("telqualyHULBAKU2023.csv")
MAGBaku<-read.csv("telqualyMAGBAKU2023.csv")

TSLEC<-max(LECBaku$Speed)
TSVER<-max(VERBaku$Speed)
TSPER<-max(PERBaku$Speed)
TSSAI<-max(SAIBaku$Speed)
TSHAM<-max(HAMBaku$Speed)
TSALO<-max(ALOBaku$Speed)
TSNOR<-max(NORBaku$Speed)
TSTSU<-max(TSUBaku$Speed)
TSSTR<-max(STRBaku$Speed)
TSPIA<-max(PIABaku$Speed)
TSRUS<-max(RUSBaku$Speed)
TSOCO<-max(OCOBaku$Speed)
TSALB<-max(ALBBaku$Speed)
TSBOT<-max(BOTBaku$Speed)
TSSAR<-max(SARBaku$Speed)
TSZHO<-max(ZHOBaku$Speed)
TSHUL<-max(HULBaku$Speed)
TSMAG<-max(MAGBaku$Speed)

TSFerrari<-max(TSLEC,TSSAI)
TSRedBull<-max(TSVER,TSPER)
TSAston<-max(TSALO,TSSTR)
TSMclaren<-max(TSNOR,TSPIA)
TSAlpine<-TSOCO
TSAlpha<-TSTSU
TSMercedes<-max(TSRUS,TSHAM)
TSHaas<-max(TSMAG,TSHUL)
TSWilliams<-max(TSSAR,TSALB)
TSAlfa<-max(TSBOT,TSZHO)

teams2<-c("Ferrari","Red Bull","Aston Martin","Mercedes","McLaren",
          "Alpine","AlphaTauri","Haas","Williams","Alfa Romeo")
TopSpeed<-c(TSFerrari,TSRedBull,TSAston,TSMercedes,TSMclaren,
            TSAlpine,TSAlpha,TSHaas,TSWilliams,TSAlfa)
topspeed <- data.frame(Team = teams2, TopSpeed = TopSpeed)


ggplot(topspeed, aes(x=reorder(Team, TopSpeed), y=TopSpeed, fill=Team)) +
  geom_bar(stat="identity", width=0.8) +
  scale_fill_manual(values=c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75","Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo"="#C92D4B")) +
  scale_y_continuous(breaks=seq(0, 350, by=50), expand=c(0, 0), limits=c(0, 350)) +
  theme_bw() + theme(panel.background=element_rect(fill="white", color="white"),
                     plot.background=element_rect(fill="white", color="white"),
                     plot.title=element_text(size=18, hjust=0.5),
                     plot.subtitle=element_text(size=15, hjust=0.5),
                     plot.caption=element_text(size=11, hjust=0.5),
                     legend.position="none",
                     axis.text.x=element_text(size=10)) +
  labs(x=" ", title="Top Speeds by team in Qualy", subtitle="2023 Azerbaijan GP", caption="@f1.datascience")







LECBaku$faster <- NA

for (i in 1:nrow(LECBaku)) {
  if (!is.na(VERBaku$Speed[i]) & !is.na(LECBaku$Speed[i]) & !is.na(PERBaku$Speed[i])) {
    if (VERBaku$Speed[i] > LECBaku$Speed[i] & VERBaku$Speed[i] > PERBaku$Speed[i]) {
      LECBaku$faster[i] <- "Verstappen is faster"
    } else if (LECBaku$Speed[i] > VERBaku$Speed[i] & LECBaku$Speed[i] > PERBaku$Speed[i]) {
      LECBaku$faster[i] <- "Leclerc is faster"
    } else if (PERBaku$Speed[i] > VERBaku$Speed[i] & PERBaku$Speed[i] > LECBaku$Speed[i]) {
      LECBaku$faster[i] <- "Pérez is faster"
    } else {
      LECBaku$faster[i] <- "Leclerc is faster"
    }
  } else {
    LECBaku$faster[i] <-"Leclerc is faster"
  }
}

LapDataLECBaku<- data.frame(X = LECBaku$X, Y = LECBaku$Y, Speed=LECBaku$Speed, Faster=LECBaku$faster)
pLECBaku<- ggplot(LapDataLECBaku, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Faster,group=Faster),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("Verstappen is faster" = "#3671C6", "Leclerc is faster" = "#F91536","Pérez is faster"="lightgreen","Same speed"="gold"))+
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
  
  labs(title="LEC, VER and PER fastest lap comparison",subtitle="2023 Azerbaijan GP",caption = "@f1.datascience")
pLECBaku



PIABaku<-read.csv("telqualyPIABAKU2023.csv")
STRBaku<-read.csv("telqualySTRBAKU2023.csv")
PIABaku$faster <- NA

for (i in 1:nrow(PIABaku)) {
  if (!is.na(PIABaku$Speed[i]) & !is.na(STRBaku$Speed[i])) {
    if (PIABaku$Speed[i] > STRBaku$Speed[i]) {
      PIABaku$faster[i] <- "Piastri is faster"
    }else{
      PIABaku$faster[i] <- "Stroll is faster" 
    }
  }
}


LapDataPIA<- data.frame(X = PIABaku$X, Y = PIABaku$Y, Speed=PIABaku$Speed, Faster=PIABaku$faster)
pPIA<- ggplot(LapDataPIA, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Faster),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("Piastri is faster" = "#F58020", "Stroll is faster" = "#358C75"))+
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
  
  labs(title="STR and PIA fastest lap comparison",subtitle="2023 Azerbaijan GP",caption = "@f1.datascience")
pPIA

gapBAKUSS<-read.csv("BAKUSS.csv")

pQualySS<-ggplot(gapBAKUSS, aes(x=Gap,y=reorder(Driver,-Gap),fill=Team))+ 
  geom_vline(xintercept = c(1,2,3,4,5), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
                               "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo"="#C92D4B"))+
  scale_x_continuous(breaks = seq(0, 5.6, by = 0.2))+
  scale_y_discrete(expand = c(0,0))+
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Gap to fastest with Mediums")+
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
  labs(caption = "@f1.datascience",subtitle = "2023 Azerbaijan GP (Sprint Shootout)")+theme(legend.position = "none")
pQualySS

sprint<-read.csv("BAKUS2023.csv")
drivers<-c("NOR","PIA","ZHO","BOT")
sprintdf <- sprint[sprint$Driver %in% drivers, ]
sprintshort<-na.omit(sprintdf)
sprintdf<-sprintshort[sprintshort$LapNumber>=7,]

sprintplot<-ggplot(sprintdf) +
  geom_line(size = 1,aes(x = LapNumber, y = LapTimeSeconds,col=Driver)) +
  geom_point(size=3,aes(x=LapNumber,y=LapTimeSeconds,col=Driver))+
  scale_x_continuous(breaks=seq(6, 17, by = 1))+
  scale_y_continuous(breaks=seq(100, 130, by = 2))+
  scale_color_manual(values=c("NOR"="#F58020","PIA"="#2095f5","ZHO"="#C92D4B","BOT"="#0039c7"))+
  theme_bw()+
  theme(plot.caption = element_text(size = 11, hjust=0.5),
        plot.title=element_text(size = 18, hjust = 0.5),
        axis.title = element_text(size = 13),
        plot.subtitle=element_text(size = 15, hjust = 0.5))+
  labs(x = "Lap", y = "Laptime (s)",title="NOR, PIA, ZHO and BOT laptime comparison",subtitle="2023 Arzerbaijan GP Sprint (7-17)",caption="@f1.datascience")
sprintplot


stints<-read.csv("StintsBAKU2023.csv")
stints$Driver<-factor(stints$Driver,levels=c("DEV","ZHO","BOT","HUL","SAR",
                                             "OCO","GAS","MAG","ALB","PIA",
                                             "TSU","NOR","RUS","STR","HAM",
                                             "SAI","ALO","LEC","VER","PER"))

stints$PIT<-factor(stints$PIT,levels=c("4.MEDIUM","3.SOFT","3.HARD","3.MEDIUM","2.SOFT","2.MEDIUM","2.HARD",
                                     "1.SOFT","1.MEDIUM","1.HARD"))
stintsBAKU<-ggplot(stints,aes(Driver,StintLength,fill=PIT))+
  geom_bar(stat="identity")+
  coord_flip()+
  scale_y_continuous(limits=c(0,52),breaks = seq(0, 52, by = 2))+
  scale_fill_manual(values=c("1.SOFT"="#FF3333","2.SOFT"="#FF3333","3.SOFT"="#FF3333","1.MEDIUM"="#ffe541","2.MEDIUM"="#ffe541","3.MEDIUM"="#ffe541","4.MEDIUM"="#ffe541",
                             "1.HARD"="#d2d2d2","2.HARD"="#d2d2d2","3.HARD"="#d2d2d2"))+
  labs(title="Tyre strategies",subtitle="2023 Azerbaijan GP",caption = "@f1.datascience",y="Lap")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title=element_text(size = 18, hjust = 0.5),
        plot.subtitle=element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust=0.5),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -20),size=12),
        axis.ticks.y =element_blank(),
        axis.title.y=element_blank(),
        legend.text = element_text(size=12), 
        legend.title = element_text(size=12))+theme(legend.position = "none")
stintsBAKU


resultsBaku<-read.csv("ResultsBAKUR2023.csv")
resultsBaku<-resultsBaku[c("Abbreviation","GridPosition","Position")]
names(resultsBaku)<-c("Driver","GridPos","RacePos")
resultsBaku$GridPos[resultsBaku$Driver=="HUL"]<-20
resultsBaku$GridPos[resultsBaku$Driver=="OCO"]<-19
left_label <- paste(resultsBaku$Driver, round(resultsBaku$GridPos),sep=", ")
right_label <- paste(resultsBaku$Driver, round(resultsBaku$RacePos),sep=", ")
p <- ggplot(resultsBaku) + 
  geom_segment(aes(x=1, xend=2, y=GridPos, yend=RacePos, col=Driver), size=1, show.legend=F) +
  geom_vline(xintercept=1, linetype="dashed", size=.1) +
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(values = c("VER"="#3671C6","PER"="#3671C6","ALO"="#358C75","SAI"="#F91536","HAM"="#6CD3BF","STR"="#358C75","RUS"="#6CD3BF","BOT"="#C92D4B","GAS"="#2293D1","ALB"="#37BEDD",
                                "TSU"="#5E8FAA","SAR"="#37BEDD","MAG"="#B6BABD","DEV"="#5E8FAA","HUL"="#B6BABD","ZHO"="#C92D4B","NOR"="#F58020","OCO"="#2293D1","LEC"="#F91536","PIA"="#F58020")) +
  scale_y_reverse() + 
  geom_text(aes(y = GridPos, x = 1, label = left_label), hjust = 1, size = 4) +
  geom_text(aes(y = RacePos, x = 2, label = right_label), hjust = 0, size = 4) +
  geom_text(label = "Grid Position", x = 1, y = max(resultsAUS$GridPos, resultsAUS$RacePos) * 1.1, hjust = 1.2, size = 5) +
  geom_text(label = "Race Position", x = 2, y = max(resultsAUS$GridPos, resultsAUS$RacePos) * 1.1, hjust = -0.1, size = 5) +
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
  labs(title="Grid and race position",subtitle="2023 Azerbaijan GP",caption = "@f1.datascience")
p



