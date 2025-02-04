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






library(ggplot2)
library(dplyr)

Bahrainspeed <-LapDataMAX
Bahrainspeed$Type <- NA

for (i in 1:nrow(Bahrainspeed)) {
  if (!is.na(Bahrainspeed$Speed[i])) {
    if (Bahrainspeed$Speed[i] >= 240) {
      Bahrainspeed$Type[i] <- "High Speed"
    } else if (Bahrainspeed$Speed[i] >= 200 & Bahrainspeed$Speed[i] < 240) {
      Bahrainspeed$Type[i] <- "Medium Speed"
    } else if (Bahrainspeed$Speed[i] < 200) {
      Bahrainspeed$Type[i] <- "Low Speed"
    }
  }
}



pMAX<- ggplot(Bahrainspeed, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed"))+
  coord_equal() + coord_flip() +scale_y_reverse()+
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
  
  labs(title="Bahrain International Circuit",subtitle="Speed",caption = "@f1.datascience")
pMAX

jedahspeed <-LapDataCHECO
jedahspeed$Type <- NA

for (i in 1:nrow(jedahspeed)) {
  if (!is.na(jedahspeed$Speed[i])) {
    if (jedahspeed$Speed[i] >= 240) {
      jedahspeed$Type[i] <- "High Speed"
    } else if (jedahspeed$Speed[i] >= 200 & jedahspeed$Speed[i] < 240) {
      jedahspeed$Type[i] <- "Medium Speed"
    } else if (jedahspeed$Speed[i] < 200) {
      jedahspeed$Type[i] <- "Low Speed"
    }
  }
}

jedahspeedData <- data.frame(X = jedahspeed$X, Y = jedahspeed$Y, Speed = jedahspeed$Speed, Type = jedahspeed$Type)

pCHECO <- ggplot(jedahspeedData, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed")) +
  coord_equal() + coord_flip() + scale_y_reverse() +
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
  labs(title = "Jeddah Corniche Circuit", subtitle = "Speed", caption = "@f1.datascience")

pCHECO

LapDataMAX2<- data.frame(X = telVERQAustralia$X, Y = telVERQAustralia$Y, Speed=telVERQAustralia$Speed, Faster=telVERQAustralia$faster)
australia<-LapDataMAX2
australia$Type <- NA

for (i in 1:nrow(australia)) {
  if (!is.na(australia$Speed[i])) {
    if (australia$Speed[i] >= 240) {
      australia$Type[i] <- "High Speed"
    } else if (australia$Speed[i] >= 200 & australia$Speed[i] < 240) {
      australia$Type[i] <- "Medium Speed"
    } else if (australia$Speed[i] < 200) {
      australia$Type[i] <- "Low Speed"
    }
  }
}

australiaData <- data.frame(X = australia$X, Y = australia$Y, Speed = australia$Speed, Type = australia$Type)
albert <- ggplot(australiaData, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed")) +
  coord_equal() + coord_flip() + scale_y_reverse() +
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
  labs(title = "Melbourne Grand Prix Circuit", subtitle = "Speed", caption = "@f1.datascience")

albert

japan<-LapDataVERJAP
japan$Type <- NA

for (i in 1:nrow(japan)) {
  if (!is.na(japan$Speed[i])) {
    if (japan$Speed[i] >= 240) {
      japan$Type[i] <- "High Speed"
    } else if (japan$Speed[i] >= 200 & japan$Speed[i] < 240) {
      japan$Type[i] <- "Medium Speed"
    } else if (japan$Speed[i] < 200) {
      japan$Type[i] <- "Low Speed"
    }
  }
}

japanData <- data.frame(X = japan$X, Y = japan$Y, Speed = japan$Speed, Type = japan$Type)
suzuka<- ggplot(japanData, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed")) +
  coord_equal() + coord_flip() + scale_y_reverse() +
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
  labs(title = "Suzuka International Racing Course", subtitle = "Speed", caption = "@f1.datascience")

suzuka
PERQ<-read.csv("PERQMIA.csv")
LapDataPERMiami<- data.frame(X = PERQ$X, Y = PERQ$Y, Speed=PERQ$Speed)
miami<-LapDataPERMiami
miami$Type <- NA

for (i in 1:nrow(miami)) {
  if (!is.na(miami$Speed[i])) {
    if (miami$Speed[i] >= 240) {
      miami$Type[i] <- "High Speed"
    } else if (miami$Speed[i] >= 200 & miami$Speed[i] < 240) {
      miami$Type[i] <- "Medium Speed"
    } else if (miami$Speed[i] < 200) {
      miami$Type[i] <- "Low Speed"
    }
  }
}

miamiData <- data.frame(X = miami$X, Y = miami$Y, Speed = miami$Speed, Type = miami$Type)
miamiC<- ggplot(miamiData, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed")) +
  coord_equal() +
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
  labs(title = "Miami International Autodrome", subtitle = "Speed", caption = "@f1.datascience")

miamiC


HAMQ<-read.csv("HAMQIMO.csv")
LapDataHAMImo<- data.frame(X = HAMQ$X, Y = HAMQ$Y, Speed=HAMQ$Speed)
imola<-LapDataPERMiami
imola$Type <- NA

for (i in 1:nrow(imola)) {
  if (!is.na(imola$Speed[i])) {
    if (imola$Speed[i] >= 240) {
      imola$Type[i] <- "High Speed"
    } else if (imola$Speed[i] >= 200 & imola$Speed[i] < 240) {
      imola$Type[i] <- "Medium Speed"
    } else if (imola$Speed[i] < 200) {
      imola$Type[i] <- "Low Speed"
    }
  }
}

imolaData <- data.frame(X = imola$X, Y = imola$Y, Speed = imola$Speed, Type = imola$Type)
imolaC<- ggplot(imolaData, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed")) +
  coord_equal() +
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
  labs(title = "Autodromo Enzo e Dino Ferrari", subtitle = "Speed", caption = "@f1.datascience")

imolaC


VERQ<-read.csv("telVERQMonaco.csv")
LapDataVERMonaco<- data.frame(X = VERQ$X, Y = VERQ$Y, Speed=VERQ$Speed)
monaco<-LapDataVERMonaco
monaco$Type <- NA

for (i in 1:nrow(monaco)) {
  if (!is.na(monaco$Speed[i])) {
    if (monaco$Speed[i] >= 240) {
      monaco$Type[i] <- "High Speed"
    } else if (monaco$Speed[i] >= 200 & monaco$Speed[i] < 240) {
      monaco$Type[i] <- "Medium Speed"
    } else if (monaco$Speed[i] < 200) {
      monaco$Type[i] <- "Low Speed"
    }
  }
}

monacoData <- data.frame(X = monaco$X, Y = monaco$Y, Speed = monaco$Speed, Type = monaco$Type)
monacoC<- ggplot(monacoData, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed")) +
  coord_equal() +
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
  labs(title = "Circuit de Monaco", subtitle = "Speed", caption = "@f1.datascience")

monacoC


VERQ<-read.csv("VERQCAN.csv")
LapDataVERCANADA<- data.frame(X = VERQ$X, Y = VERQ$Y, Speed=VERQ$Speed)
canada<-LapDataVERCANADA
canada$Type <- NA

for (i in 1:nrow(canada)) {
  if (!is.na(canada$Speed[i])) {
    if (canada$Speed[i] >= 240) {
      canada$Type[i] <- "High Speed"
    } else if (canada$Speed[i] >= 200 & canada$Speed[i] < 240) {
      canada$Type[i] <- "Medium Speed"
    } else if (canada$Speed[i] < 200) {
      canada$Type[i] <- "Low Speed"
    }
  }
}

canadaData <- data.frame(X = canada$X, Y = canada$Y, Speed = canada$Speed, Type = canada$Type)
canadaC<- ggplot(canadaData, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed")) +
  coord_equal() + coord_flip()+
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
  labs(title = "Circuit Gilles-Villeneuve", subtitle = "Speed", caption = "@f1.datascience")

canadaC

spain<-LapDataVERSpain
spain$Type <- NA

for (i in 1:nrow(spain)) {
  if (!is.na(spain$Speed[i])) {
    if (spain$Speed[i] >= 240) {
      spain$Type[i] <- "High Speed"
    } else if (spain$Speed[i] >= 200 & spain$Speed[i] < 240) {
      spain$Type[i] <- "Medium Speed"
    } else if (spain$Speed[i] < 200) {
      spain$Type[i] <- "Low Speed"
    }
  }
}

spainData <- data.frame(X = spain$X, Y = spain$Y, Speed = spain$Speed, Type = spain$Type)
spainC<- ggplot(spainData, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed")) +
  coord_equal() +
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
  labs(title = "Circuit de Barcelona-Catalunya", subtitle = "Speed", caption = "@f1.datascience")

spainC





VERQ<-read.csv("VERQAUS.csv")
LapDataVERAustria<- data.frame(X = VERQ$X, Y = VERQ$Y, Speed=VERQ$Speed)
austria<-LapDataVERAustria
austria$Type <- NA

for (i in 1:nrow(austria)) {
  if (!is.na(austria$Speed[i])) {
    if (austria$Speed[i] >= 240) {
      austria$Type[i] <- "High Speed"
    } else if (austria$Speed[i] >= 200 & austria$Speed[i] < 240) {
      austria$Type[i] <- "Medium Speed"
    } else if (austria$Speed[i] < 200) {
      austria$Type[i] <- "Low Speed"
    }
  }
}

austriaData <- data.frame(X = austria$X, Y = austria$Y, Speed = austria$Speed, Type = austria$Type)
austriaC<- ggplot(austriaData, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed")) +
  coord_equal() + coord_flip()+
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
  labs(title = "Red Bull Ring", subtitle = "Speed", caption = "@f1.datascience")

austriaC



VERQ<-read.csv("VERQSil.csv")
LapDataVERSil<- data.frame(X = VERQ$X, Y = VERQ$Y, Speed=VERQ$Speed)
sil<-LapDataVERSil
sil$Type <- NA

for (i in 1:nrow(sil)) {
  if (!is.na(sil$Speed[i])) {
    if (sil$Speed[i] >= 240) {
      sil$Type[i] <- "High Speed"
    } else if (sil$Speed[i] >= 200 & sil$Speed[i] < 240) {
      sil$Type[i] <- "Medium Speed"
    } else if (sil$Speed[i] < 200) {
      sil$Type[i] <- "Low Speed"
    }
  }
}

silData <- data.frame(X = sil$X, Y = sil$Y, Speed = sil$Speed, Type = sil$Type)
silC<- ggplot(silData, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed")) +
  coord_flip() + coord_equal()+
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
  labs(title = "Silverstone Circuit", subtitle = "Speed", caption = "@f1.datascience")

silC


VERQ<-read.csv("HAMQHUN.csv")
LapDataVERHUN<- data.frame(X = VERQ$X, Y = VERQ$Y, Speed=VERQ$Speed)
hungary<-LapDataVERHUN
hungary$Type <- NA

for (i in 1:nrow(hungary)) {
  if (!is.na(hungary$Speed[i])) {
    if (hungary$Speed[i] >= 240) {
      hungary$Type[i] <- "High Speed"
    } else if (hungary$Speed[i] >= 200 & hungary$Speed[i] < 240) {
      hungary$Type[i] <- "Medium Speed"
    } else if (hungary$Speed[i] < 200) {
      hungary$Type[i] <- "Low Speed"
    }
    
  }
}

hungaryData <- data.frame(X = hungary$X, Y = hungary$Y, Speed = hungary$Speed, Type = hungary$Type)
hungaryC<- ggplot(hungaryData, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed")) +
  coord_flip() + coord_equal()+
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
  labs(title = "Hungaroring", subtitle = "Speed", caption = "@f1.datascience")

hungaryC


VERQ<-read.csv("VERQBEL.csv")
LapDataVERBEL<- data.frame(X = VERQ$X, Y = VERQ$Y, Speed=VERQ$Speed)
spa<-LapDataVERBEL
spa$Type <- NA

for (i in 1:nrow(spa)) {
  if (!is.na(spa$Speed[i])) {
    if (spa$Speed[i] >= 240) {
      spa$Type[i] <- "High Speed"
    } else if (spa$Speed[i] >= 200 & spa$Speed[i] < 240) {
      spa$Type[i] <- "Medium Speed"
    } else if (spa$Speed[i] < 200) {
      spa$Type[i] <- "Low Speed"
    }
    
  }
}

spaData <- data.frame(X = spa$X, Y = spa$Y, Speed = spa$Speed, Type = spa$Type)
spaC<- ggplot(spaData, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed")) +
  coord_flip() + coord_equal()+
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
  labs(title = "Circuit de Spa-Francorchamps", subtitle = "Speed", caption = "@f1.datascience")

spaC


VERQ<-read.csv("VERQNET.csv")
LapDataVERNET<- data.frame(X = VERQ$X, Y = VERQ$Y, Speed=VERQ$Speed)
net<-LapDataVERNET
net$Type <- NA

for (i in 1:nrow(net)) {
  if (!is.na(net$Speed[i])) {
    if (net$Speed[i] >= 240) {
      net$Type[i] <- "High Speed"
    } else if (net$Speed[i] >= 200 & net$Speed[i] < 240) {
      net$Type[i] <- "Medium Speed"
    } else if (net$Speed[i] < 200) {
      net$Type[i] <- "Low Speed"
    }
    
  }
}

netData <- data.frame(X = net$X, Y = net$Y, Speed = net$Speed, Type = net$Type)
netC<- ggplot(netData, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed")) +
  coord_equal()+
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
  labs(title = "Circuit Zandvoort", subtitle = "Speed", caption = "@f1.datascience")

netC



VERQ<-read.csv("VERQMonza.csv")
LapDataVERMonza<- data.frame(X = VERQ$X, Y = VERQ$Y, Speed=VERQ$Speed)
monza<-LapDataVERMonza
monza$Type <- NA

for (i in 1:nrow(monza)) {
  if (!is.na(monza$Speed[i])) {
    if (monza$Speed[i] >= 240) {
      monza$Type[i] <- "High Speed"
    } else if (monza$Speed[i] >= 200 & monza$Speed[i] < 240) {
      monza$Type[i] <- "Medium Speed"
    } else if (monza$Speed[i] < 200) {
      monza$Type[i] <- "Low Speed"
    }
    
  }
}

monzaData <- data.frame(X = monza$X, Y = monza$Y, Speed = monza$Speed, Type = monza$Type)
monzaC<- ggplot(monzaData, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed")) +
  coord_equal()+
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
  labs(title = "Autodromo Nazionale Monza", subtitle = "Speed", caption = "@f1.datascience")

monzaC


VERQ<-read.csv("VERQbaku.csv")
LapDataVERbaku<- data.frame(X = VERQ$X, Y = VERQ$Y, Speed=VERQ$Speed)
baku<-LapDataVERbaku
baku$Type <- NA

for (i in 1:nrow(baku)) {
  if (!is.na(baku$Speed[i])) {
    if (baku$Speed[i] >= 240) {
      baku$Type[i] <- "High Speed"
    } else if (baku$Speed[i] >= 200 & baku$Speed[i] < 240) {
      baku$Type[i] <- "Medium Speed"
    } else if (baku$Speed[i] < 200) {
      baku$Type[i] <- "Low Speed"
    }
    
  }
}

bakuData <- data.frame(X = baku$X, Y = baku$Y, Speed = baku$Speed, Type = baku$Type)
bakuC<- ggplot(bakuData, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed")) +
  coord_flip() + coord_equal()+
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
  labs(title = "Baku City Circuit", subtitle = "Speed", caption = "@f1.datascience")

bakuC



VERQ<-read.csv("VERQSin.csv")
LapDataVERSin<- data.frame(X = VERQ$X, Y = VERQ$Y, Speed=VERQ$Speed)
Sin<-LapDataVERSin
Sin$Type <- NA

for (i in 1:nrow(Sin)) {
  if (!is.na(Sin$Speed[i])) {
    if (Sin$Speed[i] >= 240) {
      Sin$Type[i] <- "High Speed"
    } else if (Sin$Speed[i] >= 200 & Sin$Speed[i] < 240) {
      Sin$Type[i] <- "Medium Speed"
    } else if (Sin$Speed[i] < 200) {
      Sin$Type[i] <- "Low Speed"
    }
    
  }
}

SinData <- data.frame(X = Sin$X, Y =Sin$Y, Speed = Sin$Speed, Type = Sin$Type)
SinC<- ggplot(SinData, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed")) +
  coord_flip() + coord_equal()+
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
  labs(title = "Marina Bay Street Circuit", subtitle = "Speed", caption = "@f1.datascience")

SinC



VERQ<-read.csv("VERQaustin.csv")
LapDataVERaustin<- data.frame(X = VERQ$X, Y = VERQ$Y, Speed=VERQ$Speed)
austin<-LapDataVERaustin
austin$Type <- NA

for (i in 1:nrow(austin)) {
  if (!is.na(austin$Speed[i])) {
    if (austin$Speed[i] >= 240) {
      austin$Type[i] <- "High Speed"
    } else if (austin$Speed[i] >= 200 & austin$Speed[i] < 240) {
      austin$Type[i] <- "Medium Speed"
    } else if (austin$Speed[i] < 200) {
      austin$Type[i] <- "Low Speed"
    }
    
  }
}

austinData <- data.frame(X = austin$X, Y =austin$Y, Speed = austin$Speed, Type = austin$Type)
austinC<- ggplot(austinData, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed")) +
  coord_flip() + coord_equal()+
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
  labs(title = "Circuit of The Americas", subtitle = "Speed", caption = "@f1.datascience")

austinC


VERQ<-read.csv("VERQMex.csv")
LapDataVERMex<- data.frame(X = VERQ$X, Y = VERQ$Y, Speed=VERQ$Speed)
mex<-LapDataVERMex
mex$Type <- NA

for (i in 1:nrow(mex)) {
  if (!is.na(mex$Speed[i])) {
    if (mex$Speed[i] >= 240) {
      mex$Type[i] <- "High Speed"
    } else if (mex$Speed[i] >= 200 & mex$Speed[i] < 240) {
      mex$Type[i] <- "Medium Speed"
    } else if (mex$Speed[i] < 200) {
      mex$Type[i] <- "Low Speed"
    }
    
  }
}

mexData <- data.frame(X = mex$X, Y =mex$Y, Speed = mex$Speed, Type = mex$Type)
mexC<- ggplot(mexData, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed")) +
  coord_flip() + coord_equal()+
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
  labs(title = "Autódromo Hermanos Rodríguez", subtitle = "Speed", caption = "@f1.datascience")

mexC


VERQ<-read.csv("VERQBra.csv")
LapDataVERBra<- data.frame(X = VERQ$X, Y = VERQ$Y, Speed=VERQ$Speed)
bra<-LapDataVERBra
bra$Type <- NA

for (i in 1:nrow(bra)) {
  if (!is.na(bra$Speed[i])) {
    if (bra$Speed[i] >= 240) {
      bra$Type[i] <- "High Speed"
    } else if (bra$Speed[i] >= 200 & bra$Speed[i] < 240) {
      bra$Type[i] <- "Medium Speed"
    } else if (bra$Speed[i] < 200) {
      bra$Type[i] <- "Low Speed"
    }
    
  }
}

braData <- data.frame(X = bra$X, Y =bra$Y, Speed = bra$Speed, Type = bra$Type)
braC<- ggplot(braData, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed")) +
  coord_flip() + coord_equal()+
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
  labs(title = "Autódromo José Carlos Pace", subtitle = "Speed", caption = "@f1.datascience")

braC


VERQ<-read.csv("VERQVegas.csv")
LapDataVERVegas<- data.frame(X = VERQ$X, Y = VERQ$Y, Speed=VERQ$Speed)
vegas<-LapDataVERVegas
vegas$Type <- NA

for (i in 1:nrow(vegas)) {
  if (!is.na(vegas$Speed[i])) {
    if (vegas$Speed[i] >= 240) {
      vegas$Type[i] <- "High Speed"
    } else if (vegas$Speed[i] >= 200 & vegas$Speed[i] < 240) {
      vegas$Type[i] <- "Medium Speed"
    } else if (vegas$Speed[i] < 200) {
      vegas$Type[i] <- "Low Speed"
    }
    
  }
}

vegasData <- data.frame(X = vegas$X, Y =vegas$Y, Speed = vegas$Speed, Type =vegas$Type)
vegasC<- ggplot(vegasData, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed")) +
  coord_flip() + coord_equal()+
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
  labs(title = "Las Vegas Strip Circuit", subtitle = "Speed", caption = "@f1.datascience")

vegasC



VERQ<-read.csv("VERQqatar.csv")
LapDataVERqatar<- data.frame(X = VERQ$X, Y = VERQ$Y, Speed=VERQ$Speed)
qatar<-LapDataVERqatar
qatar$Type <- NA

for (i in 1:nrow(qatar)) {
  if (!is.na(qatar$Speed[i])) {
    if (qatar$Speed[i] >= 240) {
      qatar$Type[i] <- "High Speed"
    } else if (qatar$Speed[i] >= 200 & qatar$Speed[i] < 240) {
      qatar$Type[i] <- "Medium Speed"
    } else if (qatar$Speed[i] < 200) {
      qatar$Type[i] <- "Low Speed"
    }
    
  }
}

qatarData <- data.frame(X = qatar$X, Y =qatar$Y, Speed = qatar$Speed, Type =qatar$Type)
qatarC<- ggplot(qatarData, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed")) +
  coord_flip() + coord_equal()+
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
  labs(title = "Lusail International Circuit", subtitle = "Speed", caption = "@f1.datascience")

qatarC



VERQ<-read.csv("VERQabu.csv")
LapDataVERabu<- data.frame(X = VERQ$X, Y = VERQ$Y, Speed=VERQ$Speed)
abu<-LapDataVERabu
abu$Type <- NA

for (i in 1:nrow(abu)) {
  if (!is.na(abu$Speed[i])) {
    if (abu$Speed[i] >= 240) {
      abu$Type[i] <- "High Speed"
    } else if (abu$Speed[i] >= 200 & abu$Speed[i] < 240) {
      abu$Type[i] <- "Medium Speed"
    } else if (abu$Speed[i] < 200) {
      abu$Type[i] <- "Low Speed"
    }
    
  }
}

abuData <- data.frame(X = abu$X, Y =abu$Y, Speed = abu$Speed, Type =abu$Type)
abuC<- ggplot(abuData, aes(x = X, y = Y)) +
  geom_path(size = 8, linetype = "solid", color = "black") +
  geom_segment(aes(x = lag(X), y = lag(Y), xend = X, yend = Y, color = Type),
               size = 3, lineend = "round") +
  scale_color_manual(values = c("High Speed" = "#63B234", "Medium Speed" = "#FF8200","Low Speed"="#EE232C"),
                     breaks = c("High Speed", "Medium Speed", "Low Speed")) +
  coord_equal()+
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
  labs(title = "Yas Marina Circuit", subtitle = "Speed", caption = "@f1.datascience")

abuC
