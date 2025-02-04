library(ggplot2)
library(dplyr)
library(tidyr)
library(gganimate)
setwd("/Users/ainefernandez/documents/F1DataScience")
drivercolors <- c("VER" = "#3671C6","LAW" = "#3671C6","ALO" = "#358C75","HAM" = "#F91536","ANT" = "#6CD3BF",
                  "STR" = "#358C75","RUS" = "#6CD3BF","HUL" = "#04C404","GAS" = "#FF66C4","ALB" = "#37BEDD","SAI"="#37BEDD",
                  "TSU" = "#0335D3","OCO" = "#B6BABD","BEA" = "#B6BABD",
                  "BOR" = "#04C404", "NOR" = "#F58020","DOO" = "#FF66C4","LEC" = "#F91536","PIA" = "#F58020", "HAD"="#0335D3")
teamscolors<-c("Red Bull Racing"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD","Aston Martin"="#358C75",
               "Alpine"="#FF66C4","Haas F1 Team"="#B6BABD","RB"="#0335D3","McLaren"="#F58020","Kick Sauber"="#04C404","AlphaTauri"="#5E8FAA","Alfa Romeo"="#C92D4B")

gapSingaporeQ<-read.csv("FSAVGQUALY.csv")
gapSingaporeQ2<-read.csv("FSAVGRACE.csv")
gapSingaporeQ<-gapSingaporeQ[gapSingaporeQ$Driver!="BEA",]
gapSingaporeQ<-gapSingaporeQ[gapSingaporeQ$Driver!="DOO",]
gapSingaporeQ<-gapSingaporeQ[gapSingaporeQ$Driver!="SAR",]
gapSingaporeQ<-gapSingaporeQ[gapSingaporeQ$Driver!="RIC",]

pQualyS<-ggplot(gapSingaporeQ, aes(x=Position,y=reorder(Driver,-Position),fill=Driver))+ 
  geom_vline(xintercept = seq(0, 18, by = 2), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, 18, by = 2))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(Position,1)), hjust = -0.1, size = 4) +
  xlab("Average position") +
  ylab("Driver") +
  ggtitle("Average qualifying position")+
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
  labs(caption = " ",subtitle = "2024 Season")+theme(legend.position = "none")
pQualyS

gapSingaporeQ<-read.csv("pointstotal.csv")
gapSingaporeQ<-gapSingaporeQ[gapSingaporeQ$Driver!="BEA",]
pQualyS<-ggplot(gapSingaporeQ, aes(x=AvgPoints,y=reorder(Driver,AvgPoints),fill=Driver))+ 
  geom_vline(xintercept = seq(0, 20, by = 2), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, 20, by = 2))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(AvgPoints,1)), hjust = -0.1, size = 4) +
  xlab("Average points") +
  ylab("Driver") +
  ggtitle("Average points by GP")+
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
  labs(caption = "",subtitle = "2024 Season")+theme(legend.position = "none")
pQualyS


gapSingaporeQ<-read.csv("fastestlaps2024.csv")
pQualyS<-ggplot(gapSingaporeQ, aes(x=FastestLaps,y=reorder(Driver,FastestLaps),fill=Driver))+ 
  geom_vline(xintercept = seq(0, 6, by = 1), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0,6, by = 1))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(FastestLaps,1)), hjust = -0.1, size = 4) +
  xlab("Fastest Laps") +
  ylab("Driver") +
  ggtitle("Fastest Laps")+
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
  labs(caption = "",subtitle = "2024 Season")+theme(legend.position = "none")
pQualyS





gapSingaporeQ<-read.csv("2025gridRaceStarts.csv")
pQualyS<-ggplot(gapSingaporeQ, aes(x=RaceStarts,y=reorder(Driver,RaceStarts),fill=Driver))+ 
  geom_vline(xintercept = seq(0, 425, by = 25), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, 425, by = 25),limits=c(0,425))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(RaceStarts,1)), hjust = -0.1, size = 4) +
  xlab("Race Starts") +
  ylab("Driver") +
  ggtitle("Race Starts")+
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
  labs(caption = "",subtitle = "2025 Grid")+theme(legend.position = "none")
pQualyS

df<-read.csv("teamsdrivers.csv")
df$TeamOrder <- paste(df$Order,df$Team, sep = " ")
unique(df$TeamOrder)
order_levels <- c(
  "3 Racing Bulls", "2 RB", "1 AlphaTauri", "3 Aston Martin", 
  "2 Racing Point", "5 Williams", "4 Ferrari", "5 Haas", 
  "4 Alpine", "3 Renault", "1 Manor", "2 Ferrari", 
  "1 Sauber", "1 RB", "7 Kick Sauber", "6 Haas", 
  "5 Renault", "4 Force India", "3 Sauber", "2 Force India", 
  "1 Williams", "3 Ferrari", "2 Mercedes", "1 McLaren", 
  "1 Racing Bulls", "5 Alpine", "4 AlphaTauri", "3 Toro Rosso", 
  "1 Alpine", "1 Kick Sauber", "1 Haas", "1 Mercedes", 
  "8 Aston Martin", "7 Alpine", "6 McLaren", "5 Ferrari", 
  "4 Renault", "3 McLaren", "2 Renault", "1 Minardi", 
  "3 Williams", "2 Red Bull", "1 Toro Rosso"
)
team_colors <- c(
  "1 Toro Rosso" = "#5E8FAA", 
  "2 Red Bull" = "#3671C6", 
  "3 Williams" = "#37BEDD", 
  "1 Minardi" = "#FFCD0678", 
  "2 Renault" = "#FFCD00", 
  "3 McLaren" = "#F58020", 
  "4 Renault" = "#FFCD00", 
  "5 Ferrari" = "#F91536", 
  "6 McLaren" = "#F58020", 
  "7 Alpine" = "#FF66C4", 
  "8 Aston Martin" = "#358C75", 
  "1 Mercedes" = "#6CD3BF", 
  "1 Haas" = "#B6BABD", 
  "1 Kick Sauber" = "#04C404", 
  "1 Alpine" = "#FF66C4", 
  "3 Toro Rosso" = "#5E8FAA", 
  "4 AlphaTauri" = "#5E8FAA", 
  "5 Alpine" = "#FF66C4", 
  "1 Racing Bulls" = "#0335D898", 
  "1 McLaren" = "#F58020", 
  "2 Mercedes" = "#6CD3BF", 
  "3 Ferrari" = "#F91536", 
  "1 Williams" = "#37BEDD", 
  "2 Force India" = "salmon", 
  "3 Sauber" = "#B6BABD67", 
  "4 Force India" = "salmon", 
  "5 Renault" = "#FFCD00", 
  "6 Haas" = "#B6BABD", 
  "7 Kick Sauber" = "#04C404", 
  "1 RB" = "#0335D3", 
  "1 Sauber" = "#B6BABD67", 
  "2 Ferrari" = "#F91536", 
  "1 Manor" = "#F5802989", 
  "3 Renault" = "#FFCD00", 
  "4 Alpine" = "#FF66C4", 
  "5 Haas" = "#B6BABD", 
  "4 Ferrari" = "#F91536", 
  "5 Williams" = "#37BEDD", 
  "2 Racing Point" = "#FF66C445", 
  "3 Aston Martin" = "#358C75", 
  "1 AlphaTauri" = "#5E8FAA", 
  "2 RB" = "#0335D3", 
  "3 Racing Bulls" = "#0335D898"
)

# Asigna los niveles correctos y en orden al TeamOrder
df$TeamOrder <- factor(df$TeamOrder, levels = order_levels)

# Verifica los niveles de TeamOrder en orden
levels(df$TeamOrder)

gridseasons <- ggplot(df, aes(Driver, Seasons, fill = TeamOrder)) +
  geom_bar(stat = "identity",color="black") +
  coord_flip() +
  scale_y_continuous(limits = c(0,22), breaks = seq(0, 22, by = 2)) +
  scale_fill_manual(values = team_colors) +
  labs(title = " ", subtitle = " ", caption = " ", y = "Seasons") +
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

gridseasons




# Definir colores para los equipos (si no lo has hecho ya)
teamscolors <- c(
  "Toro Rosso" = "#5E8FAA",   # Similar a AlphaTauri
  "Red Bull" = "#3671C6",      # Red Bull Racing
  "Williams" = "#37BEDD",      # Williams
  "Minardi" = "#FFCD0678",       # Ferrari (Minardi fue anteriormente parte de la marca Ferrari)
  "Renault" = "#FFCD00",       # Alpine
  "McLaren" = "#F58020",       # McLaren
  "Ferrari" = "#F91536",       # Ferrari
  "Alpine" = "#FF66C4",        # Alpine
  "Aston Martin" = "#358C75",  # Aston Martin
  "Mercedes" = "#6CD3BF",     # Mercedes
  "Haas" = "#B6BABD",          # Haas F1 Team
  "AlphaTauri" = "#5E8FAA",    # AlphaTauri
  "Force India" = "salmon",   # Alpine (antes Force India)
  "Kick Sauber" = "#04C404",   # Sauber
  "Manor" = "#F5802989",         # Ferrari (similar color)
  "Racing Point" = "#FF66C445",  # Alpine (antes Racing Point)
  "Sauber" = "#B6BABD67",        # Haas (similarly light color for Sauber)
  "RB" = "#0335D3",            # RB (Red Bull color)
  "Racing Bulls" = "#0335D898"     # Alfa Romeo
)
df$Team_Order <- paste(df$Team, df$Order)

# Asegurarse de que el factor se ordene de acuerdo con el orden de los stints
df$Team_Order <- factor(df$Team_Order, 
                        levels = unique(df$Team_Order[order(df$Order)]))

# Ahora usamos ggplot para crear la gráfica de barras apiladas
library(ggplot2)

ggplot(df, aes(x = Seasons, fill = Team_Order)) +
  geom_bar(position = "stack") +
  labs(x = "Seasons", y = "Driver") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Ajuste de ángulo para las etiquetas del eje X



racetype<-read.csv('FSRaceType.csv')
racetype<-racetype[racetype$Driver!="BEA",]
racetype<-racetype[racetype$Driver!="SAR",]
racetype<-racetype[racetype$Driver!="DOO",]
racetype<-racetype[racetype$Driver!="RIC",]
racetype$Driver<-factor(racetype$Driver,levels=c("BOT","LAW","ZHO","COL","RIC","ALB",
                                                   "MAG","OCO","STR","TSU",
                                                   "HUL","GAS","ALO","PER","HAM",
                                                   "RUS","SAI","PIA","LEC","NOR","VER"))
racetype$Finish<-factor(racetype$Finish,levels=c("Win","Podium","Points","Out of the points","DNF","DSQ"))
racetypeplot <- ggplot(racetype, aes(x = Race, y = Driver, fill = Finish)) +
  geom_bar(stat="identity",color="black") + 
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, by = 2)) +
  scale_y_discrete(expand = c(0, 0))+
  scale_fill_manual(values = c( "Win"="gold","Podium"="#3C70B5", "Points"="#78D07E","Out of the points"="#E55B4A","DNF"="#E06A9B","DSQ"="black")) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Race outcomes by driver") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 14, hjust = 0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(margin = margin(r = -25),size=13),
        axis.title = element_text(size = 14),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15)) +
  labs(caption = " ", subtitle = "2024 Season")
racetypeplot

qualytype<-read.csv('FSQUALYtype.csv')
qualytype<-qualytype[qualytype$Driver!="BEA",]
qualytype<-qualytype[qualytype$Driver!="SAR",]
qualytype<-qualytype[qualytype$Driver!="DOO",]
qualytype<-qualytype[qualytype$Driver!="RIC",]
qualytype$Driver<-factor(qualytype$Driver,levels=c("BOT","LAW","ZHO","COL","RIC","ALB",
                                                 "MAG","OCO","STR","TSU",
                                                 "HUL","GAS","ALO","PER","HAM",
                                                 "RUS","SAI","PIA","LEC","NOR","VER"))
qualytype$Q<-factor(qualytype$Q,levels=c("Q3","Q2","Q1"))
qualytypeplot <- ggplot(qualytype, aes(x = Qualy, y = Driver, fill = Q)) +
  geom_bar(stat="identity",color="black") + 
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, by = 2)) +
  scale_y_discrete(expand = c(0, 0))+
  scale_fill_manual(values = c( "Q1"="#EE232C","Q2"="gold", "Q3"="#7ED957")) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Qualifying exits by driver") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 14, hjust = 0.5),
        axis.ticks=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(margin = margin(r = -25),size=13),
        axis.title = element_text(size = 14),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15)) +
  labs(caption = " ", subtitle = "2024 Season")

print(qualytypeplot)


podiumsdrivers<-read.csv("PodiumsFS.csv")
podiumsdrivers$Driver<-factor(podiumsdrivers$Driver,levels=c("OCO","GAS","PER","RUS","HAM","PIA","SAI","LEC","NOR","VER"))
podiumsdriversplot <- ggplot(podiumsdrivers, aes(x = Podiums, y = reorder(Driver, Podiums), fill = Position)) +
  geom_vline(xintercept = seq(0, 14, by = 2), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity",color="black") + 
  scale_x_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 2)) +
  scale_y_discrete(expand = c(0, 0))+
  scale_fill_manual(values = c( "gold","#C0C0C0", "#CD7F32")) +
  xlab("Podiums") +
  ylab("Driver") +
  ggtitle("Podiums by driver") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 14, hjust = 0.5),
        axis.ticks.y=element_blank(),
        axis.text.y = element_text(margin = margin(r = -25),size=13),
        axis.title = element_text(size = 14),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15)) +
  labs(caption = " ", subtitle = "2024 Season")


print(podiumsdriversplot)


podiumsdrivers<-read.csv("PODIUMSFULL.csv")
podium_long <- podiumsdrivers %>%
  pivot_longer(cols = c("First", "Second", "Third"),
               names_to = "Position",
               values_to = "Count")

# Plot
ggplot(podium_long, aes(x = Count, y = reorder(Driver, Count), fill = Position)) + 
  geom_bar(stat = "identity", color = "black") +
  
  # Agregar etiquetas con los conteos
  geom_text(aes(label = Count),  
            position = position_stack(vjust = 0.5),  # Centrar el texto en cada sección
            size = 4, 
            color = "black") +
  
  labs(title = "Podiums by Driver",
       x = "Podiums",
       y = "Driver") +
  
  # Eliminar espacio entre barras y nombres de pilotos
  scale_y_discrete(expand = c(0, 0)) +   # Ajuste del eje Y (pilotos)
  
  # Ajuste del eje X (conteo) y rango hasta 225
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 210, by = 10), limits = c(0, 210)) + 
  
  scale_fill_manual(values = c("First" = "#FFD700",   # Oro
                               "Second" = "#C0C0C0", # Plata
                               "Third" = "#CD7F32")) + # Bronce
  
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 15)
  )+
  labs(caption = " ", subtitle = "2025 Grid")


   percentage<-read.csv("pointspercentage.csv")
percentage$Driver<-factor(percentage$Driver,levels=c("ZHO","BOT","SAR","ALB","OCO","GAS",
                                                     "MAG","HUL","RIC","TSU","STR","ALO","RUS","HAM","BEA","SAI","LEC","PIA","NOR","PER","VER"))
plotpercentage <- ggplot(percentage, aes(x = Driver, y = Percentage, fill = Driver)) +
  geom_hline(yintercept = seq(0, 100, by = 10), linetype = "dashed", color = "grey")+
  geom_bar(stat = "identity") +
  scale_fill_manual(values=drivercolors)+
  scale_y_continuous(limits=c(0,100),breaks = seq(0, 100, by = 10))+
  geom_text(aes(label = paste0(round(Percentage,1), "%")), vjust = 0.5, hjust = -0.2) +
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
  labs(caption = "@f1.datascience",subtitle = "Summer Break (14 Races)")+theme(legend.position = "none")

plotpercentage



gapSingaporeQ<-read.csv("MSAvgGaptopole.csv")
gapSingaporeQ<-gapSingaporeQ[gapSingaporeQ$Driver!="BEA",]
pQualyS<-ggplot(gapSingaporeQ, aes(x=Gap,y=reorder(Driver,-Gap),fill=Driver))+ 
  geom_vline(xintercept = seq(0, 2.4, by = 0.2), linetype = "dashed", color = "grey")+
  geom_bar(stat="identity")+
  scale_fill_manual(values=drivercolors)+
  scale_x_continuous(breaks = seq(0, 2.4, by = .2))+
  scale_y_discrete(expand = c(0,0))+
  geom_text(aes(label = round(Gap,3)), hjust = -0.1, size = 4) +
  xlab("Gap (seconds)") +
  ylab("Driver") +
  ggtitle("Average gap to pole")+
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
  labs(caption = "@f1.datascience",subtitle = "Summer Break (14 races)")+theme(legend.position = "none")
pQualyS 

sumposgained <- read.csv("DIF.csv") 
names(sumposgained) <- c("Piloto", "Dif") 

# Filtering out 'BEA' from the dataframe using the correct column name
sumposgained <- sumposgained[sumposgained$Piloto != "BEA",] 

sumposplot <- ggplot(sumposgained, aes(Dif, reorder(Piloto, Dif), fill = Dif >= 0)) +
  geom_vline(xintercept = seq(-3, 3, by = 1), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = Piloto), vjust = 0.5, 
            hjust = ifelse(sumposgained$Dif >= 0, 1.2, -0.2), 
            color = "white", size = 4) +
  scale_fill_manual(values = c("#63B234", "#CF0012")) +
  geom_vline(xintercept = 0, color = "black", size = 1, linetype = "solid") +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, by = 1)) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(title = "Difference between qualifying position and race position",
       caption = " ",
       subtitle = "2024 Season",
       x = " ",
       y = " ") +
  theme(legend.position = "none")

print(sumposplot)


# Crear el dataframe
# Crear el dataframe
# Load necessary libraries
library(ggplot2)

# Create the dataframe
df2 <- data.frame(
  Piloto = c("DOO", "BEA", "ZHO", "COL", "HAM", "MAG", "LEC", "OCO", "GAS", "SAR", 
             "PIA", "HUL", "RIC", "SAI", "PER", "ALO", "STR", "BOT", "VER", "NOR", 
             "LAW", "RUS", "ALB", "TSU"),
  Promedio_Qualy = c(20.00, 13.00, 18.08, 15.78, 8.50, 14.27, 5.33, 14.39, 13.42, 17.64, 
                     5.42, 11.71, 13.22, 5.52, 9.33, 9.67, 12.88, 15.21, 2.92, 3.54, 
                     12.67, 5.17, 12.96, 11.13),
  Promedio_Carrera = c(15.00, 9.67, 15.50, 13.56, 6.96, 13.36, 4.54, 13.70, 12.88, 17.14, 
                       5.13, 11.63, 13.39, 5.70, 9.63, 10.17, 13.38, 15.83, 3.62, 4.29, 
                       13.50, 6.75, 14.54, 13.17),
  Dif = c(5.00, 3.33, 2.58, 2.22, 1.54, 0.91, 0.79, 0.70, 0.54, 0.50, 
          0.29, 0.08, -0.17, -0.17, -0.29, -0.50, -0.50, -0.63, -0.71, -0.75, 
          -0.83, -1.58, -1.58, -2.04)
)
df2<-df2[df2$Piloto!="BEA",]
df2<-df2[df2$Piloto!="SAR",]
df2<-df2[df2$Piloto!="DOO",]
df2<-df2[df2$Piloto!="RIC",]
# Create the horizontal bar plot
ggplot(df2, aes(x = Dif, y = reorder(Piloto, Dif), fill = Dif >= 0)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  geom_text(aes(label = round(Dif, 2), 
                color = ifelse(Dif >= 0, "black", "black")), 
            hjust = ifelse(df2$Dif >= 0, 1.2, -0.2), 
            size = 5) +
  scale_fill_manual(values = c("#CF0012", "#63B234")) + # Red for negative, green for positive
  labs(
    title = "Net Average Position Change",
    subtitle = "2024 Season",
    x = "Difference (AvgQualy - AvgRace)", 
    y = "Driver"
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text.y = element_text(size = 12),
    axis.ticks.y = element_blank() # Optionally hide y-axis ticks
  ) +
  scale_color_identity()





fangio2<- data.frame(
  Stat = c("En Puntos/In the Points", "Podios/Podiums", "Sin Puntos/No points", "Victorias/Wins"),
  Value = c(43, 25.2, 20.42, 11.38))
fangio2p<-ggplot(fangio2, aes(x = "", y = Value, fill = Stat)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(Value, "%")), position = position_stack(vjust = 0.5), size = 5)+
  coord_polar(theta = "y") +
  labs(title = "Vistazo a la carrera de Brabham/Brabham's career at a glance",
       fill = " ", subtitle = "1955-1970",
       caption = "@racingwithdata") +
  theme_void() +theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    legend.title = element_text(size = 13),
    legend.text=element_text(size = 13)
  )+
  scale_fill_manual(values = c("#2ECC71","#3498DB", "#E74C3C", "#F1C40F"))
fangio2p

library(reshape2)
res2024 <- read.csv("Results2024.csv")
res2024<-res2024[res2024$driverCode!="BEA",]
res2024<-res2024[res2024$driverCode!="SAR",]
res2024<-res2024[res2024$driverCode!="DOO",]
res2024<-res2024[res2024$driverCode!="RIC",]
res2024$driverCode <- factor(res2024$driverCode, levels = rev(res2024$driverCode))  # Invertir el orden

# Convertir los datos a formato largo
res2024_melted <- melt(res2024, id.vars = "driverCode")

# Crear el heatmap
# Crear el heatmap con los puntos en cada cuadro
library(viridis)

# Crear el heatmap con los puntos y sin los nombres de los GP
ggplot(res2024_melted, aes(x = variable, y = driverCode, fill = value)) +
  geom_tile() +
  scale_fill_viridis(option = "plasma", direction = 1) +  # Usar la paleta plasma
  geom_text(aes(label = value), color = "black", size = 3) +  # Añadir los puntos en cada cuadro
  theme_bw() +
  labs(title = " ",
       x = " ", y = "Driver",
       fill = "Points") +  # Cambiar el título de la leyenda a "Points"
  theme(axis.text.x = element_blank(),  # Eliminar los nombres de los GP
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        legend.title = element_text(size = 13),  # Tamaño del título de la leyenda
        legend.text = element_text(size = 13))

res2024 <- read.csv("Results2024Teams.csv")
res2024$Team <- factor(res2024$Team, levels = rev(res2024$Team))  # Invertir el orden

# Convertir los datos a formato largo
res2024_melted <- melt(res2024, id.vars = "Team")

# Crear el heatmap
# Crear el heatmap con los puntos en cada cuadro
library(viridis)

# Crear el heatmap con los puntos y sin los nombres de los GP
ggplot(res2024_melted, aes(x = variable, y = Team, fill = value)) +
  geom_tile() +
  scale_fill_viridis(option = "plasma", direction = 1) +  # Usar la paleta plasma
  geom_text(aes(label = value), color = "black", size = 3) +  # Añadir los puntos en cada cuadro
  theme_bw() +
  labs(title = " ",
       x = " ", y = " ",
       fill = "Points") +  # Cambiar el título de la leyenda a "Points"
  theme(axis.text.x = element_blank(),  # Eliminar los nombres de los GP
        axis.text.y = element_text(size = 11, angle = 0, hjust = 1),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5),
        legend.title = element_text(size = 13),  # Tamaño del título de la leyenda
        legend.text = element_text(size = 13))

pointsdf <- read.csv("2023vs2024Drivers.csv")
ggplot(pointsdf, aes(x = DifPos, y = reorder(Driver, DifPos), fill = DifPos >= 0)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  geom_text(aes(label = round(DifPos, 2), 
                color = ifelse(DifPos >= 0, "black", "black")), 
            hjust = ifelse(pointsdf$DifPos >= 0, 1.2, -0.2), 
            size = 5) +
  scale_fill_manual(values = c("#CF0012", "#63B234")) +
  scale_x_continuous(limits = c(-8,6), breaks = seq(-8,6,by=2))+# Red for negative, green for positive
  labs(
    title = "Standings Position Changes",
    subtitle = "2023 vs 2024 Season",
    x = "Difference in Positions", 
    y = "Driver"
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text.y = element_text(size = 12),
    axis.ticks.y = element_blank() # Optionally hide y-axis ticks
  ) +
  scale_color_identity()

ggplot(pointsdf, aes(x = DifPoints, y = reorder(Driver, DifPoints), fill = DifPoints >= 0)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  geom_text(aes(label = round(DifPoints, 2), 
                color = ifelse(DifPoints >= 0, "black", "black")), 
            hjust = ifelse(pointsdf$DifPoints >= 0, 1.2, -0.2), 
            size = 5) +
  scale_fill_manual(values = c("#CF0012", "#63B234")) +
  scale_x_continuous(limits=c(-150,200),breaks = seq(-150,200,by=50))+# Red for negative, green for positive
  labs(
    title = "Points Changes",
    subtitle = "2023 vs 2024 Season",
    x = "Points Difference", 
    y = "Driver"
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text.y = element_text(size = 12),
    axis.ticks.y = element_blank() # Optionally hide y-axis ticks
  ) +
  scale_color_identity()


