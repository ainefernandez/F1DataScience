library(ggplot2)
setwd("/Users/ainefernandez/documents/F1DataScience")
fangio<-read.csv("fangio.csv")
fangio$Equipo.Team<-factor(fangio$Equipo.Team, levels = c("Maserati","Ferrari","Mercedes","Alfa Romeo")) 
streakplot <- ggplot(fangio, aes(x = Cantidad, y = reorder(Logros, -Cantidad), fill = Equipo.Team)) +
  geom_vline(xintercept = c(4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Maserati" = "steelblue", "Ferrari" = "#F91536", "Mercedes" = "#6CD3BF", "Alfa Romeo" = "#C92D4B")) +
  scale_x_continuous(limits = c(0, 52), breaks = seq(0, 52, by = 4)) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Carrera de Fangio/Fangio's career") +
  labs(
    subtitle = "1950-1958",
    caption = "@f1.datascience",
    fill = "Equipo/Team"  
  ) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    axis.text.y = element_text(margin = margin(r = -30)),
    axis.ticks.y = element_blank(),
    legend.title = element_text(size = 13),
    legend.text=element_text(size = 13)
  )

streakplot

fangio2<-read.csv("JimClark.csv")
fangio2p<-ggplot(fangio2, aes(x = "", y = Value, fill = Stat)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(Value, "%")), position = position_stack(vjust = 0.5), size = 5)+
  coord_polar(theta = "y") +
  labs(title = "Vistazo a la carrera de Fangio/Fangio's career at a glance",
       fill = " ", subtitle = "1950-1958",
       caption = "@f1.datascience") +
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


# Create the DataFrame
fangio2 <- data.frame(
  Stat = c("En Puntos/In the Points", "Podios/Podiums", "Sin Puntos/No points", "Victorias/Wins"),
  Value = c(11.11, 9.72, 44.45, 34.72)
)

# Create the DataFrame
fangio2 <- data.frame(
  Stat = c("En Puntos/In the Points", "Podios/Podiums", "Sin Puntos/No points", "Victorias/Wins"),
  Value = c(11.11, 9.72, 44.45, 34.72)
)

# Plot the pie chart
fangio2p <- ggplot(fangio2, aes(x = "", y = Value, fill = Stat)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(Value, "%")), position = position_stack(vjust = 0.5), size = 5) +
  coord_polar(theta = "y") +
  labs(title = "Vistazo a la carrera de Jim Clark/Clark's career at a glance",
       fill = " ", subtitle = "1960-1968",
       caption = "@racingwithdata") +
  theme_void() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13)
  ) +
  scale_fill_manual(values = c("#2ECC71", "#3498DB", "#E74C3C", "#F1C40F"))

fangio2p




piquet <- read.csv("piquet.csv")
piquet$Equipo.Team <- factor(piquet$Equipo.Team, levels = c("Benetton", "Lotus", "Williams", "Brabham", "McLaren", "Ensign"))
streakplot <- ggplot(piquet, aes(x = Cantidad, y = reorder(Logros, -Cantidad), fill = Equipo.Team)) +
  geom_vline(xintercept = seq(12, 204, by = 12), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Benetton" = "#008860", "Lotus" = "gold", "Williams" = "#37BEDD", "McLaren" = "#F58020", "Ensign" = "steelblue","Brabham"="#C92D4B")) +
  scale_x_continuous(limits = c(0, 204), breaks = seq(0, 204, by = 12)) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Carrera de Piquer/Piquet's career") +
  labs(
    subtitle = "1978-1991",
    caption = "@f1.datascience",
    fill = "Equipo/Team"
  ) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    axis.text.y = element_text(margin = margin(r = -20)),
    axis.ticks.y = element_blank(),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13)
  )

streakplot


piquet2<-read.csv("piquet2.csv")
piquet2p<-ggplot(piquet2, aes(x = "", y = Value, fill = Stat)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(Value, "%")), position = position_stack(vjust = 0.5), size = 5)+
  coord_polar(theta = "y") +
  labs(title = "Vistazo a la carrera de Piquet/Piquet's career at a glance",
       fill = " ", subtitle = "1978-1991",
       caption = "@f1.datascience") +
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
piquet2p



montoya <- read.csv("montoya.csv")
montoya$Equipo.Team <- factor(montoya$Equipo.Team, levels = c("McLaren", "Williams"))
streakplot <- ggplot(montoya, aes(x = Cantidad, y = reorder(Logros, -Cantidad), fill = Equipo.Team)) +
  geom_vline(xintercept = seq(10, 100, by = 10), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c( "Williams" = "#37BEDD", "McLaren" = "#F58020")) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Carrera de Montoya/Montoya's career") +
  labs(
    subtitle = "2001-2006",
    caption = "@f1.datascience",
    fill = "Equipo/Team"
  ) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    axis.text.y = element_text(margin = margin(r = -20)),
    axis.ticks.y = element_blank(),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13)
  )

streakplot


montoya2<-read.csv("montoya2.csv")
montoya2p<-ggplot(montoya2, aes(x = "", y = Value, fill = Stat)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(Value, "%")), position = position_stack(vjust = 0.5), size = 5)+
  coord_polar(theta = "y") +
  labs(title = "Vistazo a la carrera de Montoya/Montoya's career at a glance",
       fill = " ", subtitle = "2001-2006",
       caption = "@f1.datascience") +
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
montoya2p

checo<-read.csv("checo.csv")
checo$Equipo.Team<-factor(checo$Equipo.Team, levels = c("Red Bull","Racing Point","Force India","McLaren","Sauber"))
streakplot <- ggplot(checo, aes(x = Cantidad, y = reorder(Logros, -Cantidad), fill = Equipo.Team)) +
  geom_vline(xintercept = seq(15, 255, by = 15), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("McLaren" = "#F58020", "Sauber" = "#C92D4B", "Racing Point" = "hotpink", "Force India" = "darkgreen", "Red Bull" = "#3671C6")) +
  scale_x_continuous(limits = c(0, 255), breaks = seq(0, 255, by = 15)) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Carrera de Pérez/Perez's career") +
  labs(
    subtitle = "2011-2023*",
    caption = "@f1.datascience",
    fill = "Equipo/Team"
  ) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    axis.text.y = element_text(margin = margin(r = -15)),
    axis.ticks.y = element_blank(),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13)
  )

streakplot

checo2<-read.csv("checo2.csv")
checo2p<-ggplot(checo2, aes(x = "", y = Value, fill = Stat)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(Value, "%")), position = position_stack(vjust = 0.5), size = 5)+
  coord_polar(theta = "y") +
  labs(title = "Vistazo a la carrera de Pérez/Pérez's career at a glance",
       fill = " ", subtitle = "2011-2023*",
       caption = "@f1.datascience") +
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
checo2p

senna<-read.csv("senna.csv")
senna$Equipo.Team<-factor(senna$Equipo.Team, levels = c("Williams","McLaren","Lotus","Toleman"))
streakplot <- ggplot(senna, aes(x = Cantidad, y = reorder(Logros, -Cantidad), fill = Equipo.Team)) +
  geom_vline(xintercept = seq(15, 165, by = 15), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("McLaren" = "#F58020", "Lotus" = "#F1C40F", "Toleman" = "#0402C4", "Williams" = "#37BEDD")) +
  scale_x_continuous(limits = c(0, 165), breaks = seq(0, 165, by = 15)) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Carrera de Senna/Senna's career") +
  labs(
    subtitle = "1984-1994",
    caption = "@f1.datascience",
    fill = "Equipo/Team"
  ) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    axis.text.y = element_text(margin = margin(r = -15)),
    axis.ticks.y = element_blank(),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13)
  )

streakplot

senna2<-read.csv("senna2.csv")
senna2p<-ggplot(senna2, aes(x = "", y = Value, fill = Stat)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(Value, "%")), position = position_stack(vjust = 0.5), size = 5)+
  coord_polar(theta = "y") +
  labs(title = "Vistazo a la carrera de Senna/Senna's career at a glance",
       fill = " ", subtitle = "1984-1994",
       caption = "@f1.datascience") +
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
  scale_fill_manual(values = c("#F1C40F","#3498DB","#2ECC71","#E74C3C"),
                    breaks = c("Victorias/Wins", "Podios/Podiums", "En puntos/In the points","Sin puntos/No points"))
senna2p



library(ggplot2)

library(ggplot2)

# Create the data frame for Jackie Stewart's performance
jackie_data <- data.frame(
  Stat = c("Victorias/Wins", "Podios/Podiums", "En Puntos/Points", "Sin Puntos/No points"),
  Value = c(27.27, 16.16, 14.14, 42.43)
)

# Create the pie chart with correct colors
jackie_pie <- ggplot(jackie_data, aes(x = "", y = Value, fill = Stat)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(Value, "%")), position = position_stack(vjust = 0.5), size = 5) +
  coord_polar(theta = "y") +
  labs(
    title = "Vistazo a la carrera de Jackie Stewart/Jackie Stewart's career at a glance",
    fill = " ", 
    subtitle = "1965-1973",
    caption = "@racingwithdata"
  ) +
  theme_void() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13)
  ) +
  scale_fill_manual(values = c("Victorias/Wins" = "#F1C40F",   # Yellow
                               "Podios/Podiums" = "#3498DB",  # Blue
                               "En Puntos/Points" = "#2ECC71", # Green
                               "Sin Puntos/No points" = "#E74C3C")) # Red

# Print the chart
jackie_pie

library(ggplot2)

# Create the data frame for Niki Lauda's performance
lauda_data <- data.frame(
  Stat = c("Victorias/Wins", "Podios/Podiums", "En Puntos/Points", "Sin Puntos/No points"),
  Value = c(14.62, 16.96, 3.51, 64.91)
)

# Create the pie chart for Niki Lauda's career
lauda_pie <- ggplot(lauda_data, aes(x = "", y = Value, fill = Stat)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(Value, "%")), position = position_stack(vjust = 0.5), size = 5) +
  coord_polar(theta = "y") +
  labs(
    title = "Vistazo a la carrera de Niki Lauda/Niki Lauda's career at a glance",
    fill = " ", 
    subtitle = "1971-1985",
    caption = "@racingwithdata"
  ) +
  theme_void() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13)
  ) +
  scale_fill_manual(values = c("Victorias/Wins" = "#F1C40F",   # Yellow
                               "Podios/Podiums" = "#3498DB",   # Blue
                               "En Puntos/Points" = "#2ECC71", # Green
                               "Sin Puntos/No points" = "#E74C3C")) # Red

# Print the chart
lauda_pie
