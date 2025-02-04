
brazil<-read.csv("circuitsbrazil.csv")
interlagos <- brazil[brazil$Circuit == "Interlagos", ]
jaca<-brazil[brazil$Circuit == "Jacarepagua", ]
# Calculate the percentages


jacap <- ggplot(jaca, aes(x = "", y = Wins, fill = Team)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Victorias por equipo en Jacarepagua/Wins by team in Jacarepagua",
    caption = "@f1.datascience"
  ) +
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
  scale_fill_manual(values = c("McLaren" = "#F58020", "Ferrari" = "#F91536", "Renault" = "#2293D1", "Brabham" = "#F1C40F", "Williams" = "#37BEDD"))

jacap

    


streakplot <- ggplot(interlagos, aes(x = Wins, y = reorder(Team, Wins))) +
  geom_vline(xintercept = seq(1, 10, by = 1), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity", fill = "#009B3B") +
  geom_text(aes(label = Wins), hjust = -1) +  # Corrected the missing closing parenthesis
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Victorias por equipo en Interlagos/Wins by team in Interlagos") +
  labs(
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
    legend.text = element_text(size = 13))

streakplot


arg<-read.csv("buenosaires.csv")


streakplot <- ggplot(arg, aes(x = Wins, y = reorder(Team, Wins))) +
  geom_vline(xintercept = seq(1, 4, by = 1), linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity", fill = "#8FC9FF") +
  geom_text(aes(label = Wins), hjust = -1) +  # Corrected the missing closing parenthesis
  scale_x_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 1)) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Victorias por equipo en Buenos Aires/Wins by team in Buenos Aires") +
  labs(
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
    legend.text = element_text(size = 13))

streakplot
