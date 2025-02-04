pQualy2022<-ggplot(gaps, aes(x=AvgGap2022,y=reorder(Team,-AvgGap2022),fill=Team))+ 
  geom_vline(xintercept = c(0.5,1, 1.5,2.0,2.5), linetype = "dashed", color = "grey")+
  geom_vline(xintercept = c(0.5,1, 1.5,2.0,2.5), linetype = "dashed", color = "grey")+
  
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("Red Bull "="#3671C6","Mercedes "="#6CD3BF","Aston Martin "="#358C75","Ferrari "="#F91536","Williams"="#37BEDD",
                               "Alpine "="#2293D1","Haas "="#B6BABD","AlphaTauri"="#5E8FAA","McLaren  "="#F58020","Alfa Romeo "="#C92D4B"))+
  scale_x_continuous(breaks = seq(0, 2.5, by = 0.1))+
  scale_y_discrete(expand = c(0,0))+
  xlab("Average gap to pole (seconds)") +
  ggtitle("Gap to pole in Qualy after 3 races")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title=element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust=0.5),
        plot.subtitle =element_text(size = 18, hjust = 0.5),
        axis.title = element_text(size = 13),
        axis.title.y=element_blank(),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -30)),
        axis.ticks.y =element_blank())+
  labs(caption = "@f1.datascience",subtitle = "2022")+theme(legend.position = "none")
pQualy2022

pQualy2023<-ggplot(gaps, aes(x=AvgGap2023,y=reorder(Team,-AvgGap2023),fill=Team))+ 
  geom_vline(xintercept = c(0.5,1, 1.5,2.0), linetype = "dashed", color = "grey")+
  geom_vline(xintercept = c(0.5,1, 1.5,2.0), linetype = "dashed", color = "grey")+
  
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("Red Bull "="#3671C6","Mercedes "="#6CD3BF","Aston Martin "="#358C75","Ferrari "="#F91536","Williams"="#37BEDD",
                               "Alpine "="#2293D1","Haas "="#B6BABD","AlphaTauri"="#5E8FAA","McLaren  "="#F58020","Alfa Romeo "="#C92D4B"))+
  scale_x_continuous(breaks = seq(0, 2.0, by = 0.1))+
  scale_y_discrete(expand = c(0,0))+
  xlab("Average gap to pole (seconds)") +
  ggtitle("Gap to pole in Qualy after 3 races")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title=element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust=0.5),
        plot.subtitle =element_text(size = 18, hjust = 0.5) ,
        axis.title = element_text(size = 13),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -25)),
        axis.ticks.y =element_blank())+
  labs(caption = "@f1.datascience",subtitle = "2023")+theme(legend.position = "none")
pQualy2023
