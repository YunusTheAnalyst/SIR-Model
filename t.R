library(gganimate)
library(ggplot2)
library(dplyr)
library(gapminder)
library(ggthemes)
library(gifski)
library(ggpubr)


sirmodel1plot<-ggplot(data = out_long1,          
                      aes(x = time, y = value, colour = variable)) +  
  geom_line() +                                                          
  scale_x_continuous(n.breaks = 16)+
  scale_y_continuous(n.breaks = 13)+
  labs(title = "Average Infected and Recovered for beta = 1/14",
       subtitle = "Monthly values",
       x = "Time (days)",
       y = "No: of people")+
  scale_color_discrete(name="State")+
  geom_point()

sirmodel1plot.animation=sirmodel1plot+
  transition_reveal(time)

animate(sirmodel1plot.animation, height=500,width=800, fps=30, duration = 10,
        end_pause = 60, res=100)
anim_save("SIRmodel1plot1.gif")

sirmodel1plot.animation

Ro_boxplot<-ggplot(Rdata) +
  aes(x = Status, y = Ro, fill = Status) +
  geom_boxplot(shape = "circle") +
  scale_fill_hue(direction = 1) +
  labs(
    subtitle = "Status: {closest_state}",
    x = "Status",
    y = "R0",
    title = "Box Plot for Reproduction Number of Covid-19") +
  ggthemes::theme_wsj() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  scale_fill_tableau()

animm<-Ro_boxplot+ 
  transition_states(Status, wrap=FALSE)+
  shadow_mark(alpha = .5)+
  enter_grow()+
  exit_fade()+
  ease_aes()
animate(animm, height=500,width=800, fps=30, duration = 10,
        end_pause = 60, res=100)

animate(animm,duration = 15)
 
anim_save("R0_Boxplot1.gif")

ggplot(Rdata) +
  aes(x = Status, y = Ro, fill = Status) +
  geom_boxplot(shape = "circle") +
  stat_compare_means()+
  scale_fill_hue(direction = 1) +
  labs(
    subtitle = ,
    x = "Status",
    y = "R0",
    title = "Box Plot for R0 Values",
    fill = "Status"
  ) +
  ggthemes::theme_wsj() +
  compare_means()+
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

out_long2<-output2 %>% 
  mutate(Time=seq(as.date((2020/03/13:2021/08/31))))


sirmodel2plot<-ggplot(data = out_long2,          
       aes(x = time, y = value/S_I_R_V2$population[S_I_R_V2$x2020 == "Total"], colour = variable)) +  
  geom_line() +
  #scale_x_continuous(n.breaks = 20)+
  scale_x_continuous(breaks = seq(0, length(seq(as.Date("2020/03/13"), as.Date("2021/08/31"), by=1)), by = 30))+
  scale_y_continuous(n.breaks = 10)+
  labs(title = "SIR Model for Covid-19 in Kenya",
       subtitle = "For the Period March 2020 to August 2021")+
  xlab("Time (days)")+
  ylab("Proportion of the population")+
  scale_color_discrete(name="State")+
  geom_point()

sirmodel2plot.animation<-sirmodel2plot+
  transition_reveal(time)+
  exit_fade()

animate(sirmodel2plot.animation, height=500,width=800, fps=30, duration = 10,
        end_pause = 60, res=100)

anim_save("sirmodel2anim.gif")
