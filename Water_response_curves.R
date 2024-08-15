#### load packages

library(plotly)
library(readxl)
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages('tidyverse')
install.packages("ggplot2")
library(ggplot2)
library(mgcv)
library(mgcViz)
install.packages("readxl")
library(forcats)
library(tidypaleo)
install.packages("tidypaleo")
library(ggpubr)
install.packages("ggpubr")
install.packages("cowplot")
library(cowplot)
library(gridExtra)
library(grid) 
install.packages("devtools")
library(devtools)
library(tidyverse)



  
  ## plot all species individually 

Lp <- 
  ggplot(subset(Water_response_github, Species %in% c("Lobaria pulmonaria")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
  geom_point(aes(shape= Replicate),size =1.8)+
  geom_line(aes(color = Replicate))  +
  geom_line(linewidth = 0.1) +
  annotate("rect", xmin = 0.07, xmax = 0.14, ymin = -10, ymax = 30, alpha = .1,fill = "grey1") +
  labs(y= "CO[2]~assimilation ((nmol g-1 s-1))", x = "") +
  ylab(expression(CO[2]~assimilation ~ (nmol ~ g^{-1}~s^{-1}))) +
  theme(axis.title.x = element_blank()) +
  coord_cartesian(xlim =c(0,0.6 ), ylim = c(-10, 30))+
  geom_hline(yintercept=0, linetype = 2, colour = "black", size = 0.1) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(vjust = +0.1)) +
  theme(axis.title.x = element_text(vjust = 0.5)) +
  theme(axis.text.y = element_text(size=20)) +
  theme(axis.text.x = element_text(size=20)) +
  theme(axis.text.y = element_text(vjust = +0.1)) +
  ggtitle("A") +
  theme(plot.title = element_text(size = 20)) +
  theme(plot.title=element_text(vjust = -63.5)) + 
  theme(plot.title=element_text(hjust = 0.01)) +
  scale_y_continuous(breaks=seq(-10,30, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) +
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+ 
  scale_colour_manual(values = c("gray22", "slategrey", "slategray3", "gray22", "slategrey", "slategray3")) +
scale_shape_manual(values = c(1, 0, 2, 19, 15, 17)) 


######### Sticta sylvatica  ##############

Ss <- ggplot(subset(Water_response_github, Species %in% c("Sticta sylvatica")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
  geom_point(aes(shape= Replicate),size =1.8)+
  geom_line(aes(color = Replicate)) +
  geom_line(linewidth = 0.1) +
  labs(y= "", x = "")  +
  annotate("rect", xmin = 0.20, xmax = 0.37, ymin = -10, ymax = 30, alpha = .1,fill = "grey1") +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  coord_cartesian(xlim =c(0,0.6 ), ylim = c(-10, 30))+
  geom_hline(yintercept=0, linetype = 2, colour = "black", size = 0.1) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(vjust = +1)) +
  theme(axis.title.x = element_text(vjust = 0.5)) +
  theme(axis.text.y = element_text(vjust = +0.1)) +
  ggtitle("D") +
  theme(plot.title = element_text(size = 20)) +
  theme(plot.title=element_text(vjust = -63.5)) + 
  theme(plot.title=element_text(hjust = 0.01)) +
  theme(legend.title = element_text( size =14)) +
  theme(axis.text.y = element_text(size=20)) +
  theme(axis.text.x = element_text(size=20)) +
  scale_y_continuous(breaks=seq(-10,50, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) +
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+ 
  scale_colour_manual(values = c("goldenrod4","goldenrod3","gold2","goldenrod4","goldenrod3","gold2")) +
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17))




##### Hypotrachyna laevigata ###### 

Hl <- ggplot(subset(Water_response_github, Species %in% c("Hypotrachyna laevigata")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
  geom_point(aes(shape= Replicate),size =1.8)+
  geom_line(aes(color = Replicate)) +
  geom_line(linewidth = 0.1) +
  labs(y= "", x = "") +
  coord_cartesian(xlim =c(0,0.6 ), ylim = c(-10, 30))+
  annotate("rect", xmin = 0.21, xmax = 0.45, ymin = -10, ymax = 30, alpha = .1,fill = "grey1") +
  geom_hline(yintercept=0, linetype = 2, colour = "black", size = 0.1) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(vjust = +1)) +
  theme(axis.title.x = element_text(vjust = 0.5)) +
  theme(axis.text.y = element_text(vjust = +0.1)) +
   ggtitle("F") +
  theme(plot.title = element_text(size = 20)) +
  theme(plot.title=element_text(vjust = -63.5)) + 
  theme(plot.title=element_text(hjust = 0.01)) +
  theme(legend.title = element_text( size =14)) +
  theme(axis.text.y = element_text(size=20)) +
  theme(axis.text.x = element_text(size=20)) +
  scale_y_continuous(breaks=seq(-10,30, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) +
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+ 
  scale_colour_manual(values = c("deeppink2", "lightpink2", "deeppink4", "deeppink2", "lightpink2", "deeppink4"))+
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17))







##### Sticta limbata ######

Sl <-  ggplot(subset(Water_response_github, Species %in% c("Sticta limbata")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
  geom_point(aes(shape= Replicate),size =1.8)+
  geom_line(aes(color = Replicate)) +
  geom_line(linewidth = 0.1) +
  labs(y= "", x = "") +
  ylab(expression(CO[2]~assimilation ~ (nmol ~ g^{-1}~s^{-1})))+
  theme(axis.title.x = element_blank()) +
  coord_cartesian(xlim =c(0,0.6), ylim = c(-20, 50)) +
  annotate("rect", xmin = 0.13, xmax = 0.18, ymin = -20, ymax = 50, alpha = .1,fill = "grey1") +
  geom_hline(yintercept=0, linetype = 2, colour = "black", size = 0.1) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(vjust = +0.1)) +
  theme(axis.title.x = element_text(vjust = 0.5)) +
  theme(legend.key.size = unit(0.5, "cm")) +
  theme(axis.text.y = element_text(vjust = +0.1)) +
  ggtitle("C") +
  theme(plot.title = element_text(size = 20)) +
  theme(plot.title=element_text(vjust = -63.5)) + 
  theme(plot.title=element_text(hjust = 0.01)) +
  theme(legend.position = "none") +
  theme(axis.text.y = element_text(size=20)) +
  theme(axis.text.x = element_text(size=20)) +
  scale_y_continuous(breaks=seq(-20,50, by=10), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) +
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+ 
  scale_colour_manual(values = c("purple4","purple2", "orchid3", "purple4","purple2", "orchid3")) +
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17))


##### Ramalina calicaris  ######

Rc <-  ggplot(subset(Water_response_github, Species %in% c("Ramalina calicaris")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
  geom_point(aes(shape= Replicate),size =1.8)+
  geom_line(aes(color = Replicate)) +
  geom_line(linewidth = 0.1) +
  labs(y= "", x = "")  +
  theme(axis.title.x = element_blank()) +
  coord_cartesian(xlim =c(0,0.6 ), ylim = c(-10, 30))+
  annotate("rect", xmin = 0.11, xmax = 0.24, ymin = -10, ymax = 30, alpha = .1,fill = "grey1") +
  geom_hline(yintercept=0, linetype = 2, colour = "black", size = 0.1) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(legend.text = element_text (face = "italic")) +
  theme(axis.title.y = element_text(vjust = +1)) +
  theme(axis.title.x = element_text(vjust = 0.5)) +
  theme(legend.position = "none") +
  theme(axis.text.y = element_text(vjust = +0.1)) +
   ggtitle("B") +
  theme(plot.title = element_text(size = 20)) +
  theme(plot.title=element_text(vjust = -63.5)) + 
  theme(plot.title=element_text(hjust = 0.01)) +
  theme(legend.title = element_text( size =14)) +
  theme(axis.text.y = element_text(size=20)) +
  theme(axis.text.x = element_text(size=20)) +
  scale_y_continuous(breaks=seq(-10,30, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) + 
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  scale_colour_manual(values = c("darkorange4","darkorange2", "orange1", "darkorange4","darkorange2", "orange1")) +
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17)) 





##### Ricasolia virens ######


Rv <- ggplot(subset(Water_response_github, Species %in% c("Lobaria virens")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
  geom_point(aes(shape= Replicate),size =1.8)+
  geom_line(aes(color = Replicate)) +
  geom_line(linewidth = 0.1) +
  labs(y= "", x = "") +
  ylab(expression(CO[2]~assimilation ~ (nmol ~ g^{-1}~s^{-1}))) +
  theme(axis.title.x = element_blank()) +
  coord_cartesian(xlim =c(0,0.6 ), ylim = c(-10, 30))+
  annotate("rect", xmin = 0.11, xmax = 0.20, ymin = -10, ymax = 30, alpha = .1,fill = "grey1") +
  geom_hline(yintercept=0, linetype = 2, colour = "black", size = 0.1) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(legend.text = element_text (face = "italic")) +
  theme(axis.title.y = element_text(vjust = +0.1)) +
  theme(axis.title.x = element_text(vjust = 0.5)) +
  theme(legend.position = "none") +
  theme(axis.text.y = element_text(vjust = +0.1)) +
  ggtitle("E") +
  theme(plot.title = element_text(size = 20)) +
  theme(plot.title=element_text(vjust = -63.5)) + 
  theme(plot.title=element_text(hjust = 0.01)) +
  theme(axis.text.y = element_text(size=20)) +
  theme(axis.text.x = element_text(size=20)) +
  scale_y_continuous(breaks=seq(-10,30, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) + 
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+  ###### add margins to plot so that it looks good in grid arrangemnet. Numbers are bottom, left, top, right############## 
scale_colour_manual(values = c("forestgreen", "chartreuse3", "forestgreen", "chartreuse3")) +
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17))




########## Pectenia atlantica ##########

Pa <- ggplot(subset(Water_response_github, Species %in% c("Pectenia atlantica")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
  geom_point(aes(shape= Replicate),size =1.8)+
  geom_line(aes(color = Replicate)) +
  geom_line(linewidth = 0.1) +
  labs(y= "", x = "") +
  ylab(expression(CO[2]~assimilation ~ (nmol ~ g^{-1}~s^{-1}))) +
  annotate("rect", xmin = 1.08, xmax = 1.69, ymin = -10, ymax = 30, alpha = .1,fill = "grey1") + 
  xlab(expression(Thallus~water~content ~ (mm~H[2]*O ~precipitation~equivalent))) +
  coord_cartesian(xlim =c(0,3), ylim = c(-10, 30))+
  geom_hline(yintercept=0, linetype = 2, colour = "black", size =0.1)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(legend.text = element_text (face = "italic")) +
  theme(axis.text.y = element_text(size=20)) +
  theme(axis.text.x = element_text(size=20)) +
  theme(axis.title.y = element_text(vjust = +0.1)) +
  theme(axis.title.x = element_text(vjust = 0.7)) +
  theme(legend.position = "none") +
  theme(axis.text.y = element_text(vjust = +0.1)) +
  ggtitle("G") +
  theme(plot.title = element_text(size = 20)) +
  theme(plot.title=element_text(vjust = -62)) + 
  theme(plot.title=element_text(hjust = 0.01)) + 
  scale_y_continuous(breaks=seq(-10,30, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,3, by=0.5), expand= c(0,0)) + 
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  scale_colour_manual(values = c("navyblue","darkcyan", "deepskyblue2", "navyblue","darkcyan", "deepskyblue2")) +
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17))
    
    
    
  
#### arrange adn plot

layout1 <- rbind(c(1,2), ##row 1, column 2 and 3 same size
                 c(3,4))

layout2 <- rbind(c(1,2), ##row 2 elongated 
                 c(3,3))
##########################################################################



grid.arrange(Lp, Rc, Sl, Ss, ncol = 2, nrow = 2, layout_matrix = layout1)


grid.arrange(Rv, Hl, Pa, ncol = 2, nrow = 2, layout_matrix = layout2)





















