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


#### not suing ### just for exmaple on instrauctions ####
###### Loabria pulmonaria ####
Lp <- ggplot(Water_Resposne_Curves_RStudio, aes(x = Wc_mm, y = Np, colour = Species, fill = Species))   +
geom_point(shape= 21, size = 1.5, color = "darkorange1" )+
labs(y= "NP (µmol m−2 s−1)", x = "PPFD (µmol photons m−2 s−1)") +
ylab(expression( ~ (µmol ~ m^{-2}~s^{-1}))) + #superscript#
xlab(expression(Thallus~water~content ~ (mm~H[2]*O ~precipitation~equivalent))) +
coord_cartesian(xlim =c(0,2.5 ), ylim = c(-20, 50))+ ####set axis limits####
theme(legend.key.size = unit(0.9, "cm"), legend.title=element_blank()) +
geom_hline(yintercept=0, linetype = 2, colour = "black") +   ####line at 0 #####
theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())+
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 10)) +
  theme(legend.text = element_text (face = "italic")) +
theme(axis.title.y = element_text(vjust = +2.5)) +
theme(axis.title.x = element_text(vjust = -1.5)) +  ##### remove back lines and colour from plot to give white background #######
theme(legend.position ="side")+
ggtitle("Lobaria pulmonaria") +
  theme(plot.title = element_text(size = 12)) +
  theme(legend.key.size = unit(0.8, "cm")) +
theme(plot.title = element_text(face = "italic")) +
theme(plot.title=element_text(hjust = 0.8)) +
theme(plot.title=element_text(vjust = -10)) + ####### adjust plot titles #######
scale_y_continuous(breaks=seq(-20,50, by=10), expand= c(0,0)) + 
  scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0))  #######make axis sxles start from desired number,, without the gap below ######
########### ABOVE IS JUST AN EXAMPLE WITH INSTRUCTIONS ################################################
########################################################################
#######################################################################################
####################################################################################################

theme(legend.key.size = unit(0.9, "cm"), legend.title=element_blank()) +

  
  
  
  
  
  
  
  
  
  



















############################################################################################################################################

##################### subset the data set ######################

Np <- with(Water_Resposne_Curves_RStudio,(Np)) ## object created ## 

Replicate <- with(Water_Resposne_Curves_RStudio,(Replicate))


H[2]*O


Lp <- ggplot(subset(Water_Resposne_Curves_RStudio, Species %in% c("Lobaria pulmonaria")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
  geom_point(aes(shape= Replicate),size =1.8)+
  geom_line(aes(color = Replicate))  +
  geom_line(linewidth = 0.1) +
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
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(legend.text = element_text (face = "italic")) +
  theme(axis.title.y = element_text(vjust = +0.1)) +
  
  theme(axis.title.x = element_text(vjust = 0.5)) +
  theme(plot.title=element_text(vjust = -10)) + 
  theme(legend.position = c(0.9, 0.78)) +
  

  theme(legend.key.size = unit(0.5, "cm")) +
  ggtitle("Lobaria pulmonaria") +
  theme(legend.title = element_text( size =14)) +
  theme(plot.title = element_text(face = "italic")) +
  theme(axis.text.y = element_text(size=14)) +
  theme(axis.text.x = element_text(size=14)) +
  
  theme(plot.title=element_text(hjust = 0.7))+
  theme(plot.title = element_text(size = 14)) +
  scale_y_continuous(breaks=seq(-10,30, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) +
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+ 
  scale_colour_manual(values = c("gray22", "slategrey", "slategray3", "gray22", "slategrey", "slategray3")) +
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17)) 
  
 

######### Sticta sylvatica  ##############
Ss <- ggplot(subset(Water_Resposne_Curves_RStudio, Species %in% c("Sticta sylvatica")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
    geom_point(aes(shape= Replicate),size =1.8)+
    geom_line(aes(color = Replicate)) +
    geom_line(linewidth = 0.1) +
    labs(y= "", x = "")  +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.x = element_blank()) +
    coord_cartesian(xlim =c(0,0.6 ), ylim = c(-10, 30))+
   
    geom_hline(yintercept=0, linetype = 2, colour = "black", size = 0.1) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())+
    theme(axis.title.y = element_text(size = 16)) +
    theme(axis.title.x = element_text(size = 16)) +
    theme(legend.text = element_text (face = "italic")) +
    theme(axis.title.y = element_text(vjust = +1)) +
    theme(axis.title.x = element_text(vjust = 0.5)) +
    theme(legend.position = c(0.9, 0.8)) +
    theme(legend.key.size = unit(0.5, "cm")) +
    ggtitle("Sticta sylvatica") +
    theme(legend.title = element_text( size =14)) +
    theme(plot.title = element_text(face = "italic")) +
  theme(axis.text.y = element_text(size=14)) +
  theme(axis.text.x = element_text(size=14)) +
  
    theme(plot.title=element_text(hjust = 0.7)) +
    theme(plot.title=element_text(vjust = -10)) + 
    theme(plot.title = element_text(size = 14)) +
    scale_y_continuous(breaks=seq(-10,50, by=5), expand= c(0,0)) +
    scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) +
    theme(plot.margin=unit(c(0,1,0,0),"cm"))+
    theme(plot.margin=unit(c(0,1,0,0),"cm"))+ 
scale_colour_manual(values = c("goldenrod4","goldenrod3","gold2","goldenrod4","goldenrod3","gold2")) +
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17))
  



##### Hypotrachyna laevigata ###### 

Hl <- ggplot(subset(Water_Resposne_Curves_RStudio, Species %in% c("Hypotrachyna laevigata")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
  geom_point(aes(shape= Replicate),size =1.8)+
  geom_line(aes(color = Replicate)) +
    geom_line(linewidth = 0.1) +
  labs(y= "", x = "") +
  coord_cartesian(xlim =c(0,0.6 ), ylim = c(-10, 30))+
 
  geom_hline(yintercept=0, linetype = 2, colour = "black", size = 0.1) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(legend.text = element_text (face = "italic")) +
  theme(axis.title.y = element_text(vjust = +1)) +
  theme(axis.title.x = element_text(vjust = 0.5)) +
  theme(legend.position = c(0.9, 0.8)) +
  
  theme(legend.key.size = unit(0.5, "cm")) +
  ggtitle("Hypotrachyna laevigata") +
    theme(legend.title = element_text( size =14)) +
  theme(plot.title = element_text(face = "italic")) +
  theme(axis.text.y = element_text(size=14)) +
  theme(axis.text.x = element_text(size=14)) +
  
  theme(plot.title=element_text(hjust = 0.7)) +
  theme(plot.title=element_text(vjust = -10)) + 
    theme(plot.title = element_text(size = 14)) +
  scale_y_continuous(breaks=seq(-10,30, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) +
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+ 
scale_colour_manual(values = c("deeppink2", "lightpink2", "deeppink4", "deeppink2", "lightpink2", "deeppink4"))+
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17))

 

  



##### Sticta limbata ######

Sl <-  ggplot(subset(Water_Resposne_Curves_RStudio, Species %in% c("Sticta limbata")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
  geom_point(aes(shape= Replicate),size =1.8)+
  geom_line(aes(color = Replicate)) +
    geom_line(linewidth = 0.1) +
    labs(y= "", x = "") +
    ylab(expression(CO[2]~assimilation ~ (nmol ~ g^{-1}~s^{-1})))+
    theme(axis.title.x = element_blank()) +
  coord_cartesian(xlim =c(0,0.6 ), ylim = c(-20, 50))+
 
  geom_hline(yintercept=0, linetype = 2, colour = "black", size = 0.1) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(legend.text = element_text (face = "italic")) +
  theme(axis.title.y = element_text(vjust = +0.1)) +
    
  theme(axis.title.x = element_text(vjust = 0.5)) +
  theme(legend.position = c(0.9, 0.8)) +

  theme(legend.key.size = unit(0.5, "cm")) +
  ggtitle("Sticta limbata") +
    theme(legend.title = element_text( size =14)) +
  theme(plot.title = element_text(face = "italic")) +
  theme(axis.text.y = element_text(size=14)) +
  theme(axis.text.x = element_text(size=14)) +
  
  theme(plot.title=element_text(hjust = 0.7)) +
  theme(plot.title=element_text(vjust = -10)) + 
    theme(plot.title = element_text(size = 14)) +
  scale_y_continuous(breaks=seq(-20,50, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) +
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+ 
scale_colour_manual(values = c("purple4","purple2", "orchid3", "purple4","purple2", "orchid3")) +
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17))


##### Ramalina calicaris  ######

Rc <-  ggplot(subset(Water_Resposne_Curves_RStudio, Species %in% c("Ramalina calicaris")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
  geom_point(aes(shape= Replicate),size =1.8)+
    geom_line(aes(color = Replicate)) +
    geom_line(linewidth = 0.1) +
    labs(y= "", x = "")  +
    theme(axis.title.x = element_blank()) +
  coord_cartesian(xlim =c(0,0.6 ), ylim = c(-10, 30))+
 
  geom_hline(yintercept=0, linetype = 2, colour = "black", size = 0.1) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(legend.text = element_text (face = "italic")) +
  theme(axis.title.y = element_text(vjust = +1)) +
  theme(axis.title.x = element_text(vjust = 0.5)) +
  theme(legend.position = c(0.9, 0.78)) +
    
 
  theme(legend.key.size = unit(0.5, "cm")) +
  ggtitle("Ramalina calicaris") +
    theme(legend.title = element_text( size =14)) +
  theme(plot.title = element_text(face = "italic")) +
  theme(axis.text.y = element_text(size=14)) +
  theme(axis.text.x = element_text(size=14)) +
  
  theme(plot.title=element_text(hjust = 0.7)) +
  theme(plot.title=element_text(vjust = -10)) + 
    theme(plot.title = element_text(size = 14)) +
  scale_y_continuous(breaks=seq(-10,30, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) + 
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
scale_colour_manual(values = c("darkorange4","darkorange2", "orange1", "darkorange4","darkorange2", "orange1")) +
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17)) 





##### Ricasolia virens ######


Rv <- ggplot(subset(Water_Resposne_Curves_RStudio, Species %in% c("Lobaria virens")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
    geom_point(aes(shape= Replicate),size =1.8)+
    geom_line(aes(color = Replicate)) +
    geom_line(linewidth = 0.1) +
    labs(y= "", x = "") +
  ylab(expression(CO[2]~gas~exchange ~ (nmol ~ g^{-1}~s^{-1}))) +
   theme(axis.title.x = element_blank()) +
    coord_cartesian(xlim =c(0,0.6 ), ylim = c(-10, 30))+
   
    geom_hline(yintercept=0, linetype = 2, colour = "black", size = 0.1) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 16)) +
    theme(axis.title.x = element_text(size = 16)) +
    theme(legend.text = element_text (face = "italic")) +
    theme(axis.title.y = element_text(vjust = +0.1)) +
    theme(axis.title.x = element_text(vjust = 0.5)) +
    theme(legend.position = c(0.9, 0.8)) +
   
    theme(legend.key.size = unit(0.5, "cm")) +
    theme(legend.title = element_text( size =14)) +
    ggtitle("Ricasolia virens") +
    theme(plot.title = element_text(face = "italic")) +
  theme(axis.text.y = element_text(size=14)) +
  theme(axis.text.x = element_text(size=14)) +
  
    theme(plot.title=element_text(hjust = 0.76)) +
    theme(plot.title=element_text(vjust = -10)) + 
    theme(plot.title = element_text(size = 14)) +
    scale_y_continuous(breaks=seq(-10,30, by=5), expand= c(0,0)) +
    scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) + 
    theme(plot.margin=unit(c(0,1,0,0),"cm"))+  ###### add margins to plot so that it looks good in grid arrangemnet. Numbers are bottom, left, top, right############## 
  scale_colour_manual(values = c("forestgreen", "chartreuse3", "forestgreen", "chartreuse3")) +
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17))






##### Pectenia atlantica ######
Np <- with(Water_Resposne_Curves_RStudio,(Np)) ## object created ## 
Replicate <- with(Water_Resposne_Curves_RStudio, (Replicate))



Pa <- ggplot(subset(Water_Resposne_Curves_RStudio, Species %in% c("Pectenia atlantica")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
  geom_point(aes(shape= Replicate),size =1.8)+
  geom_line(aes(color = Replicate)) +
  geom_line(linewidth = 0.1) +
  labs(y= "", x = "") +
  ylab(expression(CO[2]~assimilation ~ (nmol ~ g^{-1}~s^{-1}))) +
  xlab(expression(Thallus~water~content ~ (mm~H[2]*O ~precipitation~equivalent))) +
  coord_cartesian(xlim =c(0,3 ), ylim = c(-10, 30))+
 
  geom_hline(yintercept=0, linetype = 2, colour = "black", size =0.1)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(legend.text = element_text (face = "italic")) +
  theme(axis.text.y = element_text(size=14)) +
  theme(axis.text.x = element_text(size=14)) +
  
  theme(axis.title.y = element_text(vjust = +0.1)) +
  theme(axis.title.x = element_text(vjust = 0.7)) +
  theme(legend.position = c(0.95, 0.8)) +
  
  theme(legend.key.size = unit(0.5, "cm")) +
  theme(legend.title = element_text( size =14)) +
  ggtitle("Pectenia atlantica") +
  theme(plot.title = element_text(face = "italic")) +
  theme(plot.title = element_text(size = 14)) +
  theme(plot.title=element_text(hjust = 0.76)) +
  
  theme(plot.title=element_text(vjust = -10)) + 
  scale_y_continuous(breaks=seq(-10,30, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,3, by=0.5), expand= c(0,0)) + 
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  scale_colour_manual(values = c("navyblue","darkcyan", "deepskyblue2", "navyblue","darkcyan", "deepskyblue2")) +
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17))



  ######## plot in grid arrangement  ##################
  


############## DO NOT REMOVE!!!!!!!!!!! ###############
plot_grid(ncol = 2,nrow = 4, align = "v", Lp +theme(axis.title.x = element_blank()),
Rc +theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),axis.title.y = element_blank()) +
  theme(axis.title.x = element_blank()), Sl +theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank()) +
  theme(axis.title.x = element_blank()), Ss +theme(axis.title.x = element_blank()), Rv +theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
axis.title.y = element_blank()) +
  theme(axis.title.x = element_blank()), Hl +theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()), Pa, labels = "auto") 
############## DO NOT REMOVE!!!!!!!!!!! ###############




 ####alternative way to plot, with data for omitting specific axis titles etc

plot_grid(ncol = 2,nrow = 4, align = "v", Lp, Rc, Sl, Ss, Rv, Hl, Pa) ### alternative way to plot grid

layout <- rbind(c(1,2), ##row 1, column 2 and 3 same size
                c(3,4),
                c(5,6), ### creates object for layout 
                c(7,7)) 
### above creates object fro layout of plot ## each line represents a row ##


grid.arrange(Lp, Rc, Sl, Ss, Rv, Hl, Pa, ncol = 2, nrow = 4, layout_matrix = layout)



###################### DO NOT DELETE - FINAL PLOT CODE  #######################################
layout1 <- rbind(c(1,2), ##row 1, column 2 and 3 same size
                 c(3,4))


layout2 <- rbind(c(1,2), ##row 2 elongated 
                 c(3,3))
##########################################################################

########### DO NOT DELETE  ############################


###The size of the plot is the size of your current graphics device. Change that, and your plot will automatically adjust. If you want to set the size while saving to disk, use ggsave() and set the plot size arguments. – 


grid.arrange(Lp, Rc, Sl, Ss, ncol = 2, nrow = 2, layout_matrix = layout1)

grid.arrange(Rv, Hl, Pa, ncol = 2, nrow = 2, layout_matrix = layout2)

################################################################################
###################### DO NOT DELETE - FINAL PLOT CODE  #######################################




























































  ###################### WITHOUT L. PULMONARIA ######################
#######################################################################
##########################################################################################################
#######################################################################
  
  
  

  ######### Sticta sylvatica  ##############
Ss <- ggplot(subset(Water_Resposne_Curves_RStudio, Species %in% c("Sticta sylvatica")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
  geom_point(aes(shape= Replicate),size =1.8)+
  geom_line(aes(color = Replicate)) +
  geom_line(linewidth = 0.1) +
  labs(y= "", x = "")  +
  theme(axis.title.x = element_blank()) +
  
  coord_cartesian(xlim =c(0,0.6 ), ylim = c(-10, 30))+
  
  geom_hline(yintercept=0, linetype = 2, colour = "black", size = 0.1) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.title.x = element_text(size = 14)) +
  theme(legend.text = element_text (face = "italic")) +
  theme(axis.title.y = element_text(vjust = +1)) +
  theme(axis.title.x = element_text(vjust = 0.5)) +
  theme(legend.position = c(0.9, 0.8)) +
  
  theme(legend.key.size = unit(0.5, "cm")) +
  ggtitle("Sticta sylvatica") +
  theme(legend.title = element_text( size =12)) +
  theme(plot.title = element_text(face = "italic")) +
  theme(axis.text.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  
  theme(plot.title=element_text(hjust = 0.6)) +
  theme(plot.title=element_text(vjust = -10)) + 
  theme(plot.title = element_text(size = 12)) +
  scale_y_continuous(breaks=seq(-10,50, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) +
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+ 
  scale_colour_manual(values = c("goldenrod4","goldenrod3","gold2","goldenrod4","goldenrod3","gold2")) +
  theme(plot.margin=unit(c(0.1,0.5,0.25,0.15), "cm")) +

  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17))
 



##### Hypotrachyna laevigata ###### 

Hl <- ggplot(subset(Water_Resposne_Curves_RStudio, Species %in% c("Hypotrachyna laevigata")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
  geom_point(aes(shape= Replicate),size =1.8)+
  geom_line(aes(color = Replicate)) +
  geom_line(linewidth = 0.1) +
  labs(y= "", x = "") +
  coord_cartesian(xlim =c(0,0.6 ), ylim = c(-10, 30))+
  xlab(expression(Thallus~water~content ~ (mm~H[2]*O ~precipitation~equivalent))) +
  ylab(expression(CO[2]~gas~exchange ~ (nmol ~ g^{-1}~s^{-1})))+
  geom_hline(yintercept=0, linetype = 2, colour = "black", size = 0.1) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.title.x = element_text(size = 14)) +
  theme(legend.text = element_text (face = "italic")) +
  theme(axis.title.y = element_text(vjust = +1)) +
  theme(axis.title.x = element_text(vjust = 0.5)) +
  theme(legend.position = c(0.9, 0.8)) +
  
  theme(legend.key.size = unit(0.5, "cm")) +
  ggtitle("Hypotrachyna laevigata") +
  theme(legend.title = element_text( size =12)) +
  theme(plot.title = element_text(face = "italic")) +
  theme(axis.text.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  
  theme(plot.title=element_text(hjust = 0.6)) +
  theme(plot.title=element_text(vjust = -10)) + 
  theme(plot.title = element_text(size = 12)) +
  scale_y_continuous(breaks=seq(-10,30, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) +
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+ 
  scale_colour_manual(values = c("deeppink2", "lightpink2", "deeppink4", "deeppink2", "lightpink2", "deeppink4"))+
  theme(plot.margin=unit(c(0.1,0.5,0.25,0.15), "cm")) +
  
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17))







##### Sticta limbata ######

Sl <-  ggplot(subset(Water_Resposne_Curves_RStudio, Species %in% c("Sticta limbata")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
  geom_point(aes(shape= Replicate),size =1.8)+
  geom_line(aes(color = Replicate)) +
  geom_line(linewidth = 0.1) +
  labs(y= "", x = "") +
 
  theme(axis.title.x = element_blank()) +
  coord_cartesian(xlim =c(0,0.6 ), ylim = c(-20, 50))+
  
  geom_hline(yintercept=0, linetype = 2, colour = "black", size = 0.1) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.title.x = element_text(size = 14)) +
  theme(legend.text = element_text (face = "italic")) +
  theme(axis.title.y = element_text(vjust = +0.1)) +
  
  theme(axis.title.x = element_text(vjust = 0.5)) +
  theme(legend.position = c(0.9, 0.8)) +
  
  theme(legend.key.size = unit(0.5, "cm")) +
  ggtitle("Sticta limbata") +
  theme(legend.title = element_text( size =12)) +
  theme(plot.title = element_text(face = "italic")) +
  theme(axis.text.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  
  theme(plot.title=element_text(hjust = 0.6)) +
  theme(plot.title=element_text(vjust = -10)) + 
  theme(plot.title = element_text(size = 12)) +
  scale_y_continuous(breaks=seq(-20,50, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) +
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+ 
  scale_colour_manual(values = c("purple4","purple2", "orchid3", "purple4","purple2", "orchid3")) +
  theme(plot.margin=unit(c(0.1,0.5,0.25,0.15), "cm")) +
  
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17))


##### Ramalina calicaris  ######

Rc <-  ggplot(subset(Water_Resposne_Curves_RStudio, Species %in% c("Ramalina calicaris")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
  geom_point(aes(shape= Replicate),size =1.8)+
  geom_line(aes(color = Replicate)) +
  geom_line(linewidth = 0.1) +
  labs(y= "", x = "")  +
  theme(axis.title.x = element_blank()) +
  ylab(expression(CO[2]~gas~exchange ~ (nmol ~ g^{-1}~s^{-1})))+
  coord_cartesian(xlim =c(0,0.6 ), ylim = c(-10, 30))+
  
  geom_hline(yintercept=0, linetype = 2, colour = "black", size = 0.1) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.title.x = element_text(size = 14)) +
  theme(legend.text = element_text (face = "italic")) +
  theme(axis.title.y = element_text(vjust = +1)) +
  theme(axis.title.x = element_text(vjust = 0.5)) +
  theme(legend.position = c(0.9, 0.78)) +
  
  
  theme(legend.key.size = unit(0.5, "cm")) +
  ggtitle("Ramalina calicaris") +
  theme(legend.title = element_text( size =12)) +
  theme(plot.title = element_text(face = "italic")) +
  theme(axis.text.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  
  theme(plot.title=element_text(hjust = 0.6)) +
  theme(plot.title=element_text(vjust = -10)) + 
  theme(plot.title = element_text(size = 12)) +
  scale_y_continuous(breaks=seq(-10,30, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) + 
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  scale_colour_manual(values = c("darkorange4","darkorange2", "orange1", "darkorange4","darkorange2", "orange1")) +
  theme(plot.margin=unit(c(0.1,0.5,0.25,0.15), "cm")) +
  
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17))






##### Lobaria virens ######
#color ="darkorchid"     , legend.title=element_blank()) #

Rv <- ggplot(subset(Water_Resposne_Curves_RStudio, Species %in% c("Lobaria virens")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
  geom_point(aes(shape= Replicate),size =1.8)+
  geom_line(aes(color = Replicate)) +
  geom_line(linewidth = 0.1) +
  labs(y= "", x = "") +
  
  theme(axis.title.x = element_blank()) +
  coord_cartesian(xlim =c(0,0.6 ), ylim = c(-10, 30))+
  
  geom_hline(yintercept=0, linetype = 2, colour = "black", size = 0.1) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.title.x = element_text(size = 14)) +
  theme(legend.text = element_text (face = "italic")) +
  theme(axis.title.y = element_text(vjust = +0.1)) +
  theme(axis.title.x = element_text(vjust = 0.5)) +
  theme(legend.position = c(0.9, 0.8)) +
  ylab(expression(CO[2]~gas~exchange ~ (nmol ~ g^{-1}~s^{-1})))+
  
  theme(legend.key.size = unit(0.5, "cm")) +
  theme(legend.title = element_text( size =12)) +
  ggtitle("Ricasolia virens") +
  theme(plot.title = element_text(face = "italic")) +
  theme(axis.text.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  
  theme(plot.title=element_text(hjust = 0.6)) +
  theme(plot.title=element_text(vjust = -10)) + 
  theme(plot.title = element_text(size = 12)) +
  scale_y_continuous(breaks=seq(-10,30, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) + 
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+  ###### add margins to plot so that it looks good in grid arrangemnet. Numbers are bottom, left, top, right############## 
scale_colour_manual(values = c("forestgreen", "chartreuse3", "forestgreen", "chartreuse3")) +
  theme(plot.margin=unit(c(0.1,0.5,0.25,0.15), "cm")) +
  
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17))






##### Pectenia atlantica ######
Np <- with(Water_Resposne_Curves_RStudio,(Np)) ## object created ## 
Replicate <- with(Water_Resposne_Curves_RStudio, (Replicate))



Pa <- ggplot(subset(Water_Resposne_Curves_RStudio, Species %in% c("Pectenia atlantica")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
  geom_point(aes(shape= Replicate),size =1.8)+
  geom_line(aes(color = Replicate)) +
  geom_line(linewidth = 0.1) +
  labs(y= "", x = "") +
 
  xlab(expression(Thallus~water~content ~ (mm~H[2]*O ~precipitation~equivalent))) +
  coord_cartesian(xlim =c(0,3 ), ylim = c(-10, 30))+
  
  geom_hline(yintercept=0, linetype = 2, colour = "black", size =0.1)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.title.x = element_text(size = 14)) +
  theme(legend.text = element_text (face = "italic")) +
  theme(axis.text.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  
  theme(axis.title.y = element_text(vjust = +0.1)) +
  theme(axis.title.x = element_text(vjust = 0.7)) +
  theme(legend.position = c(0.95, 0.8)) +
  
  theme(legend.key.size = unit(0.5, "cm")) +
  theme(legend.title = element_text( size =12)) +
  ggtitle("Pectenia atlantica") +
  theme(plot.title = element_text(face = "italic")) +
  theme(plot.title = element_text(size = 12)) +
  theme(plot.title=element_text(hjust = 0.6)) +
  
  theme(plot.title=element_text(vjust = -10)) + 
  scale_y_continuous(breaks=seq(-10,30, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,3, by=0.5), expand= c(0,0)) + 
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  scale_colour_manual(values = c("navyblue","darkcyan", "deepskyblue2", "navyblue","darkcyan", "deepskyblue2")) +
  theme(plot.margin=unit(c(0.1,0.5,0.25,0.15), "cm")) +
  
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17))


##### layout without pulmonaria #######################

no.lp1 <- rbind(c(1,2), 
                c(3,4))

no.lp2 <- rbind(c(1,2),
                c(3.4))


grid.arrange(Rc, Sl, Rv, Ss, ncol = 2, nrow = 3, layout_matrix = no.lp1)


grid.arrange(Hl, Pa, ncol = 2, nrow = 3, layout_matrix = no.lp2)



ylab(expression(CO[2]~gas~exchange ~ (nmol ~ g^{-1}~s^{-1}))) +
  xlab(expression(Thallus~water~content ~ (mm~H[2]*O ~precipitation~equivalent)))









#############################################
#######################################################
#######################################################



######## attempting shaded area on plot ################## 
annotate("rect", xmin = 1.08, xmax = 1.69, ymin = -5, ymax = 15, alpha = .1,fill = "grey1") ### this adds in the shaded area to highligh the rnage of OPtWC
  ###################################################################################################

##### these are plots without the legends and with lables changed from species names to Letters! ### 

Lp <- ggplot(subset(Water_Resposne_Curves_RStudio, Species %in% c("Lobaria pulmonaria")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
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
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(vjust = +0.1)) +
  theme(axis.title.x = element_text(vjust = 0.5)) +
  theme(plot.title=element_text(vjust = -10)) + 
  ggtitle("A") +
  theme(axis.text.y = element_text(size=14)) +
  theme(axis.text.x = element_text(size=14)) +
  theme(plot.title=element_text(hjust = 0.7))+
  theme(plot.title = element_text(size = 14)) +
  scale_y_continuous(breaks=seq(-10,30, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) +
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+ 
  scale_colour_manual(values = c("gray22", "slategrey", "slategray3", "gray22", "slategrey", "slategray3")) +
scale_shape_manual(values = c(1, 0, 2, 19, 15, 17))



######### Sticta sylvatica  ##############

Ss <- ggplot(subset(Water_Resposne_Curves_RStudio, Species %in% c("Sticta sylvatica")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
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
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(vjust = +1)) +
  theme(axis.title.x = element_text(vjust = 0.5)) +
  ggtitle("D") +
  theme(legend.title = element_text( size =14)) +
  theme(axis.text.y = element_text(size=14)) +
  theme(axis.text.x = element_text(size=14)) +
  theme(plot.title=element_text(hjust = 0.7)) +
  theme(plot.title=element_text(vjust = -10)) + 
  theme(plot.title = element_text(size = 14)) +
  scale_y_continuous(breaks=seq(-10,50, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) +
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+ 
  scale_colour_manual(values = c("goldenrod4","goldenrod3","gold2","goldenrod4","goldenrod3","gold2")) +
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17))




##### Hypotrachyna laevigata ###### 

Hl <- ggplot(subset(Water_Resposne_Curves_RStudio, Species %in% c("Hypotrachyna laevigata")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
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
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(vjust = +1)) +
  theme(axis.title.x = element_text(vjust = 0.5)) +
  ggtitle("F") +
  theme(legend.title = element_text( size =14)) +
  theme(axis.text.y = element_text(size=14)) +
  theme(axis.text.x = element_text(size=14)) +
  theme(plot.title=element_text(hjust = 0.7)) +
  theme(plot.title=element_text(vjust = -10)) + 
  theme(plot.title = element_text(size = 14)) +
  scale_y_continuous(breaks=seq(-10,30, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) +
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+ 
  scale_colour_manual(values = c("deeppink2", "lightpink2", "deeppink4", "deeppink2", "lightpink2", "deeppink4"))+
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17))







##### Sticta limbata ######

Sl <-  ggplot(subset(Water_Resposne_Curves_RStudio, Species %in% c("Sticta limbata")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
  geom_point(aes(shape= Replicate),size =1.8)+
  geom_line(aes(color = Replicate)) +
  geom_line(linewidth = 0.1) +
  labs(y= "", x = "") +
  ylab(expression(CO[2]~assimilation ~ (nmol ~ g^{-1}~s^{-1})))+
  theme(axis.title.x = element_blank()) +
  coord_cartesian(xlim =c(0,0.6 ), ylim = c(-20, 50))+
  annotate("rect", xmin = 0.13, xmax = 0.18, ymin = -20, ymax = 50, alpha = .1,fill = "grey1") +
  geom_hline(yintercept=0, linetype = 2, colour = "black", size = 0.1) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(vjust = +0.1)) +
  theme(axis.title.x = element_text(vjust = 0.5)) +
  theme(legend.key.size = unit(0.5, "cm")) +
  ggtitle("C") +
  theme(legend.position = "none") +
  theme(axis.text.y = element_text(size=14)) +
  theme(axis.text.x = element_text(size=14)) +
  theme(plot.title=element_text(hjust = 0.7)) +
  theme(plot.title=element_text(vjust = -10)) + 
  theme(plot.title = element_text(size = 14)) +
  scale_y_continuous(breaks=seq(-20,50, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) +
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+ 
  scale_colour_manual(values = c("purple4","purple2", "orchid3", "purple4","purple2", "orchid3")) +
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17))


##### Ramalina calicaris  ######

Rc <-  ggplot(subset(Water_Resposne_Curves_RStudio, Species %in% c("Ramalina calicaris")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
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
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(legend.text = element_text (face = "italic")) +
  theme(axis.title.y = element_text(vjust = +1)) +
  theme(axis.title.x = element_text(vjust = 0.5)) +
  theme(legend.position = "none") +
  ggtitle("B") +
  theme(legend.title = element_text( size =14)) +
  theme(axis.text.y = element_text(size=14)) +
  theme(axis.text.x = element_text(size=14)) +
  
  theme(plot.title=element_text(hjust = 0.7)) +
  theme(plot.title=element_text(vjust = -10)) + 
  theme(plot.title = element_text(size = 14)) +
  scale_y_continuous(breaks=seq(-10,30, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) + 
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  scale_colour_manual(values = c("darkorange4","darkorange2", "orange1", "darkorange4","darkorange2", "orange1")) +
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17)) 





##### Ricasolia virens ######


Rv <- ggplot(subset(Water_Resposne_Curves_RStudio, Species %in% c("Lobaria virens")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
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
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(legend.text = element_text (face = "italic")) +
  theme(axis.title.y = element_text(vjust = +0.1)) +
  theme(axis.title.x = element_text(vjust = 0.5)) +
  theme(legend.position = "none") +
  ggtitle("E") +
  theme(axis.text.y = element_text(size=14)) +
  theme(axis.text.x = element_text(size=14)) +
  
  theme(plot.title=element_text(hjust = 0.76)) +
  theme(plot.title=element_text(vjust = -10)) + 
  theme(plot.title = element_text(size = 14)) +
  scale_y_continuous(breaks=seq(-10,30, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,0.6, by=0.1), expand= c(0,0)) + 
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+  ###### add margins to plot so that it looks good in grid arrangemnet. Numbers are bottom, left, top, right############## 
scale_colour_manual(values = c("forestgreen", "chartreuse3", "forestgreen", "chartreuse3")) +
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17))




########## Pectenia atlantica ##########

Pa <- ggplot(subset(Water_Resposne_Curves_RStudio, Species %in% c("Pectenia atlantica")), aes(x = Wc_mm, y = Np, colour = Replicate, fill = Replicate)) +
  geom_point(aes(shape= Replicate),size =1.8)+
  geom_line(aes(color = Replicate)) +
  geom_line(linewidth = 0.1) +
  labs(y= "", x = "") +
  ylab(expression(CO[2]~assimilation ~ (nmol ~ g^{-1}~s^{-1}))) +
  annotate("rect", xmin = 1.08, xmax = 1.69, ymin = -10, ymax = 30, alpha = .1,fill = "grey1") + 
  xlab(expression(Thallus~water~content ~ (mm~H[2]*O ~precipitation~equivalent))) +
  coord_cartesian(xlim =c(0,3 ), ylim = c(-10, 30))+
  geom_hline(yintercept=0, linetype = 2, colour = "black", size =0.1)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(legend.text = element_text (face = "italic")) +
  theme(axis.text.y = element_text(size=14)) +
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.title.y = element_text(vjust = +0.1)) +
  theme(axis.title.x = element_text(vjust = 0.7)) +
  theme(legend.position = "none") +
  ggtitle("G") +
  theme(plot.title = element_text(size = 14)) +
  theme(plot.title=element_text(hjust = 0.76)) +
  theme(plot.title=element_text(vjust = -10)) + 
  scale_y_continuous(breaks=seq(-10,30, by=5), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,3, by=0.5), expand= c(0,0)) + 
  theme(plot.margin=unit(c(0,1,0,0),"cm"))+
  scale_colour_manual(values = c("navyblue","darkcyan", "deepskyblue2", "navyblue","darkcyan", "deepskyblue2")) +
  scale_shape_manual(values = c(1, 0, 2, 19, 15, 17))
    
    
    
    

layout1 <- rbind(c(1,2), ##row 1, column 2 and 3 same size
                 c(3,4))


layout2 <- rbind(c(1,2), ##row 2 elongated 
                 c(3,3))
##########################################################################


########### DO NOT DELETE  ############################


###The size of the plot is the size of your current graphics device. Change that, and your plot will automatically adjust. If you want to set the size while saving to disk, use ggsave() and set the plot size arguments. – 


grid.arrange(Lp, Rc, Sl, Ss, ncol = 2, nrow = 2, layout_matrix = layout1)


grid.arrange(Rv, Hl, Pa, ncol = 2, nrow = 2, layout_matrix = layout2)





















