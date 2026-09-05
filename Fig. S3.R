# 1 set working place and loading R packages---------
setwd("D:/R work place/artificial plant")
library(tidyverse)
library(Rmisc)
library(ggpubr)
library(data.table)
library(dplyr)

# 2. set figure format--------
mytheme <- theme_bw() + 
  theme(panel.background = element_rect(),
        panel.grid = element_blank(),
        axis.title = element_text(size = 10,color = "black"),
        axis.text = element_text(size = 8,color = "black"),
        strip.text = element_text(size = 10),
        aspect.ratio = 13/10,
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 7))

# 3. loading nutrient data ----
data <- read.csv("./data/nutrient.csv",header = T)%>%
  filter(Days == 21)
# plot nutrients
data$Plant <- factor(data$Plant,levels = c("Low","High"),
                     labels = c("Low","High"))

data$Mandarin <- factor(data$Mandarin,levels = c("Absent","Present"),
                        labels = c("Piscivore absent","Piscivore present"))

## phytoplankton composition-------
data2 <- gather(data,key = "index",value = "biomass",23:29)
data3 <- summarySE(data2,measurevar = "biomass",
                   groupvars = c("Plant",
                                 "Mandarin",
                                 "index"),
                   na.rm = T) %>% na.omit() 
data3$index <- factor(data3$index,labels = c("Bacillariophyta","Chlorophyta","Chrysophyta","Cryptophyta",
                                             "Cyanophyta","Euglenophyta","Pyrrophyta"))

tiff("./figures/phyt_community.tiff",res = 900, compression = "lzw",
     width = 14,height = 9,units = "cm")
ggplot(data3,aes(Plant,biomass,fill = index)) + 
  scale_fill_brewer(palette = 'Set3')+guides(fill=guide_legend(reverse=F,title=NULL,ncol=1))+
  geom_bar(aes(group = Mandarin), show.legend = T,
           stat = "identity",position = "fill",width = 0.7) + 
  facet_wrap(.~Mandarin,scales = "free_y",ncol = 2) +
  labs(x = "Plant density", 
       y = expression(Biomass~percentage~('%'))) + mytheme +
  scale_x_discrete(breaks = c("Low","High")) +
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),
                     labels = c(0,25,50,75,100)) +
  theme(legend.position = "right")
dev.off()
