# set working place and loading R packages---------
setwd("D:/R work place/artificial plant")
library(ggplot2)
library(dplyr)
library(Rmisc)
library(tidyr)
library(ggpubr)
setwd("D:/R work place/artificial plant")
################# set my theme
mytheme <- theme_bw() + 
  theme(panel.background = element_rect(),
        panel.grid = element_blank(),
        axis.title = element_text(size = 10.5,color = "black"),
        axis.text = element_text(size = 10,color = "black"),
        strip.text = element_text(size = 10.5),
        aspect.ratio = 3/2,
        legend.key.size = unit(0.15, "inches"),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10))
###  zppp  #####
data <- read.csv("./data/nutrient.csv") %>% 
  select(Days,Plant,Mandarin,zppp) %>%
  mutate(zppp = zppp /1000/ 0.29) %>%
  filter(Days == 21)


data2 <- gather(data, key = "zppp",value = "biom",4)
data2$zppp <- factor(data2$zppp, 
                     levels = c("zppp"),
                     labels = c("zppp"))
data2$Plant <- factor(data2$Plant,levels = c("Low","High"),
                      labels = c("LPD", "HPD"))
data2$Mandarin <- factor(data2$Mandarin,levels = c("Absent","Present"),
                         labels = c("Mandarin absent", "Mandarin present"))

data_zppp <- summarySE(data2,measurevar = 'biom',
                       groupvars = c("Plant","Mandarin","Days","zppp")) %>% na.omit()

zppp <- ggplot(data_zppp, aes(factor(Plant),biom,fill = Mandarin)) + 
  geom_bar(stat = "identity",position = "dodge",
           width = 0.5) +
  geom_errorbar(aes(ymin = biom - se,
                    ymax = biom + se),
                position = position_dodge(width = 0.5),
                size = 0.2,width = 0.2) +
  theme_bw() + 
  mytheme+theme(legend.key.size = unit(0.15, "inches"),
                legend.position = "top",
                strip.text = element_blank()) +
  #  annotate("text",x = 1,y = 10,label = "") +
  
  labs(x = "",y = expression(Zooplankton:phytoplankton~ratio))
tiff("./figures/zppp.tiff",compression = "lzw",res = 900,
     width = 8,height = 8,units = "cm")
zppp
dev.off()

###  Zoopsize  #####
data <- read.csv("./data/nutrient.csv") %>% 
  select(Days,Plant,Mandarin,Zoopsize) %>%
  mutate(Zoopsize = Zoopsize) %>%
  filter(Days == 21)


data2 <- gather(data, key = "Zoopsize",value = "biom",4)
data2$Zoopsize <- factor(data2$Zoopsize, 
                         levels = c("Zoopsize"),
                         labels = c("Zoopsize"))
data2$Plant <- factor(data2$Plant,levels = c("Low","High"),
                      labels = c("LPD", "HPD"))
data2$Mandarin <- factor(data2$Mandarin,levels = c("Absent","Present"),
                         labels = c("Mandarin absent", "Mandarin present"))

data_Zoopsize <- summarySE(data2,measurevar = 'biom',
                           groupvars = c("Plant","Mandarin","Days","Zoopsize")) %>% na.omit()

Zoopsize <- ggplot(data_Zoopsize, aes(factor(Plant),biom,fill = Mandarin)) + 
  geom_bar(stat = "identity",position = "dodge",
           width = 0.5) +
  geom_errorbar(aes(ymin = biom - se,
                    ymax = biom + se),
                position = position_dodge(width = 0.5),
                size = 0.2,width = 0.2) +
  theme_bw() + 
  mytheme+theme(legend.key.size = unit(0.15, "inches"),
                legend.position = "none",
                strip.text = element_blank()) +
  #  annotate("text",x = 1,y = 10,label = "") +
  
  labs(x = "",y = expression(Zooplankton~size~(μg~ind.^{-1})))
tiff("./figures/Zoopsize.tiff",compression = "lzw",res = 900,
     width = 8,height = 8,units = "cm")
Zoopsize
dev.off()

Zoopsizezppp <- ggarrange(zppp, Zoopsize, label.x = 0.08,label.y = 1,nrow=1,align = "v",
                          font.label = list(size = 14, color = "black", face = "plain", family = "serif"),   
                          common.legend = T,labels = c("a","b") )
Zoopsizezppp
tiff("./figures/Zoopsizezppp.tiff",compression = "lzw",res = 900,
     width = 14,height = 9,units = "cm")

Zoopsizezppp
dev.off()
