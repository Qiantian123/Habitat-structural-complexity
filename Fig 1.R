# 1 set working place and loading R packages---------
setwd("D:/R work place/artificial plant")

library(ggplot2)
library(dplyr)
library(tidyr)
library(Rmisc)
library(patchwork)
library(ggimage)
library(ggpubr)

# 2. set figure format--------

mytheme <- theme_bw() + 
  theme(panel.background = element_rect(),
        panel.grid = element_blank(),
        axis.title = element_text(size = 10,color = "black"),
        axis.text = element_text(size = 9,color = "black"),
        strip.text = element_text(size = 12),
        aspect.ratio = 3/2,
        legend.key.size = unit(0.15, "inches"),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10))

# 3. loading fish data ----
nutrient <- read.csv("./data/nutrient.csv",header = T) %>%
  filter(Days != 0,Days != 3,Days != 6,Days != 9,
         Days != 12,Days != 15,Days != 18)

data2 <- gather(nutrient,key = "nutrient",
                value = "biomass",18) %>%
  summarySE(measurevar = "biomass",
            groupvars = c("Days",
                          "Plant",
                          "Mandarin",
                          "nutrient"),
            na.rm = T) %>% na.omit()

# plot carp biomass
data2$Plant <- factor(data2$Plant,levels = c("Low","High"),
                      labels = c("LPD", "HPD"))
data2$Mandarin <- factor(data2$Mandarin,levels = c("Absent","Present"),
                         labels = c("Mandarin absent", "Mandarin present"))

tiff("./figures/carp_biomass.tiff",res = 900, compression = "lzw",
     width = 14,height = 14,units = "cm")

p1<-ggplot(data2,aes(factor(Plant),biomass)) + 
  geom_bar(aes(fill = Mandarin), show.legend = T,
           stat = "identity",position = "dodge",width = 0.5) + 
  geom_errorbar(aes(factor(Plant),group=Mandarin,ymin = biomass - se,
                    ymax = biomass + se),position = position_dodge(width = 0.5),width = 0.2,
                size = 0.2) + 
  facet_grid(~nutrient,scales = "free_y") +
  annotate("text",x = 0.8,y = 2.9,label = " ",size=5,family = "serif") +
  
  labs(x = "", 
       y = expression(Crucian~carp~biomass~(g~m^{-2}))) + mytheme +
theme(strip.text = element_blank(),
      legend.position = "top",
      legend.direction = "horizontal")
p1
dev.off()


nutrient <- read.csv("./data/nutrient.csv",header = T) %>%
  filter(Days != 0,Days != 3,Days != 6,Days != 9,
         Days != 12,Days != 15,Days != 18)

data2 <- gather(nutrient,key = "nutrient",
                value = "biomass",20) %>%
  summarySE(measurevar = "biomass",
            groupvars = c("Days",
                          "Plant",
                          "Mandarin",
                          "nutrient"),
            na.rm = T) %>% na.omit()

# plot carp number
data2$Plant <- factor(data2$Plant,levels = c("Low","High"),
                      labels = c("LPD", "HPD"))
data2$Mandarin <- factor(data2$Mandarin,levels = c("Absent","Present"),
                         labels = c("Mandarin absent", "Mandarin present"))

tiff("./figures/carp_number.tiff",res = 900, compression = "lzw",
     width = 14,height = 14,units = "cm")

p2<-ggplot(data2,aes(factor(Plant),biomass)) + 
  geom_bar(aes(fill = Mandarin), show.legend = T,
           stat = "identity",position = "dodge",width = 0.5) + 
  geom_errorbar(aes(factor(Plant),group=Mandarin,ymin = biomass - se,
                    ymax = biomass + se),position = position_dodge(width = 0.5),width = 0.2,
                size = 0.2) + 
  facet_grid(~nutrient,scales = "free_y") +
  annotate("text",x = 0.8,y = 42,label = " ",size=5,family = "serif") +
  
  labs(x = "", 
       y = expression(Crucian~carp~number~(ind.))) + mytheme +
  theme(strip.text = element_blank(),
        legend.position = "none",
        legend.direction = "horizontal")
p2
dev.off()

carp <- ggarrange(p1, p2, common.legend = TRUE, legend="top",label.x = 0.065,label.y = 1,
                  font.label = list(size = 14, color = "black", face = "plain", family = "serif"),   
                  labels = c(" a","b") )
carp

tiff("./figures/carp.tiff",compression = "lzw",res = 900,
     width = 12,height = 8,units = "cm")

carp

dev.off()
