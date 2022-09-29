#Pakker
library(readr)
library(devtools)
library(ggplot2)
library(tidyr)
library(knitr)
library(ggthemes)
library(dplyr)
library(plyr)
library(stringr)
library(scales)
library(grid)
library(RColorBrewer)
library(tidyverse)
library(readxl)
library(gridExtra)

options(scipen=999)

#Plots-opsætning
theme_set(theme_light())
ggplot <- function(...) ggplot2::ggplot(...) + scale_color_brewer(palette="Set1", direction=1)
th <- theme(title = element_text(colour = "#404040"),
            plot.background=element_rect(fill="#f3f3f3"),  
            panel.background = element_rect(fill="#f3f3f3"), 
            legend.background = element_rect(fill="#f3f3f3"),
            plot.subtitle = element_text(color="#666666"),
            plot.caption = element_text(color="#AAAAAA", size=8),
            legend.key = element_rect(fill = "#f3f3f3", colour = "#f3f3f3"),
            plot.margin = unit(c(0.5, 0.7, 0.5, 0.7), "cm"))
# axis.title.x=element_blank() tilføjes, hvis x-titel ikke skal med
# axis.text.x=element_text(angle=90,hjust=1,vjust=0.5) tilføjes, hvis x-aksens navne skal roteres
# panel.grid.major.x = element_blank() tilføjes, hvis grid-stregerne skal fjernes (.minor.x eller y)
# HUSK at angive +labs(title = "", subtitle = "", x="", y="", caption = "Kilde:")
# Gemme plots: ggsave("p1.png", plot = p1, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")
# To liner plot.subtitle

#Data
setwd("C:/Users/Andreas/Dropbox/Forsvaret/HO/Afgangsprojekt/Data")

Svar <- read_excel("Svar fra spørgeskema.xlsx") #DONE
Minimum <- read_excel("Minimum faktorer.xlsx") #Done
Onske <- read_excel("Ønske faktorer.xlsx") #Done
Udsagn <- read_excel("Udsagn.xlsx") #DONE
Uddybning <- read_excel("Uddybning af prioritet.xlsx") #DONE
Alder <- read_excel("Alder.xlsx") #DONE
Bopæl <- read_excel("Bopæl.xlsx") #DONE
Info <- read_excel("Optaget.xlsx") #DONE

#Infomationer om VPL - FPS
p1 <- ggplot(Info, aes(x=factor(År), y=`Antal,TJ`, fill=Køn, label = scales::percent(Procent, accuracy=0.1))) + geom_bar(stat = "identity", position = "dodge") +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) +
  labs(title = "Fordeling af værnepligtige", x=NULL, y="Antal", caption = "Kilde: SVSYS-data, trukket d. 11/8-2022", fill=NULL) + 
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) + 
  theme(legend.position="bottom") + scale_fill_brewer(palette = "Paired") + th 

ggsave("Info.pdf", plot = p1 , width = 20, height = 15, units = "cm", path = "C:/Users/Andreas/Dropbox/Forsvaret/HO/Afgangsprojekt/Data")

p2 <- ggplot(Info, aes(x=Køn, y=`Antal,AF`, fill=Køn, label = scales::percent(Procent2, accuracy=0.1))) + geom_bar(stat = "identity", position = "dodge") +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) +
  labs(title = "Fordeling af afgået værnepligtige", subtitle = "Ikke opgjort for 2022, da uddannelsen er igangværende", x=NULL, y="Antal", caption = "Kilde: SVSYS-data, trukket d. 11/8-2022", fill=NULL) + 
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) + 
  theme(legend.position="none") + scale_fill_brewer(palette = "Paired") + th 

ggsave("Afgået.pdf", plot = p2 , width = 20, height = 15, units = "cm", path = "C:/Users/Andreas/Dropbox/Forsvaret/HO/Afgangsprojekt/Data")

p3 <- ggplot(Alder, aes(x=Aldersfordeling, y=Antal1, fill=factor(År), label = scales::percent(Procent1, accuracy=0.1))) + geom_bar(stat = "identity", position = "dodge") +
  geom_text(position = position_dodge(width = 1),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  labs(title = "Aldersfordeling af værnepligtige", x=NULL, y="Antal", caption = "Kilde: SVSYS-data, trukket d. 11/8-2022", fill=NULL) + 
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust=1)) + scale_fill_brewer(palette = "Paired") + th 

ggsave("Alder.pdf", plot = p3 , width = 26, height = 15, units = "cm", path = "C:/Users/Andreas/Dropbox/Forsvaret/HO/Afgangsprojekt/Data")

p4 <- ggplot(Bopæl, aes(x=Bopæl, y=Antal, fill=factor(År), label = scales::percent(Procent, accuracy=0.1))) + geom_bar(stat = "identity", position = "dodge") +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) +
  labs(title = "Fordeling af værnepligtige på regioner", subtitle = "Udenlandske statsborgere og personer med ubekendt adresse er ikke medtaget", x=NULL, y="Antal", caption = "Kilde: SVSYS-data, trukket d. 11/8-2022", fill=NULL) + 
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) + 
  theme(legend.position="bottom") + scale_fill_brewer(palette = "Paired") + th 

ggsave("Bopæl.pdf", plot = p4 , width = 20, height = 15, units = "cm", path = "C:/Users/Andreas/Dropbox/Forsvaret/HO/Afgangsprojekt/Data")

#Spørgeskema

##Prioritet
Svar1 <- Svar[Svar$Spørgsmål == 1,]

p5 <- ggplot(Svar1, aes(x=Svar, y=Antal, fill=Svar, label = scales::percent(Procent/100, accuracy=0.1))) + geom_bar(stat = "identity", position = "dodge") +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) +
  labs(title = "Mobiliseringsværnepligten som 1. prioritet", x=NULL, y="Antal", caption = "Kilde: Egen undersøgelse", fill=NULL) + 
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) + 
  theme(legend.position="none") + scale_fill_brewer(palette = "Paired") + th 

ggsave("Prioritet.pdf", plot = p5 , width = 20, height = 15, units = "cm", path = "C:/Users/Andreas/Dropbox/Forsvaret/HO/Afgangsprojekt/Data")

##Kendskab
Svar2 <- Svar[Svar$Spørgsmål == 4,]
Svar2$Svar2 <- str_wrap(Svar2$Svar, width = 20)

p6 <- ggplot(Svar2, aes(x=Svar2, y=Antal, fill=Svar, label = scales::percent(Procent/100, accuracy=0.1))) + geom_bar(stat = "identity", position = "dodge") +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) +
  labs(title = "Kendskab til Mobiliseringsværnepligten", x=NULL, y="Antal", caption = "Kilde: Egen undersøgelse", fill=NULL) + 
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) + 
  theme(legend.position="none") + scale_fill_manual(values = c("#08519C","#3182BD","#6BAED6","#AAD6F3","#BDD7E7")) + th #"#D9F2FE"

ggsave("Kendskab.pdf", plot = p6 , width = 20, height = 15, units = "cm", path = "C:/Users/Andreas/Dropbox/Forsvaret/HO/Afgangsprojekt/Data")

##Vægtfordeling af prioriteter
Svar3 <- Svar[Svar$Spørgsmål == 6,]

p7 <- ggplot(Svar3, aes(x=Svar, y=Procent, fill=Svar, label = scales::percent(Procent/100, accuracy=0.1))) + geom_bar(stat = "identity", position = "dodge") + scale_y_continuous(limits = c(0,38), breaks = seq(0, 40, by = 5)) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) +
  labs(title = "Ønsket faktorer tilstede", subtitle = "Gennemsnitlig betragtning for holdet", x=NULL, y="Procent", caption = " ", fill=NULL) + 
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) + 
  theme(legend.position="none") + scale_fill_brewer(palette = "Blues", direction = -1) + th 

#ggsave("Vægtfordeling, ønske.pdf", plot = p7 , width = 20, height = 15, units = "cm", path = "C:/Users/Andreas/Dropbox/Forsvaret/HO/Afgangsprojekt/Data")

Svar4 <- Svar[Svar$Spørgsmål == 8,]

p8 <- ggplot(Svar4, aes(x=Svar, y=Procent, fill=Svar, label = scales::percent(Procent/100, accuracy=0.1))) + geom_bar(stat = "identity", position = "dodge") + scale_y_continuous(limits = c(0,38), breaks = seq(0, 40, by = 5)) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) +
  labs(title = "Minimum faktorer tilstede", subtitle = "Gennemsnitlig betragtning for holdet", x=NULL, y="Procent", caption = "Kilde: Egen undersøgelse", fill=NULL) + 
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) + 
  theme(legend.position="none") + scale_fill_brewer(palette = "Blues", direction = -1) + th 

#ggsave("Vægtfordeling, minimum.pdf", plot = p7 , width = 20, height = 15, units = "cm", path = "C:/Users/Andreas/Dropbox/Forsvaret/HO/Afgangsprojekt/Data")

ggsave("Vægtfordeling.pdf", plot = grid.arrange(p7,p8, ncol=2, nrow=1), width = 40, height = 15, units = "cm", path = "C:/Users/Andreas/Dropbox/Forsvaret/HO/Afgangsprojekt/Data")

##Motiveret
Svar5 <- Svar[Svar$Spørgsmål == 12,]
Svar5$Svar <- as.character(Svar5$Svar)
Svar5$Svar <- factor(Svar5$Svar, levels=unique(Svar5$Svar))

p9 <- ggplot(Svar5, aes(x=factor(Svar), y=Antal, label = scales::percent(Procent/100, accuracy=0.1))) + geom_bar(stat = "identity", position = "dodge", fill="#1F78B4") + coord_flip() +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            hjust = -0.5,    # nudge above top of bar
            size = 3) +
  labs(title = "Motivationen i Mobiliseringsværnepligten", subtitle = "Status efter 4 ugers uddannelse", x=NULL, y="Antal", caption = "Kilde: Egen undersøgelse", fill=NULL) + scale_y_continuous(limits = c(0,45), breaks = seq(0, 43, by = 5)) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) + 
  theme(legend.position="none") + th 

#ggsave("Motiveret.pdf", plot = p9 , width = 20, height = 15, units = "cm", path = "C:/Users/Andreas/Dropbox/Forsvaret/HO/Afgangsprojekt/Data")

##Anbefaling
Svar6 <- Svar[Svar$Spørgsmål == 13,]
Svar6$Svar <- as.character(Svar6$Svar)
Svar6$Svar <- factor(Svar6$Svar, levels=unique(Svar6$Svar))

p10 <- ggplot(Svar6, aes(x=factor(Svar), y=Antal, label = scales::percent(Procent/100, accuracy=0.1))) + geom_bar(stat = "identity", position = "dodge", fill="#1F78B4") + coord_flip() +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            hjust = -0.5,    # nudge above top of bar
            size = 3) +
  labs(title = "Sandsynligheden for at anbefale Mobiliseringsværnepligten", subtitle = "Status efter 4 ugers uddannelse" , x=NULL, y="Antal", caption = "Kilde: Egen undersøgelse", fill=NULL) + scale_y_continuous(limits = c(0,45), breaks = seq(0, 43, by = 5)) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) + 
  theme(legend.position="none") + th 

#ggsave("Anbefaling.pdf", plot = p10 , width = 20, height = 15, units = "cm", path = "C:/Users/Andreas/Dropbox/Forsvaret/HO/Afgangsprojekt/Data")

ggsave("mot+sand, sammen.pdf", plot = grid.arrange(p9,p10, ncol=2, nrow=1), width = 42, height = 20, units = "cm", path = "C:/Users/Andreas/Dropbox/Forsvaret/HO/Afgangsprojekt/Data")

#Uddybning
Uddybning$Emne <- str_wrap(Uddybning$Emne, width = 20)
Uddybning$Emne <- with(Uddybning, reorder(Emne, desc(Antal)))
  
p11 <- ggplot(Uddybning,aes(x = Emne, y = Antal, fill = Prioritet, label = scales::percent(Procent/100, accuracy=0.1))) + geom_bar(stat = "identity", position = "dodge") + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) +
  labs(title = "Uddybning af prioritet", x=NULL, y="Antal", caption = "Kilde: Egen undersøgelse", fill=NULL) + 
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) + 
  theme(legend.position="bottom") + scale_fill_manual(values = c("#A6CEE3","#6BAED6","#1F78B4")) + th 

ggsave("Uddybning.pdf", plot = p11 , width = 35, height = 15, units = "cm", path = "C:/Users/Andreas/Dropbox/Forsvaret/HO/Afgangsprojekt/Data")

#Ønske faktorer
Ønske <- gather(Onske, Prioritet, Antal,`1. Prioritet`:`5. Prioritet`)
Ønske$Faktorer <- str_wrap(Ønske$Faktorer, width = 15)

p12 <- ggplot(Ønske, aes(fill=factor(Prioritet, levels = c("5. Prioritet", "4. Prioritet", "3. Prioritet", "2. Prioritet", "1. Prioritet")), y=Antal, x=Faktorer)) + geom_bar(position="stack", stat="identity") + 
  labs(title = "Prioritering af ønsket faktorer", x=NULL, y="Antal", caption = "Kilde: Egen undersøgelse", fill=NULL) + scale_y_continuous(limits = c(0,83), breaks = seq(0, 80, by = 10)) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 15)) + scale_fill_manual(values = c("#BDD7E7","#AAD6F3","#6BAED6","#3182BD","#08519C")) + guides(fill=guide_legend(reverse = T)) + th 

ggsave("Ønske faktor, prioritet.pdf", plot = p12 , width = 40, height = 20, units = "cm", path = "C:/Users/Andreas/Dropbox/Forsvaret/HO/Afgangsprojekt/Data")

#Minimum faktorer
Minimum1 <- gather(Minimum, Prioritet, Antal,`1. Prioritet`:`5. Prioritet`)
Minimum1$Faktorer <- str_wrap(Minimum1$Faktorer, width = 15)

p13 <- ggplot(Minimum1, aes(fill=factor(Prioritet, levels = c("5. Prioritet", "4. Prioritet", "3. Prioritet", "2. Prioritet", "1. Prioritet")), y=Antal, x=Faktorer)) + geom_bar(position="stack", stat="identity") + 
  labs(title = "Prioritering af minimum faktorer", x=NULL, y="Antal", caption = "Kilde: Egen undersøgelse", fill=NULL) + scale_y_continuous(limits = c(0,83),  breaks = seq(0, 80, by = 10)) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 15)) + scale_fill_manual(values = c("#BDD7E7","#AAD6F3","#6BAED6","#3182BD","#08519C")) + guides(fill=guide_legend(reverse = T)) + th 

ggsave("Minimum faktor, prioritet.pdf", plot = p13 , width = 40, height = 20, units = "cm", path = "C:/Users/Andreas/Dropbox/Forsvaret/HO/Afgangsprojekt/Data")

#Udsagn
Udsagn1 <- gather(Udsagn, Vigtighed, Antal,`Ikke vigtigt`:`Ekstremt vigtigt`)
Udsagn1$Udsagn <- str_wrap(Udsagn1$Udsagn, width = 12)
Udsagn1$procent <- round(Udsagn1$Antal/Udsagn1$`I alt`*100,2) 

p14 <- ggplot(Udsagn1, aes(fill=factor(Vigtighed, levels = c("Ikke vigtigt", "Lidt vigtigt", "Moderat vigtigt", "Meget vigtigt", "Ekstremt vigtigt")), y=procent, x=Udsagn)) + geom_bar(position="stack", stat="identity") + 
  labs(title = "Betydningen af udsagn for individet", x=NULL, y="Procent", caption = "Kilde: Egen undersøgelse", fill=NULL) + scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) + 
  theme(legend.position="bottom", axis.text.x = element_text(angle = 15)) + scale_fill_manual(values = c("#BDD7E7","#AAD6F3","#6BAED6","#3182BD","#08519C")) + guides(fill=guide_legend(reverse = F)) + th

ggsave("Udsagn.pdf", plot = p14 , width = 40, height = 20, units = "cm", path = "C:/Users/Andreas/Dropbox/Forsvaret/HO/Afgangsprojekt/Data")




