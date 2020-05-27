# Stephen Burkot
# 5.25.2020

library(ggplot2)
library(xkcd)
library(extrafont)
library(tidyr)
library(RColorBrewer)
# library(sf)
# library(rnaturalearth)
# library(rnaturalearthdata)
# library(rgeos)

vignette("xkcd-intro")
rm(list=ls())

library(extrafont)
library(ggplot2)
library(ggalt)
if( 'xkcd' %in% fonts()) {p <- ggplot() + geom_point(aes(x=mpg, y=wt), data=mtcars) + theme(text = element_text(size = 16, family = "xkcd"))
} else {
  warning("Not xkcd fonts installed!")
  p <- ggplot() + geom_point(aes(x=mpg, y=wt), data=mtcars)
  }
# 
#  download.file("http://simonsoftware.se/other/xkcd.ttf",
#                 + dest="xkcd.ttf", mode="wb")
#  system("mkdir ~/.fonts")
#  system("cp xkcd.ttf ~/.fonts")
#  font_import(path="C:\\Users\\sburk\\Documents\\Misc\\CoffeeData", pattern = "[X/x]kcd", prompt=FALSE)
 fonts()
#  fonttable()
 if(.Platform$OS.type != "unix") {
   ## Register fonts for Windows bitmap output
    loadfonts(device="win")
   } else {
    loadfonts()
   }

world<-ne_countries(scale="medium", returnclass ="sf")

setwd("C:/Users/sburk/Documents/Misc/CoffeeData")
master<-read.csv("InstantData.csv", header=TRUE, sep=",", stringsAsFactors = FALSE, na.strings = c("","NA"))

# add placement instead of rank
master$placement<-NA
master$placement[which(master$Rank==38)]<-1
master$placement[which(master$Rank==16)]<-2
master$placement[which(master$Rank==8)]<-3
master$placement[which(master$Rank==4)]<-4
master$placement[which(master$Rank==2)]<-5
master$placement[which(master$Rank==1)]<-6


# Create cols for each round
master$round1<-0
master$round2<-0
master$round3<-0
master$round4<-0
master$round5<-0
master$round6<-0

master$round1[which(master$placement>=1)]<-1
master$round2[which(master$placement>=2)]<-2
master$round3[which(master$placement>=3)]<-3
master$round4[which(master$placement>=4)]<-4
master$round5[which(master$placement>=5)]<-5
master$round6[which(master$placement>=6)]<-6


# Intro data viz
master$Bean<-factor(master$Bean)
master$Fairtrade.Mark<-factor(master$Fairtrade.Mark)
g1<-ggplot(master, aes(x=Name, y=Cost_per_100g))+geom_point(aes(col=Bean, shape=Fairtrade.Mark, size=2))+theme_minimal()+theme(axis.text.x = element_text(angle=90, vjust=0.5))+
  geom_segment(aes(x=Name, xend=Name, y=0, yend=Cost_per_100g))+ theme(text = element_text(size = 18, family = "xkcd"))+ylab("Cost in GBP per 100g")

# Create a long dataset for graphing bean type per round
master_long_bean<-gather(master, round, value, round1:round6, factor_key = TRUE)

master_long_bean<-master_long_bean[which(master_long_bean$value>0),]

g3<-ggplot(master_long_bean, aes(x=value, fill=Bean))+geom_bar(position="fill")+theme_classic()
g4<-ggplot(master_long_bean, aes(x=value, fill=Bean))+geom_bar()+theme_minimal()+ theme(text = element_text(size = 16, family = "xkcd"))+xlab("Round")

# Cost vs round placement
plot(master_long_bean$Cost_per_100g, master_long_bean$value)
fit<-lm(master_long_bean$value~master_long_bean$Cost_per_100g) #intercept=0.8649, slope=0.2487
summary(fit)

g6<-ggplot(master_long_bean, aes(x=Cost_per_100g, y=value ))+geom_jitter(width=0, height=0.05, aes(col=Manufacturer, size=1.5))+theme_minimal()+ theme(text = element_text(size = 16, family = "xkcd"))+xlab("Cost in GBP per 100g")+ylab("Round")+
  scale_y_continuous(breaks=seq(1,6,1))

g12<-ggplot(master_long_bean, aes(x=value, y=Cost_per_100g ))+geom_jitter(width=0, height=0.05, aes(col=Manufacturer, size=1.5))+theme_minimal()+ theme(text = element_text(size = 16, family = "xkcd"))+ylab("Cost in GBP per 100g")+xlab("Round")+
    scale_x_continuous(breaks=seq(1,6,1))

# Organic plot and encircle
organic_select<-master_long_bean[master_long_bean$Cost_per_100g>3.85 & master_long_bean$Cost_per_100g<4.85 & master_long_bean$value>0.9 & master_long_bean$value<2.15 & master_long_bean$Organic==1,]

master_long_bean$Organic<-factor(master_long_bean$Organic)
g9<-ggplot(master_long_bean, aes(x=Cost_per_100g, y=value ))+geom_jitter(width=0, height=0.1, aes(col=Organic, size=1.5))+theme_minimal()+ theme(text = element_text(size = 16, family = "xkcd"))+xlab("Cost in GBP per 100g")+ylab("Round")+
  scale_y_continuous(breaks=seq(1,6,1))+geom_encircle(aes(x=Cost_per_100g, y=value), data=organic_select, color="turquoise2", size=2)+labs(fill="Organic?")+annotate("text", x=3.7, y=2.4, label="The organic triangle", family="xkcd", color="turquoise2")

# Average cost for Oganic
orgcost<-mean(master$Cost_per_100g[which(master$Organic==1)])
nonOrgCost<-mean(master$Cost_per_100g[which(master$Organic==0)])

# Average placement for organic
orgPlace<-mean(master$placement[which(master$Organic==1)])
nonOrgPlace<-mean(master$placement[which(master$Organic==0)])

# Average cost per round
rnd1_avg<-mean(master$Cost_per_100g[which(master$round1==1)])
rnd2_avg<-mean(master$Cost_per_100g[which(master$round2==2)])
rnd3_avg<-mean(master$Cost_per_100g[which(master$round3==3)])
rnd4_avg<-mean(master$Cost_per_100g[which(master$round4==4)])
rnd5_avg<-mean(master$Cost_per_100g[which(master$round5==5)])
rnd6_avg<-mean(master$Cost_per_100g[which(master$round6==6)])

round<-c(1,2,3,4,5,6)
avgcost<-c(rnd1_avg, rnd2_avg, rnd3_avg, rnd4_avg, rnd5_avg, rnd6_avg)
AvgCost.df<-data.frame(round,avgcost)

g13<-ggplot(AvgCost.df, aes(x=round, y=avgcost))+geom_point(size=4)+geom_smooth(method="lm", formula=y~x, se=T)+theme_minimal()+
  xlab("Round")+ylab("Average cost in GBP per 100g")+ theme(text = element_text(size = 16, family = "xkcd"))+scale_x_continuous(breaks=seq(1,6,1))

# Calculating info around fit
fit2<-lm(AvgCost.df$avgcost~AvgCost.df$round) #Cost=3.3774 + 0.2365(Round)
summary(fit2)

# Cost difference between fairtrade and not
master.ft<-master[which(!is.na(master$Fairtrade.Mark)),]

# Round summary for fairtrade
summary(master.ft$placement[which(master.ft$Fairtrade.Mark==1)])
# Round summary for non fairtrade
summary(master.ft$placement[which(master.ft$Fairtrade.Mark==0)])

# Cost summary for fairtrade
summary(master.ft$Cost_per_100g[which(master.ft$Fairtrade.Mark==1)])
# Cost summary for not fairtrade
summary(master.ft$Cost_per_100g[which(master.ft$Fairtrade.Mark==0)])

# Boxplot for fairtrade vs Cost
master.ft$Fairtrade.Mark<-factor(master.ft$Fairtrade.Mark)
g7<-ggplot(data=master.ft, aes(y=Cost_per_100g, x=Fairtrade.Mark, fill=Fairtrade.Mark))+geom_boxplot(show.legend = F)+geom_point()+theme_minimal()+
   theme(text = element_text(size = 16, family = "xkcd"))+ylab("Cost in GBP per 100g")+xlab("Fairtrade mark?")+scale_x_discrete(breaks=c("0","1"), labels=c("No", "Yes"))+
  theme(legend.position = "none")

g8<-ggplot(data=master.ft, aes(y=placement, x=Fairtrade.Mark, fill=Fairtrade.Mark))+geom_boxplot(show.legend = F)+geom_point()+theme_minimal()+
  theme(text = element_text(size = 16, family = "xkcd"))+ylab("Placement")+xlab("Fairtrade mark?")+scale_x_discrete(breaks=c("0","1"), labels=c("No", "Yes"))+
  theme(legend.position = "none")

g2<-ggplot(master.ft, aes(master.ft$Fairtrade.Mark, master.ft$Cost_per_100g, group=2))+geom_point()+geom_boxplot()

master$placement<-factor(master$placement)
customcol<-c("#E74C3C", "#E67E22","#F1C40F", "#2ECC71", "#16A085", "#3498DB")
g10<-ggplot(master, aes(x=Country, y=Cost_per_100g ))+geom_point(aes(color=placement, size=3))+theme_minimal()+ theme(text = element_text(size = 16, family = "xkcd"))+ylab("Cost in GBP per 100g")+
  scale_y_continuous(breaks=seq(1,5,1))+scale_color_grey(start=0.8, end=0.3)#+scale_color_manual(values=customcol) #+scale_color_brewer(palette = "Greens")

# Cost difference between bean type
master.bean<-master[which(!is.na(master$Bean)),]

# Result as a function of bean type
summary(master.bean$placement[which(master.bean$Bean=="Arabica")])
summary(master.bean$placement[which(master.bean$Bean=="Robusta")])
summary(master.bean$placement[which(master.bean$Bean=="Mix")])
summary(master$placement[which(is.na(master$Bean))])

# Cost as a function of bean
summary(master.bean$Cost_per_100g[which(master.bean$Bean=="Arabica")])
summary(master.bean$Cost_per_100g[which(master.bean$Bean=="Robusta")])
summary(master.bean$Cost_per_100g[which(master.bean$Bean=="Mix")])
summary(master$Cost_per_100g[which(is.na(master$Bean))])

g3<-ggplot(master, aes(x=round, fill=Bean))+geom_bar(position="fill")+theme_classic()
g4<-ggplot(master, aes(x=round, fill=Bean))+geom_bar()+ theme(text = element_text(size = 16, family = "xkcd"))


# Archive
# --------------------------------------
# Plots
# cost vs rank, jittered
# plot(y=master$rank_inv, x=master$Cost_per_100g)
# g<-ggplot(master, aes(x=Cost_per_100g, y=rank_inv))+geom_smooth(method="lm")+geom_point()
