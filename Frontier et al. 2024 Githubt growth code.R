anemone <- read.csv("Section 2.6.1. Proportional growth data.csv")
anemonettest <- read.csv("Section 2.6.1. T-test of anemone 2020 BW.csv")
anemonesplit  <- read.csv("Figure 2. Growth metrics.csv")


anemone$Species<-as.factor(anemone$Species)
anemone$Year<-as.factor(anemone$Year)
anemone$Days<-as.numeric(anemone$Days)
anemone$Boxcox<-as.numeric(anemone$Boxcox)
anemonettest$Species<-as.factor(anemonettest$Species)
anemonettest$Year<-as.factor(anemonettest$Year)
anemonettest$Buoyant_Weight_measurement_g<-as.numeric(anemonettest$Buoyant_Weight_measurement_g)

#install.packages
library(car)
library(MASS)
require (nlme)
require(lme4)
require(multcomp)
require(ggplot2)

#homogeneity of variance for t-test
leveneTest(anemonettest$Buoyant_Weight_measurement_g~anemonettest$Species)

#t test for initial differences in BW between the species
#initialBW<-subset(anemonettest, anemone$Year =="2021")
t.test(Buoyant_Weight_measurement_g~Species, paired=FALSE, data = anemonettest)

#significant homogeneity of variance
leveneTest(anemone$Proportion.change~anemone$Year*anemone$Species)

#proportion.change was not significant in git hub file, check if this is transformed data (wirtten may 2024)

#transformed data
b<-boxcox(lm(Update.proportion ~ 1, data=anemone))
lambda <- b$x[which.max(b$y)]
#-1.19

leveneTest(anemone$Boxcox~anemone$Year*anemone$Species)
leveneTest(anemone$Boxcox~anemone$Species)

#linear models
#simplified species model
speciesmodel <- lm(boxcox~ Species, data= anemone)
anova(speciesmodel)

#effect of year, 15 months and 24 months, for Isotealia
isotealia<-subset(anemone, anemone$Species =="Isotealia")
isotealiamodel <- lm(boxcox~ Year, data= isotealia)
anova(isotealiamodel)


#Figure 3, line graph

mytheme<-theme_classic()+theme(axis.title=element_text(size=20),axis.text.y= element_text (size=15),
                          strip.text.x = element_text(size=15, face= "italic"), 
                          legend.position="none")

anemonesplit$Year1<-as.factor(anemonesplit$Year1)
anemonesplit$ID<-as.factor(anemonesplit$ID)
anemonesplit$Species<-as.factor(anemonesplit$Species)
anemonesplit$Month.1<-as.factor(anemonesplit$Month.1)


lineplot<-ggplot(anemonesplit, aes(x = Month.1, y = BW1measurement, colour = ID, group = ID)) +facet_wrap(~Species+Year1, scales="free_x")+
  geom_line() + geom_point(size = 3, shape = 16)+ ylab("BW per individual (g)") +xlab("Time (months)")
lineplot+mytheme

