#Frontier et al. 2024

# load and view data


#Anemone_bw_ADFM_faeces

#files named according to github upload

faecesweights <- read.csv("Section 2.6.3. T-test anemone bw faecal analysis")


#faeces1day <- read.csv("Supplemental file S7 Faeces per day.csv", header = T)
#faeces7day <- read.csv("Figure 4. Faeces pooled.csv", header = T)

faeces1day$AnemoneID <- as.factor(faeces1day$AnemoneID)
faeces7day$AnemoneID <- as.factor(faeces7day$AnemoneID)
faeces1day$Day<-as.factor(faeces1day$Day)
faeces1day$Season<-as.factor(faeces1day$Season)
faeces7day$Season<-as.factor(faeces7day$Season)
faeces1day$Species<-as.factor(faeces1day$Species)
faeces7day$Species<-as.factor(faeces7day$Species)

faecesweights$Species<-as.factor(faecesweights$Species)
faecesweights$Season<-as.factor(faecesweights$Season)


#install.packages
library(lmerTest)
require (nlme)
require(lme4)
require(multcomp)
require(ggplot2)
require(car)

#Assumptions of normality, Levenes test, n=282
leveneTest(faeces1day$AFDMfaecespergAFDM~faeces1day$Species*faeces1day$Season)


#test for faeces AFDM between season
leveneTest(faecesweights$Anemone_AFDM~faecesweights$Season)

#t-test for difference in AFDM of two independent groups samples
t.test(Anemone_AFDM~Season, paired=FALSE, data = faecesweights)


#linear model for faeces per day data, n=282
model1day <- lm(AFDMfaecespergAFDM~Day+Species*Season, data = faeces1day)
#typeII Sums of squares, no significant interaction
Anova(model1day)


#Assumptions


hist(residuals(model1day))

plot(fitted(model1day), residuals (model1day))


par(mfrow=c(2,2))

plot(model1day)


#Assumption of normality, Levenes test, n=44
leveneTest(faeces7day$AFDMfaecespergAFDM~faeces7day$Species*faeces7day$Season)
model7day <- lm(AFDMfaecespergAFDM~ Species*Season, data= faeces7day)
#typeII Sums of squares, no significant interaction
Anova(model7day)


#Supplementary file S6

mytheme<-theme_bw()+theme(axis.title=element_text(size=15),axis.text.y= element_text (size=15), strip.text.x = element_text(size=15, face= "italic"), axis.text.x = element_text(size=15))
faecestimeplot<-ggplot(faeces1day, aes(x=Day, y= AFDMfaecespergAFDM, fill=Species))+ylab(expression(paste('faecesAFDM(g) per anemoneAFDM(g) day'^-1))) +xlab("Species")+geom_boxplot()+mytheme +scale_fill_manual(values=c("orange3", "firebrick1"))
faecestimeplot+mytheme

#Figure 5, 7 day pooled data
mytheme<-theme_bw()+theme(axis.title=element_text(size=20),axis.text.y= element_text (size=10), strip.text.x = element_text(size=20), axis.text.x = element_text(size=15, face= "italic"), legend.position="none")
anemoneplot<-ggplot(faeces7day, aes(x=Species, y=AFDMfaecespergAFDM, fill=Species))+ylab(expression(paste('Faeces AFDM(g) 7 days'))) +xlab("Species")+geom_boxplot()+mytheme +scale_fill_manual(values=c("orange3", "firebrick1")) +
  facet_wrap(~Season, scales="free_x")+geom_text(aes(label = Label), vjust = 1.5, colour = "black")
anemoneplot+mytheme

