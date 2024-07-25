#Frontier et al. 2024


respiration <- read.csv("Figure 3. Respiration")
respiration$Season<-as.factor(respiration$Season)

#install.packages
library(lmerTest)
require (nlme)
require(lme4)
require(multcomp)
require(car)
require(ggplot2)

mytheme<-theme_bw()+theme(axis.title=element_text(size=25),axis.text.y= element_text (size=15),
                          strip.text.x = element_text(size=20), axis.text.x = element_text(size=15, face= "italic"), 
                          legend.position="none")

#test for homogeneity
leveneTest(respiration$plot~respiration$Season*respiration$Species)


#plot for Figure 4
anemoneplot1<-ggplot(respiration, aes(x=Species, y=plot, fill=Species))+ylab(expression(paste('O'[2],mu,'mol hr'^-1,'AFDM(g)'))) +xlab("Species")+geom_boxplot()+mytheme +scale_fill_manual(values=c("orange3", "firebrick1")) +
  facet_wrap(~Season, scales="free_x")+geom_text(aes(label = label), vjust = 1.5, colour = "black")
anemoneplot1+mytheme

modnontrans<-lme(plot~Season*Species, random=~1|ID, method="ML", data= respiration)
#type II sums of squares because no significant interaction
Anova(modnontrans)
anova(modnontrans)

#model substitution to determin the importance of the fixed factor
model.fixed=gls(plot~Season*Species, data=respiration, method = "ML")
anova(modnontrans, model.fixed)
#model has a lower AIC value with fixed factor but it is not having a significant effect 
#this is proved by running an anova without the random factor,
#identical model outputs for both models using type II Sums of squares
Anova(model.fixed)


#further confirmed by including ID as a nested factor
mod <- lmer(plot ~ Season*Species + (1 | ID), data=respiration)
summary(mod)
ranova(mod)