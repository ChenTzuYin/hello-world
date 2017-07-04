

#For Taiwanese ERP Rating norming test

library(lme4)
library(ggplot2)

#read csv

rating <- read.csv("2017-03-01RatingMergeFor15.csv")
rating
summary(rating)

rating <-subset(rating, Subject=="9")
#rating <-subset(rating, Subject=="5"|Subject=="6"|Subject=="7")
#rating <-subset(rating, Subject=="701"|Subject=="702"|Subject=="703"|Subject=="704"|Subject=="706")

rating$condition <- ifelse(rating$pattern==1 & rating $accent == 1, "a", 0)
rating$condition <- ifelse(rating$pattern==1 & rating $accent == 2, "b", rating$condition)
rating$condition <- ifelse(rating$pattern==1 & rating $accent == 3, "c", rating$condition)
rating$condition <- ifelse(rating$pattern==1 & rating $accent == 4, "d", rating$condition)
rating$condition <- ifelse(rating$pattern==2 & rating $accent == 1, "e", rating$condition)
rating$condition <- ifelse(rating$pattern==2 & rating $accent == 2, "f", rating$condition)
rating$condition <- ifelse(rating$pattern==2 & rating $accent == 3, "g", rating$condition)
rating$condition <- ifelse(rating$pattern==2 & rating $accent == 4, "h", rating$condition)
rating$condition <- ifelse(rating$pattern==3 & rating $accent == 1, "i", rating$condition)

head(rating,30)

#ggnorm for rating
#rating$Stimulus.RESP <- as.numeric(rating$Stimulus.RESP)
#qqnorm(rating$Stimulus.RESP)
#rating$Stimulus.RESP.log <- log(rating$Stimulus.RESP)
#qqnorm(rating$Stimulus.RESP.log)

#ggnorm for RT
#qqnorm(rating$Stimulus.RT)
#qqline(rating$Stimulus.RT)

#rating$Stimulus.RT.log <- log(rating$Stimulus.RT)
#qqnorm(rating$Stimulus.RT)
#qqline(rating$Stimulus.RT)


#pre-processing

rating.trimmed <- subset(rating,rating$Stimulus.RT<8000)
nrow(rating)-nrow(rating.trimmed)
nrow(rating.trimmed)/nrow(rating)
rating <- rating.trimmed

#norming$structure <-ifelse(norming$C1Type=="-1", 0, 1)
#norming$tone <-ifelse(norming$Condition=="a"|norming$Condition=="b"|norming$Condition=="c"|norming$Condition=="d",0,1)
#norming$violation <-ifelse(norming$NormalOrNot=="Normal",0,1)
#head(norming)
#summary(norming)

#center
#norming$cstructure <- scale(norming$structure)
#norming$ctone <-scale(norming$tone)
#norming$cviolation <-scale(norming$violation)
#summary(norming)
#norming.s <- norming[,c("Subject","ItemNoA","Condition","Nature.RESP","Nature.RT","cstructure","ctone","cviolation")]
#names(norming.s)<-c("Subject","Item","Condition","Rating","RT","cstructure","ctone","cviolation")
#norming.s$Rating <-as.numeric(norming.s$Rating)

#rating mean and sd
rating.mean <- aggregate(rating$Stimulus.RESP, by=list(rating$condition), FUN=mean)
rating.mean
names(rating.mean) <- c("condition","mean")
rating.mean

rating.sd <-aggregate(rating$Stimulus.RESP, by=list(rating$condition), FUN=sd)
rating.sd
names(rating.sd) <- c("condition","sd")
rating.sd
rating.sd$se <- (rating.sd$sd)/sqrt(3)

rating <- cbind(rating.mean, rating.sd$se)
names(rating) <- c("condition","mean","se")
rating

#rating$violation <- ifelse(rating$violation=="Normal", "grammtical", "violated")
rating$group <- ifelse(rating$condition=="a"|rating$condition=="b"|rating$condition=="c"|rating$condition=="d", "exp1",0)
rating$group <- ifelse(rating$condition=="e"|rating$condition=="f"|rating$condition=="g"|rating$condition=="h", "exp1",rating$group)
rating$group <- ifelse(rating$condition=="i", "filler",rating$group)
rating

#plot
rating$mean.round <- round((rating$mean),digit=2)

g = ggplot(rating, aes(x=condition, y=mean,fill=group))
g = g + geom_bar(stat="identity")
g = g + geom_errorbar(aes(ymin = mean-se, ymax = mean + se), width = 0.1, size = 0.5, colour = "black")
g = g + geom_text(aes(label=mean.round), vjust=-2, size = 5, colour="black")
g = g + xlab("Condition")
g = g + ylab("Rating Average(1-6)")
g = g + scale_y_continuous(limits = c(0, 7))
g = g + annotate("text",x=1, y=0.5, label="congruent",angle=90, size=5,hjust=0)
g = g + annotate("text",x=2, y=0.5, label="non-application",angle=90, size=5,hjust=0)
g = g + annotate("text",x=3, y=0.5, label="lexical tone violation",angle=90, size=5,hjust=0)
g = g + annotate("text",x=4, y=0.5, label="non-existent tone",angle=90, size=5,hjust=0)
g = g + annotate("text",x=5, y=0.5, label="congruent",angle=90, size=5,hjust=0)
g = g + annotate("text",x=6, y=0.5, label="overapplication",angle=90, size=5,hjust=0)
g = g + annotate("text",x=7, y=0.5, label="lexical tone violation",angle=90, size=5,hjust=0)
g = g + annotate("text",x=8, y=0.5, label="non-existent tone",angle=90, size=5,hjust=0)
g = g + annotate("text",x=9, y=0.5, label="filler(congruent)",angle=90, size=5,hjust=0)
g = g + ggtitle("Participant No.9")
#g = g + scale_fill_manual(values=c("#FF99FF","#993366"))
g

ppi<-300
png("ParticipantNo09.png",width=9*ppi, height=6*ppi, res=ppi)
g
dev.off()



#RT mean and sd

rating.mean <- aggregate(norming$Nature.RT, by=list(norming$Condition,norming$NormalOrNot), FUN=mean)
rating.mean
names(rating.mean) <- c("condition","violation","mean")
rating.mean

rating.sd <-aggregate(norming$Nature.RT, by=list(norming$Condition,norming$NormalOrNot), FUN=sd)
rating.sd
names(rating.sd) <- c("condition","violation","sd")
rating.sd
rating.sd$se <- (rating.sd$sd)/sqrt(length(unique(norming$Subject)))

rating <- cbind(rating.mean, rating.sd$se)
names(rating) <- c("condition","violation","mean","se")
rating

rating$violation <- ifelse(rating$violation=="Normal", "grammtical", "violated")
rating

#plot
rating$mean.round <- round((rating$mean),digit=0)

g = ggplot(rating, aes(x=condition, y=mean, group = violation,  fill=violation))
g = g + geom_bar(stat="identity")
g = g + geom_errorbar(aes(ymin = mean-se, ymax = mean + se), width = 0.1, size = 0.5, colour = "black")
g = g + geom_text(aes(label=mean.round), vjust=-3.5, size = 5, colour="black")
g = g + xlab("Condition")
g = g + ylab("Reaction Time (ms)")
g = g + scale_y_continuous(limits = c(0, 2000))
g = g + annotate("text",x=1, y=100, label="’|2â¡3",angle=90, size=5,hjust=0)
g = g + annotate("text",x=2, y=100, label="*’|2â¡2",angle=90, size=5,hjust=0)
g = g + annotate("text",x=3, y=100, label="*’|2â¡3“û3‹2",angle=90, size=5,hjust=0)
g = g + annotate("text",x=4, y=100, label="’|2â¡2“û3‹2",angle=90, size=5,hjust=0)
g = g + annotate("text",x=5, y=100, label="1Ô1",angle=90, size=5,hjust=0)
g = g + annotate("text",x=6, y=100, label="*1Ô2",angle=90, size=5,hjust=0)
g = g + annotate("text",x=7.3, y=100, label="1Ô1“û3‹2",angle=90, size=5,hjust=0)
g = g + annotate("text",x=8, y=100, label="*1Ô2“û3‹2",angle=90, size=5,hjust=0)
g = g + scale_fill_manual(values=c("#CCCCFF","#336699"))
g

ppi<-300
png("RT.bar.png",width=9*ppi, height=6*ppi, res=ppi)
g
dev.off()

#
summary(norming.s)

lmer10 <- lmer(Rating ~ cstructure*ctone + (1 + cstructure*ctone|Subject) + (1 + cstructure*ctone|Item), data=subset(norming.s, cviolation<0))
summary(lmer10)

lmer9 <- lmer(Rating ~ cstructure*ctone + (1 + cstructure*ctone|Subject) + (1 + cstructure+ctone|Item), data=subset(norming.s, cviolation<0))
summary(lmer9)

lmer8 <- lmer(Rating ~ cstructure*ctone + (1 + cstructure*ctone|Subject) + (1 + ctone|Item), data=subset(norming.s, cviolation<0))
summary(lmer8)

lmer7 <- lmer(Rating ~ cstructure*ctone + (1 + cstructure*ctone|Subject) + (1 |Item), data=subset(norming.s, cviolation<0))
summary(lmer7)

lmer6 <- lmer(Rating ~ cstructure*ctone + (1 + cstructure+ctone|Subject) + (1 |Item), data=subset(norming.s, cviolation<0))
summary(lmer6)

lmer5 <- lmer(Rating ~ cstructure*ctone + (1 + cstructure|Subject) + (1 |Item), data=subset(norming.s, cviolation<0))
summary(lmer5)

lmer0 <- lmer(Rating ~ cstructure*ctone + (1 |Subject) + (1 |Item), data=subset(norming.s, cviolation<0))
summary(lmer0)

anova(lmer10, lmer9, lmer8, lmer7, lmer6, lmer5, lmer0)
#lmer7
#Fixed effects:
#                 Estimate Std. Error t value
#(Intercept)       5.83473    0.05611  103.99
#cstructure       -0.05995    0.04137   -1.45
#ctone             0.03135    0.03963    0.79
#cstructure:ctone  0.09514    0.04405    2.16



lmer7a <- lmer(Rating ~ cstructure+ctone + (1 + cstructure*ctone|Subject) + (1 |Item), data=subset(norming.s, cviolation<0))
summary(lmer7a)
lmer7b <- lmer(Rating ~ cstructure+cstructure:ctone + (1 + cstructure*ctone|Subject) + (1 |Item), data=subset(norming.s, cviolation<0))
summary(lmer7b)
lmer7c <- lmer(Rating ~ cstructure:ctone+ctone + (1 + cstructure*ctone|Subject) + (1 |Item), data=subset(norming.s, cviolation<0))
summary(lmer7c)

#interaction
anova(lmer7, lmer7a)
#       Df    AIC   BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
#lmer7a 15 463.32 516.5 -216.66   433.32                           
#lmer7  16 460.88 517.6 -214.44   428.88 4.4432      1    0.03504 *

#main effect of tone
anova(lmer7, lmer7b)
#       Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
#lmer7b 15 459.52 512.69 -214.76   429.52                         
#lmer7  16 460.88 517.60 -214.44   428.88 0.6344      1     0.4257

#main effect of structure
anova(lmer7, lmer7c)
#       Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
#lmer7c 15 460.96 514.14 -215.48   430.96                         
#lmer7  16 460.88 517.60 -214.44   428.88 2.0802      1     0.1492


#main effect: tone3 pair
summary(norming.s)
lmer10 <- lmer(Rating ~ cstructure + (1 + cstructure|Subject) + (1 + cstructure|Item), data=subset(norming.s, cviolation<0 & ctone <0))
summary(lmer10)


