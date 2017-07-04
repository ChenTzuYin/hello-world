# L2 file modified for Saki's data

# Preliminary stats. For now, I'm not worrying about accuracy (but note that at least one Buffalo participant should be thrown out, and maybe more from both groups, depending on cutoff decisions)

# Modified 8/17/2015 when checking data from reanalysis using RoboDoc. Note that the subject numbers may be different from the original data set (need to compare the list files to make sure).
# Modified 3/4/2015 to perform analyses on alternate version of L1 eyetracking data where all of RC (that + NP + verb) is treated as a single region (Edson Miyamoto's idea to see if there is a "reversal")
# Modified 8/21/2014 to switch data to long format and to avoid all of the missing values being treated as 0 ms reading times (we should be dropping the words that were not fixated on, or had fixation times less than 80 ms, not treating them as having 0 ms reading times by accident)

library(lme4)
library(languageR)  # for xylowess.fnc()
library(coefplot2)  # install.packages("coefplot2", repos="http://www.math.mcmaster.ca/bolker/R", type="source")  # also, make sure "coda" is already installed
library(ggplot2)
library(car) # for Anova()
library(effects) # for an alternate way of plotting CIs
library(pbkrtest) # used for parametric bootstraping (one way to get p values)
library(RePsychLing)
library(plyr) # for round_any
#library(lmerTest) # 
# Try to use the RePsychLing package to pick appropriate random effects structures...
# install.packages("devtools")
# library(devtools)
# devtools::install_github("dmbates/RePsychLing")


c. <- function (x) scale(as.numeric(x), scale = FALSE)  # version of centering for categorical variables with only two categories

# Try to read in a sample cleaned and dried file

# RoboDoc data
	
	data<-read.csv("Tsumura_firstfix.ixs"); MeasureName <- "L1 First Fixation"   # no zeros
	#data<-read.csv("Tsumura_firstpassregout.ixs"); MeasureName <- "L1 p(Regression)"  

# rename columns to match the names used in the existing code

	names(data) <- c("sequence", "subject", "item", "condition", "region", "RT")

	# Don't do this for p(regression), p(fixation) data, number of fixations
	# note - this also removes all of the NAs
	data <- subset(data,RT>1) # need to filter out the zeros (all RTs should be greater than 80ms - the zeros in the Tokyo data are non-fixated regions that should have been filtered out in eyedry

	summary(data) # Data prepared using long format (one data point per row)


# create columns for RC type and NP type

	data$condition[data$condition == 1] <- "comma"
	data$condition[data$condition == 2] <- "nocomma"
	data$condition <- factor(data$condition)


# convert region numbers to region names

	data$region[data$region==1] <- "While"
	data$region[data$region==2] <- "thefather"
	data$region[data$region==3] <- "walks"
	data$region[data$region==4] <- "thedog"
	data$region[data$region==5] <- "with"
	data$region[data$region==6] <- "darkbrownfur"
	data$region[data$region==7] <- "follows"
	data$region[data$region==8] <- "himathisside"

	data$region <- factor(data$region, levels = c("While", "thefather", "walks", "thedog", "with", "darkbrownfur", "follows", "himathisside"))

# factor various columns

	data$subject <- factor(data$subject)
	data$item <- factor(data$item)

# remove participants with low accuracy scores 
# 3.1.2016 - decided to use use 70% (or continue to use 70%) due to this seeming to be the most appropriate number for the Buffalo eye-movement data
# See excel sheet, for Tokyo 42 data, the participants below 70% are:
# 1, 14, 17, and 42. (42 also removed for blinks)

	# participants to drop from Tokyo42 data
#	data <- subset(data,subject !="1")
#	data <- subset(data,subject !="14")
#	data <- subset(data,subject !="17")
#	data <- subset(data,subject !="42")	
#	length(unique(data$subject))	 # 20 participants left for Tokyo42 data - so we only lost 2 extra with RoboDoc as compared to the previous version
	
	# Participants to drop from buffalo data (these numbers stay the same in the RoboDoc redo, since the only changes in participant number were higher in the list
#	data <- subset(data,subject !="19")
#	data <- subset(data,subject !="36")
#	data <- subset(data,subject !="40") # These three participants have accuracy rates below 70%. Participant 40's low score was somehow overlooked in earlier versions of the analysis.

	data$subject <- factor(data$subject) # this removes extra levels from deleted participants
	length(unique(data$subject))	 # 33 participants before removing anyone for low amounts of data)
	 



	 
# remove subjects who are missing too many items (or not - doing this in this way has the property of removing different numbers of participants from 2nd pass vs. other measures, since if participants read with no regressions, they have no 2nd pass times for an itme)

#	length(unique(data$subject)) #70 for buffalo	
#	length(unique(data$item))	 #20 
#	table(data[,c("subject","item")])
#	subjectcounts<-aggregate(item~subject,data=data,FUN=function (x) length(unique(x)))
#	colnames(subjectcounts)<- c("subject", "numberOfValiditems")
#	subjectcounts$percentvaliditems<-subjectcounts$numberOfValiditems/20
#	subjectcounts
#	subjectcounts[order(subjectcounts$numberOfValiditems),]
#	nrow(subjectcounts)
#	nrow(subset(subjectcounts,percentvaliditems>=.75)) # keep 22/42 (listed as 22/40 since #7 and #10 had all trials rejected within RoboDoc) participants at 75% (55/70)
#
#	nrow(data)  #626 - this is a sanity check to make sure we're not multiplying the table (6142 for Buffalo)
#	data<-merge(data,subjectcounts)
#	nrow(data)  # still produces 626, so we're ok
#
#	data <- subset(data,percentvaliditems>=.75)	
#	head(data)
#	data$subject <- factor(data$subject)
#	length(unique(data$subject))	
#	sort(unique(data$subject))	
	
# Try some quick lmers for each region using Baayen's technique for outlier removal	

	# Choose appropriate region...
	subdata <- subset(data,region == "While"); RegionName <- "While"
	subdata <- subset(data,region == "thefather"); RegionName <- "the father"
	subdata <- subset(data,region == "walks"); RegionName <- "walks"
	subdata <- subset(data,region == "thedog"); RegionName <- "the dog"
	subdata <- subset(data,region == "with"); RegionName <- "with"
	subdata <- subset(data,region == "darkbrownfur"); RegionName <- "dark brown fur"
	subdata <- subset(data,region == "follows"); RegionName <- "follows"
	subdata <- subset(data,region == "himathisside"); RegionName <- "him at his side"

	hist(subdata$RT,breaks=1000)
    boxplot(subdata$RT)
    boxplot(subdata$RT, ylim = c(0,5000))
    hist(subdata$RT)
    hist(subdata$RT, xlim = c(0,2000), breaks = 1000)
    plot(density(subdata$RT))
    plot(density(subdata$RT), xlim = c(0,2000))
    qqnorm(subdata$RT)
    qqline(subdata$RT)

    DudCutoffHigh <- 600  #Remove all datapoints with longer RTs
    DudCutoffLow <- 100    #Remove all datapoints with shorter RTs    
  
    nrow(subset(subdata,subdata$RT>DudCutoffHigh|subdata$RT<DudCutoffLow))/nrow(subdata)  # but note that this includes RTs from fillers and items, so the next three lines produce the numbers for potential data points (not that we neccesarily will report results for all regions)
    
# Actually filter out the outlier data (37/8040 datapoints from numbered regions)

	subdata <- subset(subdata, subdata $RT<DudCutoffHigh)
	subdata <- subset(subdata, subdata $RT> DudCutoffLow)


   #options(scipen=999) # 999 to prevent scientific notation, 0 to allow.

	# Run full model first, and use rePCA to decide what the most appropriate set of random effects is
	rc.lmer<-(lmer(RT~c.(condition)+(1+c.(condition)||subject)+(1+c.(condition)||item), data = subdata))
	rc.lmer.trimmed<-(lmer(RT~c.(condition)+(1+c.(condition)||subject)+(1+c.(condition)||item), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))
	summary(rc.lmer.trimmed)
	summary(rePCA(rc.lmer.trimmed))

	rc.lmer<-(lmer(RT~c.(condition)+(1+c.(condition)|subject)+(1+c.(condition)|item), data = subdata))
	rc.lmer.trimmed1<-(lmer(RT~c.(condition)+(1+c.(condition)|subject)+(1+c.(condition)|item), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))
	rc.lmer.trimmed2<-(lmer(RT~c.(condition)+(1+c.(condition)||subject)+(1+c.(condition)||item), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))
	rc.lmer.trimmed3<-(lmer(RT~c.(condition)+(1|subject)+(1|item), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))

	summary(rc.lmer.trimmed1)
	summary(rePCA(rc.lmer.trimmed1))

	summary(rc.lmer.trimmed2)
	summary(rePCA(rc.lmer.trimmed2))

	summary(rc.lmer.trimmed3)
	summary(rePCA(rc.lmer.trimmed3))

anova(rc.lmer.trimmed1,rc.lmer.trimmed2,rc.lmer.trimmed3)


	par(mfrow=c(2,2))
	plot(fitted(rc.lmer.trimmed),residuals(rc.lmer.trimmed))
	qqnorm(residuals(rc.lmer.trimmed))
	qqline(residuals(rc.lmer.trimmed))	
	plot(fitted(rc.lmer),residuals(rc.lmer))
	qqnorm(residuals(rc.lmer))
	qqline(residuals(rc.lmer))	
	

	
	newdat <- expand.grid(
			condition=c("comma","nocomma") # make sure these are in the right order!
			, RT = 0
		)
		mm = model.matrix(terms(rc.lmer.trimmed),newdat)
		newdat$RT = mm %*% fixef(rc.lmer.trimmed)
		pvar1 <- diag(mm %*% tcrossprod(vcov(rc.lmer.trimmed),mm))  # these are the confidence intervals (range where mean is expected)
		tvar1 <- pvar1+VarCorr(rc.lmer.trimmed)$subject[1]          # these are for prediction intervals (range where next data point is expected to fall) - not clear that this is exactly correct, since we have subject and item variance
		newdat <- data.frame(
			 newdat
			 , plo = newdat$RT-2*sqrt(pvar1)  
			 , phi = newdat$RT+2*sqrt(pvar1)
			 , tlo = newdat$RT-2*sqrt(tvar1)  
			 , thi = newdat$RT+2*sqrt(tvar1)
		)
		confidence_interval_data <- newdat #rename for output clarity

		newdat

	# Summarize results (no indents to faciliate copy + paste)

trim_summary <- paste(nrow(subdata)-nrow(subset(subdata, abs(scale(resid(rc.lmer)))<2.5))," out of ",nrow(subdata), " data points (",format((nrow(subdata)-nrow(subset(subdata, abs(scale(resid(rc.lmer)))<2.5)))/nrow(subdata)*100,digits=2,nsmall=1),"%)"," were removed for region ", RegionName, sep="")
when <- paste("Results for", MeasureName, RegionName, Sys.Date())
cat("Results for", MeasureName, RegionName)
summary(rc.lmer.trimmed)
trim_summary
confidence_interval_data;when
Anova(rc.lmer.trimmed)


		# Graph, if we care to see
		# calculate y limits (or specify below to keep constant across graphs)
		yminimum <- round_any(min(newdat$plo),50,floor)
		ymaximum <- round_any(max(newdat$phi),50,ceiling)
		limits <- aes(ymax = phi, ymin=plo)
		p <- ggplot(newdat, aes(fill=condition, y=RT, x=condition))
		dodge <- position_dodge(width=0.9)
		p <- p + geom_bar(position=dodge, stat="identity")
		p <- p + theme_bw(base_size = 12, base_family = "")
		p <- p + scale_x_discrete(breaks=c("comma", "nocomma"),name="Condition" ,labels=c("Comma", "No Comma"))
		p <- p + coord_cartesian(ylim = c(yminimum, ymaximum)) + scale_y_continuous(breaks=seq(yminimum,ymaximum,100)) 
		#p <- p + coord_cartesian(ylim = c(250, 450)) + scale_y_continuous(breaks=seq(250,450,50))# 700 ms seems to be the max for anything I need for the poster
		p <- p + scale_fill_grey(name="Condition", labels=c("Comma","No Comma"))  
		p <- p + geom_errorbar(limits, position=dodge, width=0.25) 
		p <- p + ylab("First Fixation Duration (ms)")
		#p <- p + ylab("First Fixation Duration (ms)")
 		#p <- p + scale_shape_discrete(name="Relative Clause Type", labels=c("Object Relative","subject Relative")) # since RC determines both color and shape, we have to do any renaming 2x to avoid a double legend
		p <- p + theme(legend.position=c(.5, .85), legend.key = element_blank())
		p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
		p 


	# example of stepwise model simplification tested using Buffalo Go-past rcverb (i.e., for order of reduction) - it's not obvious that doing outlier removal based on the complete model is correct (or incorrect)
	rc.lmer.trimmed<-(lmer(RT~c.(RC)*c.(NP)+(1+c.(RC)*c.(NP)|subject)+(1+c.(RC)*c.(NP)|item), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))
	summary(rc.lmer.trimmed)
	summary(rePCA(rc.lmer.trimmed))

	rc.lmer.trimmed1<-(lmer(RT~c.(RC)*c.(NP)+(1+c.(RC)+c.(NP)|subject)+(1+c.(RC)*c.(NP)|item), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))
	summary(rc.lmer.trimmed1)
	summary(rePCA(rc.lmer.trimmed1))
	anova(rc.lmer.trimmed,rc.lmer.trimmed1)	

	rc.lmer.trimmed2<-(lmer(RT~c.(RC)*c.(NP)+(1+c.(RC)+c.(NP)|subject)+(1+c.(RC)+c.(RC):c.(NP)|item), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))
	summary(rc.lmer.trimmed2)
	summary(rePCA(rc.lmer.trimmed2))	
	anova(rc.lmer.trimmed,rc.lmer.trimmed1,rc.lmer.trimmed2)	

	rc.lmer.trimmed3<-(lmer(RT~c.(RC)*c.(NP)+(1+c.(RC)+c.(NP)|subject)+(1+c.(RC):c.(NP)|item), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))
	summary(rc.lmer.trimmed3)
	summary(rePCA(rc.lmer.trimmed3))	
	anova(rc.lmer.trimmed,rc.lmer.trimmed1,rc.lmer.trimmed2,rc.lmer.trimmed3)	

	rc.lmer.trimmed4<-(lmer(RT~c.(RC)*c.(NP)+(1+c.(RC)|subject)+(1+c.(RC):c.(NP)|item), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))
	summary(rc.lmer.trimmed4)
	summary(rePCA(rc.lmer.trimmed4))	
	anova(rc.lmer.trimmed,rc.lmer.trimmed1,rc.lmer.trimmed2,rc.lmer.trimmed3,rc.lmer.trimmed4)	

	rc.lmer.trimmed5<-(lmer(RT~c.(RC)*c.(NP)+(1|subject)+(1+c.(RC):c.(NP)|item), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))
	summary(rc.lmer.trimmed5)
	summary(rePCA(rc.lmer.trimmed5))	
	anova(rc.lmer.trimmed,rc.lmer.trimmed1,rc.lmer.trimmed2,rc.lmer.trimmed3,rc.lmer.trimmed4,rc.lmer.trimmed5)	

	rc.lmer.trimmed6<-(lmer(RT~c.(RC)*c.(NP)+(1|subject)+(1|item), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))
	summary(rc.lmer.trimmed6)
	summary(rePCA(rc.lmer.trimmed6))	
	anova(rc.lmer.trimmed,rc.lmer.trimmed1,rc.lmer.trimmed2,rc.lmer.trimmed3,rc.lmer.trimmed4,rc.lmer.trimmed5,rc.lmer.trimmed6)	


	summary(rc.lmer.trimmed)
	Anova(rc.lmer.trimmed)
	library(lmerTest)
	#rerun the model using the version of lmer from lmerTest
	rc.lmer.trimmedTEST<-(lmer(RT~c.(RC)*c.(NP)+(1+c.(RC)*c.(NP)|subject)+(1+c.(RC)*c.(NP)|item), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))
	rc.lmer.trimmedTESTnc<-(lmer(RT~RC*NP+(1+RC*NP|subject)+(1+RC*NP|item), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))

	summary(rc.lmer.trimmedTEST)
	anova(rc.lmer.trimmedTEST)
	anova(rc.lmer.trimmedTEST,ddf = "Kenward-Roger") # takes a long time to run
	anova(rc.lmer.trimmedTEST,ddf = "lme4")
	difflsmeans(rc.lmer.trimmedTESTnc) # crashes on model with centered predictors, but I suppose this is ok if you care about all of the sub tests, and don't want to manually shift all of the intercepts around. This really just tells you the same thing as a graph with 95% CIs
	difflsmeans(rc.lmer.trimmedTESTnc,test.effs="RC:NP")
	lsmeans(rc.lmer.trimmedTESTnc)
	detach(lmerTest) # so that other models are run using regular lmer





	# different optimizers, just in case it helps....
	rc.lmer<-(lmer(RT~c.(RC)*c.(NP)+(1+c.(RC)+c.(NP)|item)+(1+c.(RC)+c.(NP)|subject), control=lmerControl(optimizer = "bobyqa"), data = subdata))
	rc.lmer.trimmed<-(lmer(RT~c.(RC)*c.(NP)+(1+c.(RC)+c.(NP)|item)+(1+c.(RC)+c.(NP)|subject), control=lmerControl(optimizer = "bobyqa"), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))
	
	rc.lmer<-(lmer(RT~c.(RC)*c.(NP)+(1+c.(RC)+c.(NP)|item)+(1+c.(RC)+c.(NP)|subject), control=lmerControl(optimizer = "Nelder_Mead"), data = subdata))
	rc.lmer.trimmed<-(lmer(RT~c.(RC)*c.(NP)+(1+c.(RC)+c.(NP)|item)+(1+c.(RC)+c.(NP)|subject), control=lmerControl(optimizer = "Nelder_Mead"), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))



# Test whether verb type matters

	rc.lmer<-(lmer(RT~c.(RC)*c.(NP)*VerbType+(1+c.(RC)+c.(NP)|item)+(1+c.(RC)+c.(NP)|subject), data = subdata))
	rc.lmer.trimmed<-(lmer(RT~c.(RC)*c.(NP)*VerbType+(1+c.(RC)+c.(NP)|item)+(1+c.(RC)+c.(NP)|subject), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))
	summary(rc.lmer.trimmed)
	Anova(rc.lmer.trimmed)

	rc.lmer<-(lmer(RT~c.(RC)*c.(NP)*RCverbSyllableCount+(1+c.(RC)+c.(NP)|item)+(1+c.(RC)+c.(NP)|subject), data = subdata))
	rc.lmer.trimmed<-(lmer(RT~c.(RC)*c.(NP)*RCverbSyllableCount+(1+c.(RC)+c.(NP)|item)+(1+c.(RC)+c.(NP)|subject), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))
	summary(rc.lmer.trimmed)
	Anova(rc.lmer.trimmed)


	# Choose appropriate region...
	prosubdata <- subset(data,region == "mainsubj"&NP=="Pro")
	prosubdata <- subset(data,region == "that"&NP=="Pro")
	prosubdata <- subset(data,region == "rcnp"&NP=="Pro")
	prosubdata <- subset(data,region == "rcverb"&NP=="Pro")
	prosubdata <- subset(data,region == "mcverb"&NP=="Pro")  # interaction between verb type and RC type (only) at mc verb (total RT)

	summary(prosubdata)


	rc.lmer<-(lmer(RT~RC*VerbType+(1+RC+VerbType|item)+(1+RC+VerbType|subject), data = prosubdata))
	rc.lmer.trimmed<-(lmer(RT~RC*VerbType+(1+RC+VerbType|item)+(1+RC+VerbType|subject), data = prosubdata, subset = abs(scale(resid(rc.lmer)))<2.5))
	summary(rc.lmer.trimmed)
	Anova(rc.lmer.trimmed)
	summary(rePCA(rc.lmer.trimmed))


	rc.lmer<-(lmer(RT~RC*VerbType+(1|item)+(1|subject), data = prosubdata))
	rc.lmer.trimmed<-(lmer(RT~RC*VerbType+(1|item)+(1|subject), data = prosubdata, subset = abs(scale(resid(rc.lmer)))<2.5))



# Test whether syllable count matters (total RT)
	# Mainsubj: Effect of syllable count and interaction between syllable count and RC type
	# That: ns
	# RC NP: effect of RC type
	# RC Verb: Effect of syllable count (not surprising!)
	# MC verb: Effect of syllable count

	rc.lmer<-(lmer(RT~RC*RCverbSyllableCount+(1+RC+RCverbSyllableCount|item)+(1+RC+RCverbSyllableCount|subject), data = prosubdata))
	rc.lmer.trimmed<-(lmer(RT~RC*RCverbSyllableCount+(1+RC+RCverbSyllableCount|item)+(1+RC+RCverbSyllableCount|subject), data = prosubdata, subset = abs(scale(resid(rc.lmer)))<2.5))
	summary(rc.lmer.trimmed)
	Anova(rc.lmer.trimmed)

	summary(rePCA(rc.lmer.trimmed))


# Look to see which items seem to have the biggest reversal for pronominal RCs (do this using total reading time or total number of fixations)

	minidata <- subset(data,(NP=="Pro")&((region == "mainsubj")|(region == "that")|(region == "rcnp")))
	summary(minidata)
aggregate(RT~item+condition, data=minidata,FUN=mean)



# Looking at models using rePCA (note that these are all with un-trimmed data....)

	testmodel.max<-(lmer(RT~RC*NP+(1+RC+NP|item)+(1+RC+NP|subject), data = subdata,REML=FALSE))
	testmodel.1<-(lmer(RT~RC*NP+(1+RC|item)+(1+RC|subject), data = subdata,REML=FALSE))
	testmodel.2<-(lmer(RT~RC*NP+(1|item)+(1|subject), data = subdata,REML=FALSE))


	summary(testmodel.max)
	summary(rePCA(testmodel.max))
	summary(testmodel.1)
	summary(rePCA(testmodel.1))
	summary(testmodel.2)
	summary(rePCA(testmodel.2))
anova(testmodel.max,testmodel.1,testmodel.2)








	subdata.full <- subset(subdata,NP == "Full")
	subdata.pro <- subset(subdata, NP == "Pro")

	hist(subdata$RT,breaks = 100, col = "blue",xlim = c(0,1000))
	qqnorm(subdata$RT)
    qqline(subdata$RT)
 	summary(subdata$RT)

	rc.lmer<-(lmer(RT~c.(RC)*c.(NP)+(1+c.(RC)+c.(NP)|item)+(1+c.(RC)+c.(NP)|subject), data = subdata))
	rc.lmer.trimmed<-(lmer(RT~c.(RC)*c.(NP)+(1+c.(RC)+c.(NP)|item)+(1+c.(RC)+c.(NP)|subject), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))
	rc.lmer.trimmed.min<-(lmer(RT~c.(RC)*c.(NP)+(1|item)+(1|subject), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))
	rc.lmer.trimmed.max<-(lmer(RT~c.(RC)*c.(NP)+(1+c.(RC)*c.(NP)|item)+(1+c.(RC)*c.(NP)|subject), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))
	anova(rc.lmer.trimmed,rc.lmer.trimmed.max,rc.lmer.trimmed.min)
	
	fixef(rc.lmer.trimmed.max)
	fixef(rc.lmer.trimmed)
	fixef(rc.lmer.trimmed.min)

	summary(rc.lmer.trimmed.max)
	summary(rc.lmer.trimmed)
	summary(rc.lmer.trimmed.min)
	
	par(mfrow=c(2,2))
	plot(fitted(rc.lmer),residuals(rc.lmer))
	qqnorm(residuals(rc.lmer))
	qqline(residuals(rc.lmer))	

	plot(fitted(rc.lmer.trimmed),residuals(rc.lmer.trimmed))
	qqnorm(residuals(rc.lmer.trimmed))
	qqline(residuals(rc.lmer.trimmed))	

	#Anova(rc.lmer.trimmed)
   	#paste(nrow(data)-nrow(subset(data, subset = abs(scale(resid(rc.lmer)))<2.5)),"data points (",format(nrow(subset(data, subset = abs(scale(resid(rc.lmer)))>=2.5))/nrow(data)*100,digits=2,nsmall=1),"%)","were removed")
	#drawgraph(rc.lmer.trimmed)

		rc.lmer.m<-(lmer(RT~c.(RC)*c.(NP)+(1+c.(RC)*c.(NP)|item)+(1+c.(RC)*c.(NP)|subject), data = subdata))
		rc.lmer.trimmed.m<-(lmer(RT~c.(RC)*c.(NP)+(1+c.(RC)*c.(NP)|item)+(1+c.(RC)*c.(NP)|subject), data = subdata, subset = abs(scale(resid(rc.lmer.m)))<2.5))
		summary(rc.lmer.trimmed.m)
	
		rc.lmer.0<-(lmer(RT~c.(RC)*c.(NP)+(1|item)+(1|subject), data = subdata))
		rc.lmer.trimmed.0<-(lmer(RT~c.(RC)*c.(NP)+(1|item)+(1|subject), data = subdata, subset = abs(scale(resid(rc.lmer.0)))<2.5))
		summary(rc.lmer.trimmed.0)
	
		rc.lmer.trimmed.1<-(lmer(RT~c.(RC)*c.(NP)+(1|item)+(0+c.(RC)+c.(NP)|item)+(1|subject)+(0+c.(RC)+c.(NP)|subject), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5,control=lmerControl(optCtrl=list(maxfun=20000) )))
		summary(rc.lmer.trimmed.1)
		anova(rc.lmer.trimmed,rc.lmer.trimmed.1)
	
		rc.lmer.trimmed.2<-(lmer(RT~c.(RC)*c.(NP)+(1|item)+(0+c.(RC)|item)+(1|subject)+(0+c.(RC)|subject), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5,control=lmerControl(optCtrl=list(maxfun=20000) )))
		summary(rc.lmer.trimmed.2)
		anova(rc.lmer.trimmed,rc.lmer.trimmed.1,rc.lmer.trimmed.2)
	
		rc.lmer.trimmed.3<-(lmer(RT~c.(RC)*c.(NP)+(1|item)+(1|subject), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5,control=lmerControl(optCtrl=list(maxfun=20000) )))
		summary(rc.lmer.trimmed.3)	
	

	rc.lmer.full<-(lmer(RT~c.(RC)+(1+c.(RC)|item)+(1+c.(RC)|subject), data = subdata.full))
	rc.lmer.full.trimmed<-(lmer(RT~c.(RC)+(1+c.(RC)|item)+(1+c.(RC)|subject), data = subdata.full, subset = abs(scale(resid(rc.lmer.full)))<2.5))
	rc.lmer.full.trimmed.min<-(lmer(RT~c.(RC)+(1|item)+(1|subject), data = subdata.full, subset = abs(scale(resid(rc.lmer.full)))<2.5))
	anova(rc.lmer.full.trimmed,rc.lmer.full.trimmed.min)

	summary(rc.lmer.full.trimmed.min)
	summary(rc.lmer.full.trimmed)
	
	rc.lmer.pro<-(lmer(RT ~c.(RC)+(1+c.(RC)|item)+(1+c.(RC)|subject), data = subdata.pro))
	rc.lmer.pro.trimmed<-(lmer(RT ~c.(RC)+(1+c.(RC)|item)+(1+c.(RC)|subject), data = subdata.pro, subset = abs(scale(resid(rc.lmer.pro)))<2.5))
	rc.lmer.pro.trimmed.min<-(lmer(RT ~c.(RC)+(1|item)+(1|subject), data = subdata.pro, subset = abs(scale(resid(rc.lmer.pro)))<2.5))
	anova(rc.lmer.pro.trimmed,rc.lmer.pro.trimmed.min)

	summary(rc.lmer.pro.trimmed.min)
	summary(rc.lmer.pro.trimmed)
	
		summary(M2c<-(lmer(RT~c.(RC)+(1+c.(RC)|item)+(1+c.(RC)|subject), data = subdata.pro)))
		summary(M2<-(lmer(RT~RC+(1+RC|item)+(1+RC|subject), data = subdata.pro)))
		summary(M1<-(lmer(RT~RC+(1|item)+(0+RC|item)+(1|subject)+(0+RC|subject), data = subdata.pro)))
		summary(M0<-(lmer(RT~RC+(1|item)+(1|subject), data = subdata.pro)))
		anova(M0,M1,M2)

		summary(lmer(RT~RC+(1+RC|item)+(1+RC|subject), data = subdata.pro,control=lmerControl(optimizer = "bobyqa", restart_edge = TRUE)))
		summary(lmer(RT~RC+(1+RC|item)+(1+RC|subject), data = subdata.pro,control=lmerControl(optimizer = "Nelder_Mead", restart_edge = TRUE)))
	
	# graph for RTs	(RC x NP)
	
		newdat <- expand.grid(
			RC=c("OR","SR") # make sure these are in the right order!
			, NP=c("Full","Pro")
			, RT = 0
		)
		mm = model.matrix(terms(rc.lmer.trimmed),newdat)
		newdat$RT = mm %*% fixef(rc.lmer.trimmed)
		pvar1 <- diag(mm %*% tcrossprod(vcov(rc.lmer.trimmed),mm))  # these are the confidence intervals (range where mean is expected)
		tvar1 <- pvar1+VarCorr(rc.lmer.trimmed)$subject[1]          # these are for prediction intervals (range where next data point is expected to fall) - not clear that this is exactly correct, since we have subject and item variance
		newdat <- data.frame(
			 newdat
			 , plo = newdat$RT-2*sqrt(pvar1)  
			 , phi = newdat$RT+2*sqrt(pvar1)
			 , tlo = newdat$RT-2*sqrt(tvar1)  
			 , thi = newdat$RT+2*sqrt(tvar1)
		)
		print(newdat) # data for confidence intervals (phi and plo)
	
		# Better looking version of plotting confidence intervals
		limits <- aes(ymax = phi, ymin=plo)
		p <- ggplot(newdat, aes(fill=RC, y=RT, x=NP))
		dodge <- position_dodge(width=0.9)
		p <- p + geom_bar(position=dodge, stat="identity")
		p <- p + theme_bw(base_size = 12, base_family = "")
		p <- p + scale_x_discrete(breaks=c("Full", "Pro"),name="Context Type" ,labels=c("Full NP", "Pronominal NP"))
		p <- p + coord_cartesian(ylim = c(250, 450)) + scale_y_continuous(breaks=seq(250,450,50))# 700 ms seems to be the max for anything I need for the poster
		p <- p + scale_fill_grey(name="Relative Clause Type", labels=c("Object Relative","subject Relative"))  
		p <- p + geom_errorbar(limits, position=dodge, width=0.25) 
		p <- p + ylab("First Fixation Duration (ms)")
		#p <- p + ylab("First Fixation Duration (ms)")
 		p <- p + scale_shape_discrete(name="Relative Clause Type", labels=c("Object Relative","subject Relative")) # since RC determines both color and shape, we have to do any renaming 2x to avoid a double legend
		p <- p + theme(legend.position=c(.5, .85), legend.key = element_blank())
		p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
		p 
		
		MyWidth <- 5.5 # width in inches
		pdf("Figure1n.pdf",MyWidth,MyWidth*.618) # for now, a 7" wide graph, with a 1:.618 ratio. Since it's a PDF, (and done with vector graphics), we can resize the graph in the document. But, the size we choose here will affect the font size
		p
		dev.off()

################################################################

# This isn't really the normal way to analyze the data - I just wanted to get graphs of eyetracking reading times to compare with the self-paced data.


     subdata <- subset(data, (LinearRegion=="mainsubj"|LinearRegion =="that"|LinearRegion=="rc1"|LinearRegion=="rc2"|LinearRegion=="mcverb"))
     rc.lmer        <-(lmer(RT~c.(RC)*c.(NP)*LinearRegion+(1|item)+(1|subject), data = subdata)) # model simplified due to convergence issues
     rc.lmer.trimmed<-(lmer(RT~c.(RC)*c.(NP)*LinearRegion+(1|item)+(1|subject), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))
     summary(rc.lmer.trimmed)
     Anova(rc.lmer.trimmed)
     
         newdat <- expand.grid(
			RC=c("OR","SR")  # can also use levels(subdata$RC), etc.
			, NP=c("Full","Pro")  
			, LinearRegion=c("mainsubj", "that", "rc1", "rc2", "mcverb")  # note that these need to be in the same order as the internal order of the factor set by levels()
			, RT = 0
		)
		mm = model.matrix(terms(rc.lmer.trimmed),newdat)
		newdat$RT = mm %*% fixef(rc.lmer.trimmed)
		pvar1 <- diag(mm %*% tcrossprod(vcov(rc.lmer.trimmed),mm))  # these are the confidence intervals (range where mean is expected)
		tvar1 <- pvar1+VarCorr(rc.lmer.trimmed)$subject[1]          # these are for prediction intervals (range where next data point is expected to fall) - not clear that this is exactly correct, since we have subject and item variance
		newdat <- data.frame(
			 newdat
			 , plo = newdat$RT-2*sqrt(pvar1)
			 , phi = newdat$RT+2*sqrt(pvar1)
			 , tlo = newdat$RT-2*sqrt(tvar1)  
			 , thi = newdat$RT+2*sqrt(tvar1)
		)

		newdat $RCNP <- factor(paste(newdat$RC, newdat$NP, sep='')) # use this to make graph with single legend

		newdat # data for confidence intervals (phi and plo)
	
		# Attempt at plotting reading times and CIs for all regions. Need to try to find a way to get multiple example sentences in different colors. This seems difficult in ggplot - but there may be a way to fake it with multiple plots 
		# The CIs here are probabably wrong, since this assumes equal variance across regions, which is typically not the case (and is clearly not the case in this data set)
		# It would be better to get CIs from separate models for each region
		# See Masson and Loftus for discussion of CIs
		
		# This version seems to be what I was trying to get....
		
		# calculate y limits (or specify below to keep constant across graphs)
		yminimum <- round_any(min(newdat$plo),50,floor)
		ymaximum <- round_any(max(newdat$phi),50,ceiling)



		g0 <- ggplot(newdat, aes(x = LinearRegion, y = RT, colour = RCNP, shape = RCNP,group = RCNP))
		g0 <- g0 + theme_bw(base_size = 12, base_family = "")
		g0 <- g0 + coord_cartesian(ylim = c(yminimum, ymaximum)) + scale_y_continuous(breaks=seq(yminimum,ymaximum,100)) 
		#g0 <- g0 + coord_cartesian(ylim = c(150, 550)) + scale_y_continuous(breaks=seq(150,550,100)) 
		g0 <- g0 + geom_point(size=2) + geom_line(aes(linetype=RCNP))
		g0 <- g0 + scale_x_discrete(breaks=c("mainsubj", "that", "rc1", "rc2", "mcverb"), labels=c("The producer", "that", "the actress\nliked\nliked\nshe", "liked\nthe actress\nher\nliked", "sent"))
		g0 <- g0 + scale_color_manual(name="Condition",limits=c("ORFull","SRFull","ORPro","SRPro"),breaks=c("ORFull","SRFull","ORPro","SRPro"),labels=c("Full NP Object Relative","Full NP subject Relative","Pronominal Object Relative","Pronominal subject Relative"), values=c("gray20","gray80","gray20","gray80")) #lower numbers of gray are darker
		g0 <- g0 + scale_linetype_manual(name="Condition",limits=c("ORFull","SRFull","ORPro","SRPro"),breaks=c("ORFull","SRFull","ORPro","SRPro"),labels=c("Full NP Object Relative","Full NP subject Relative","Pronominal Object Relative","Pronominal subject Relative"), values=c(1,1,2,2)) #lower numbers of gray are darker
		g0 <- g0 + scale_shape_manual(name="Condition",limits=c("ORFull","SRFull","ORPro","SRPro"),breaks=c("ORFull","SRFull","ORPro","SRPro"),labels=c("Full NP Object Relative","Full NP subject Relative","Pronominal Object Relative","Pronominal subject Relative"), values=c(19,19,17,17)) #lower numbers of gray are darker
		g0 <- g0 + guides(colour = guide_legend(override.aes = list(linetype = c(1,1,2,2))))
		g0 <- g0 + theme(legend.key.width=unit(3,"line"),legend.key = element_blank())
		g0 <- g0 + geom_errorbar(aes(ymin = plo, ymax = phi), width = 0.25)
		g0 <- g0 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
		g0 <- g0 + ylab("Reading Time (ms)")
		g0 <- g0 + xlab("") 
		g0 <- g0 + theme(legend.position=c(.8, .85)) # or whatever looks best
		g0

	

	
################################################################

# Regression (binary) data only....	also p fixation

	summary(data)
	data<-subset(data,!is.na(RT))

	# p(fixation) coded as 0 or 100
	data$RT[data$RT=="100"] <- 1
	
	# Choose appropriate region...
	subdata <- subset(data,region == "mainsubj") # not for p(regression)!
	subdata <- subset(data,region == "that")
	subdata <- subset(data,region == "rcnp")
	subdata <- subset(data,region == "rcverb")
	subdata <- subset(data,region == "mcverb")
	
	aggregate(RT~RC+NP,data=subdata, FUN=mean)	
	
	# Run a model
	rc.glmer.2<-(glmer(RT~c.(RC)*c.(NP)+(1+c.(RC)*c.(NP)|item)+(1+c.(RC)*c.(NP)|subject), data = subdata, family=binomial))
	rc.glmer.1<-(glmer(RT~c.(RC)*c.(NP)+(1+c.(RC)+c.(NP)|item)+(1+c.(RC)+c.(NP)|subject), data = subdata, family=binomial))
	rc.glmer.0<-(glmer(RT~c.(RC)*c.(NP)+(1|item)+(1|subject), data = subdata, family=binomial))
	anova(rc.glmer.1,rc.glmer.0)	

	summary(rc.glmer.0)
	rePCA(rc.glmer.0) # rePCA seems to produce junky output for glmer models - some info on web suggests it should work, but most/all examples are from lmer
	Anova(rc.glmer.0)

	# try using a different optimizer if the default doesn't converge...
	rc.glmer.0<-(glmer(RT~RC*NP+(1|item)+(1|subject),control=glmerControl(optimizer="bobyqa"), data = subdata, family=binomial))
	rc.glmer.0<-(glmer(RT~RC*NP+(1|item)+(1|subject), data = subdata, family=binomial))
	rc.glmer.0<-(glmer(RT~c.(RC)*c.(NP)+(1|item)+(1|subject),control=glmerControl(optimizer="bobyqa"), data = subdata, family=binomial))



		newdat <- expand.grid(
			RC=c("OR","SR") # make sure these are in the right order!
			, NP=c("Full","Pro")
			, RT = 0
		)
		mm = model.matrix(terms(rc.glmer.0),newdat)
		newdat$RT = mm %*% fixef(rc.glmer.0)
		pvar1 <- diag(mm %*% tcrossprod(vcov(rc.glmer.0),mm))  # these are the confidence intervals (range where mean is expected)
		tvar1 <- pvar1+VarCorr(rc.glmer.0)$subject[1]          # these are for prediction intervals (range where next data point is expected to fall) - not clear that this is exactly correct, since we have subject and item variance
		newdat <- data.frame(
			 newdat
			 , plo = newdat$RT-2*sqrt(pvar1)  
			 , phi = newdat$RT+2*sqrt(pvar1)
			 , tlo = newdat$RT-2*sqrt(tvar1)  
			 , thi = newdat$RT+2*sqrt(tvar1)
		)
		# Transform back into probabilities for graphing
		newdat$RTt <- (exp(newdat$RT)/(1+exp(newdat$RT)))
		newdat$plot <- (exp(newdat$plo)/(1+exp(newdat$plo)))
		newdat$phit <- (exp(newdat$phi)/(1+exp(newdat$phi)))
		confidence_interval_data <- newdat #rename for output clarity
		when <- paste("Results for", MeasureName, RegionName, Sys.Date())


# Summarize results (no indents to faciliate copy + paste)
cat("Results for", MeasureName, RegionName)
summary(rc.glmer.0)
confidence_interval_data;when
Anova(rc.glmer.0)






	subdata.full <- subset(subdata,NP == "Full")
	subdata.pro <- subset(subdata, NP == "Pro")


	rc.glmer<-(glmer(RT~c.(RC)*c.(NP)+(1+c.(RC)+c.(NP)|item)+(1+c.(RC)+c.(NP)|subject), data = subdata, family=binomial))
	rc.glmer.0<-(glmer(RT~c.(RC)*c.(NP)+(1|item)+(1|subject), data = subdata, family=binomial))
	anova(rc.glmer,rc.glmer.0)
	fixef(rc.glmer)
	fixef(rc.glmer.0)
	summary(rc.glmer)
	summary(rc.glmer.0)
	
		rc.glmer.1a<-(glmer(RT~c.(RC)*c.(NP)+(1+c.(NP)|item)+(1+c.(RC)+c.(NP)|subject), data = subdata, family=binomial))
		rc.glmer.1b<-(glmer(RT~c.(RC)*c.(NP)+(1+c.(RC)|item)+(1+c.(RC)+c.(NP)|subject), data = subdata, family=binomial))
		rc.glmer.1c<-(glmer(RT~c.(RC)*c.(NP)+(1+c.(RC)+c.(NP)|item)+(1+c.(NP)|subject), data = subdata, family=binomial))
		rc.glmer.1d<-(glmer(RT~c.(RC)*c.(NP)+(1+c.(RC)+c.(NP)|item)+(1+c.(RC)|subject), data = subdata, family=binomial))
		anova(rc.glmer,rc.glmer.1a,rc.glmer.0)
		anova(rc.glmer,rc.glmer.1b,rc.glmer.0)
		anova(rc.glmer,rc.glmer.1c,rc.glmer.0)
		anova(rc.glmer,rc.glmer.1d,rc.glmer.0)
	
	
	
	rc.glmer.full<-(glmer(RT~RC+(1+RC|item)+(1+RC|subject), data = subdata.full, family=binomial))
	rc.glmer.full.0<-(glmer(RT~RC+(1|item)+(1|subject), data = subdata.full, family=binomial))
	anova(rc.glmer.full,rc.glmer.full.0)
	summary(rc.glmer.full)
	summary(rc.glmer.full.0)
	
	rc.glmer.pro<-(glmer(RT~RC+(1+RC|item)+(1+RC|subject), data = subdata.pro, family=binomial))
	rc.glmer.pro.0<-(glmer(RT~RC+(1|item)+(1|subject), data = subdata.pro, family=binomial))
	anova(rc.glmer.pro,rc.glmer.pro.0)
	summary(rc.glmer.pro)
	summary(rc.glmer.pro.0)
	
		rc.glmer.pro.1a<-(glmer(RT~RC+(1|item)+(1+RC|subject), data = subdata.pro, family=binomial))
		rc.glmer.pro.1b<-(glmer(RT~RC+(1+RC|item)+(1|subject), data = subdata.pro, family=binomial))
		anova(rc.glmer.pro,rc.glmer.pro.1a,rc.glmer.pro.0)
		anova(rc.glmer.pro,rc.glmer.pro.1b,rc.glmer.pro.0)
		summary(rc.glmer.pro.1a)
		summary(rc.glmer.pro.1b)

#rc.glmer.0 <- rc.glmer

				newdat <- expand.grid(
					RC=c("OR","SR") # make sure these are in the right order!
					, NP=c("Full","Pro")
					, RT = 0
				)
				mm = model.matrix(terms(rc.glmer.0),newdat)
				newdat$RT = mm %*% fixef(rc.glmer.0)
				pvar1 <- diag(mm %*% tcrossprod(vcov(rc.glmer.0),mm))  # these are the confidence intervals (range where mean is expected)
				tvar1 <- pvar1+VarCorr(rc.glmer.0)$subject[1]          # these are for prediction intervals (range where next data point is expected to fall) - not clear that this is exactly correct, since we have subject and item variance
				newdat <- data.frame(
					 newdat
					 , plo = newdat$RT-2*sqrt(pvar1)  
					 , phi = newdat$RT+2*sqrt(pvar1)
					 , tlo = newdat$RT-2*sqrt(tvar1)  
					 , thi = newdat$RT+2*sqrt(tvar1)
				)
				newdat$RTt <- (exp(newdat$RT)/(1+exp(newdat$RT)))
				newdat$plot <- (exp(newdat$plo)/(1+exp(newdat$plo)))
				newdat$phit <- (exp(newdat$phi)/(1+exp(newdat$phi)))
				print(newdat) # data for confidence intervals (phi and plo)
				
				limits <- aes(ymax = phit, ymin=plot)
				p <- ggplot(newdat, aes(fill=RC, y=RTt, x=NP))
				dodge <- position_dodge(width=0.9)
				p <- p + geom_bar(position=dodge, stat="identity")
				p <- p + theme_bw(base_size = 12, base_family = "")
				p <- p + scale_x_discrete(breaks=c("Full", "Pro"),name="Context Type" ,labels=c("Full NP", "Pronominal NP"))
				p <- p + coord_cartesian(ylim = c(0, 1)) + scale_y_continuous(breaks=seq(0,1,.25))# 700 ms seems to be the max for anything I need for the poster
				p <- p + scale_fill_grey(name="Relative Clause Type", labels=c("Object Relative","subject Relative"))  
				p <- p + geom_errorbar(limits, position=dodge, width=0.25) 
				p <- p + ylab("p(regression)")
				#p <- p + ylab("p(fixation)")
		 		p <- p + scale_shape_discrete(name="Relative Clause Type", labels=c("Object Relative","subject Relative")) # since RC determines both color and shape, we have to do any renaming 2x to avoid a double legend
				p <- p + theme(legend.position=c(.5, .85), legend.key = element_blank())
				p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
				p 
						

	
	

 
# use this chunk of code to get graphs for p(regression) and p(fixation)
		td <- aggregate(subdata$RT,by=list(subdata$RC,subdata$NP),mean)
		colnames(td) <- c("RC", "NP","Probability") 
	
		p <- ggplot(td, aes(fill=RC, y=Probability, x=NP))
		dodge <- position_dodge(width=0.9)
		p <- p + geom_bar(position=dodge, stat="identity")
		p <- p + theme_bw(base_size = 12, base_family = "")
		p <- p + scale_x_discrete(breaks=c("Full", "Pro"),name="NP Type" ,labels=c("Full NP", "Pronominal NP"))
		p <- p + coord_cartesian(ylim=c(0, 1))
		p <- p + ylab("P(regression)") 
		#p <- p + ylab("P(fixation)") 
		p <- p + scale_fill_grey(name="Relative Clause Type", labels=c("Object Relative","subject Relative")) # since RC determines both color and shape, we have to do any renaming 2x to avoid a double legend
		p <- p + theme(legend.position=c(.5, .7), legend.key = element_blank())
		p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(color = 'black'))
		p 

################################################################

# Use this code to test if there is a difference in the number of fixations between L1 and L2 speakers

# Run one dataset through the normal process, then save it here. Then repeat with the other one.

# As appropriate:

	L1data <- data
	L2data <- data

	L1data$Group <- "L1"
	L2data$Group <- "L2"


	summary(L1data)
	summary(L2data)

	L1data$subject <- paste("L1-S",L1data$subject,sep="")
	L2data$subject <- paste("L2-S",L2data$subject,sep="")

	combineddata <- rbind(L1data,L2data)
	combineddata$Group <- factor(combineddata$Group)
	combineddata$subject <- factor(combineddata$subject)

	summary(combineddata)
	
	combined.lmer <- lmer(RT~c.(NP)*c.(RC)*c.(Group)*region+(1|subject)+(1|item), data=combineddata)
	summary(combined.lmer)
	Anova(combined.lmer)




# B&W publication style graphs
drawgraph <- function (x) {
	# graph for straight RTs	
		newdat <- expand.grid(
			RC=c("OR","SR") # make sure these are in the right order!
			, NP=c("Full","Pro")
			, RT = 0
		)
		mm = model.matrix(terms(x),newdat)
		newdat$RT = mm %*% fixef(x)
		pvar1 <- diag(mm %*% tcrossprod(vcov(x),mm))  # these are the confidence intervals (range where mean is expected)
		tvar1 <- pvar1+VarCorr(x)$subject[1]          # these are for prediction intervals (range where next data point is expected to fall) - not clear that this is exactly correct, since we have subject and item variance
		newdat <- data.frame(
			 newdat
			 , plo = newdat$RT-2*sqrt(pvar1)  
			 , phi = newdat$RT+2*sqrt(pvar1)
			 , tlo = newdat$RT-2*sqrt(tvar1)  
			 , thi = newdat$RT+2*sqrt(tvar1)
		)
		print(newdat) # data for confidence intervals (phi and plo)
	
		# Better looking version of plotting confidence intervals
		limits <- aes(ymax = phi, ymin=plo)
		p <- ggplot(newdat, aes(fill=RC, y=RT, x=NP))
		dodge <- position_dodge(width=0.9)
		p <- p + geom_bar(position=dodge, stat="identity")
		p <- p + theme_bw(base_size = 12, base_family = "")
		p <- p + scale_x_discrete(breaks=c("Full", "Pro"),name="Context Type" ,labels=c("Full NP", "Pronominal NP"))
		#p <- p + coord_cartesian(ylim = c(0, 425)) + scale_y_continuous(breaks=seq(0,425,25))# for now, let ggplot choose the scale
		p <- p + scale_fill_grey(name="Relative Clause Type", labels=c("Object Relative","subject Relative"))  
		p <- p + geom_errorbar(limits, position=dodge, width=0.25) 
		p <- p + ylab("Reading Time (ms)") 
		p <- p + scale_shape_discrete(name="Relative Clause Type", labels=c("Object Relative","subject Relative")) # since RC determines both color and shape, we have to do any renaming 2x to avoid a double legend
		p <- p + theme(legend.position=c(.8, .8), legend.key = element_blank())
		p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
		p 
		
		MyWidth <- 5.5 # width in inches
		pdf("FigureX.pdf",MyWidth,MyWidth*.618) # for now, a 7" wide graph, with a 1:.618 ratio. Since it's a PDF, (and done with vector graphics), we can resize the graph in the document. But, the size we choose here will affect the font size
		p
		dev.off()
		
}

# Color powerpoint style graphs
drawgraph <- function (x) { 
	# graph for straight RTs	
		newdat <- expand.grid(
			RC=c("OR","SR") # make sure these are in the right order!
			, NP=c("Full","Pro")
			, RT = 0
		)
		mm = model.matrix(terms(x),newdat)
		newdat$RT = mm %*% fixef(x)
		pvar1 <- diag(mm %*% tcrossprod(vcov(x),mm))  # these are the confidence intervals (range where mean is expected)
		tvar1 <- pvar1+VarCorr(x)$subject[1]          # these are for prediction intervals (range where next data point is expected to fall) - not clear that this is exactly correct, since we have subject and item variance
		newdat <- data.frame(
			 newdat
			 , plo = newdat$RT-2*sqrt(pvar1)  
			 , phi = newdat$RT+2*sqrt(pvar1)
			 , tlo = newdat$RT-2*sqrt(tvar1)  
			 , thi = newdat$RT+2*sqrt(tvar1)
		)
		print(newdat) # data for confidence intervals (phi and plo)
	
		# Better looking version of plotting confidence intervals
		limits <- aes(ymax = phi, ymin=plo)
		p <- ggplot(newdat, aes(fill=RC, y=RT, x=NP))
		dodge <- position_dodge(width=0.9)
		p <- p + geom_bar(position=dodge, stat="identity")
		#p <- p + theme_bw(base_size = 18, base_family = "")
		p <- p + scale_fill_manual(values=c("red","blue","green"))
		p <- p + scale_x_discrete(breaks=c("Full", "Pro"),name="Context Type" ,labels=c("Full NP", "Pronominal NP"))
		#p <- p + coord_cartesian(ylim = c(0, 425)) + scale_y_continuous(breaks=seq(0,425,25))# for now, let ggplot choose the scale
		#p <- p + scale_fill_grey(name="Relative Clause Type", labels=c("Object Relative","subject Relative"))  
		p <- p + geom_errorbar(limits, position=dodge, width=0.25) 
		p <- p + ylab("Reading Time (ms)") 
		p <- p + scale_shape_discrete(name="Relative Clause Type", labels=c("Object Relative","subject Relative")) # since RC determines both color and shape, we have to do any renaming 2x to avoid a double legend
		p <- p + theme(legend.position=c(.6, 1), legend.key = element_blank(),plot.background = element_blank())
		p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),axis.line = element_line(color = 'black'))
		p 
}

drawgraph2 <- function (x) { 
	# graph for straight RTs, one NP type only	
		newdat <- expand.grid(
			RC=c("OR","SR") # make sure these are in the right order!
			, RT = 0
		)
		mm = model.matrix(terms(x),newdat)
		newdat$RT = mm %*% fixef(x)
		pvar1 <- diag(mm %*% tcrossprod(vcov(x),mm))  # these are the confidence intervals (range where mean is expected)
		tvar1 <- pvar1+VarCorr(x)$subject[1]          # these are for prediction intervals (range where next data point is expected to fall) - not clear that this is exactly correct, since we have subject and item variance
		newdat <- data.frame(
			 newdat
			 , plo = newdat$RT-2*sqrt(pvar1)  
			 , phi = newdat$RT+2*sqrt(pvar1)
			 , tlo = newdat$RT-2*sqrt(tvar1)  
			 , thi = newdat$RT+2*sqrt(tvar1)
		)
		print(newdat) # data for confidence intervals (phi and plo)
	
		# Better looking version of plotting confidence intervals
		limits <- aes(ymax = phi, ymin=plo)
		p <- ggplot(newdat, aes(fill=RC, y=RT,x=RC))
		dodge <- position_dodge(width=0.9)
		p <- p + geom_bar(position=dodge, stat="identity")
		p <- p + scale_fill_manual(values=c("red","blue","green"))
		p <- p + geom_errorbar(limits, position=dodge, width=0.25) 
		p <- p + ylab("Reading Time (ms)") 
		p <- p + scale_shape_discrete(name="Relative Clause Type", labels=c("Object Relative","subject Relative")) # since RC determines both color and shape, we have to do any renaming 2x to avoid a double legend
		p <- p + theme(legend.position=c(.6, 1), legend.key = element_blank(),plot.background = element_blank())
		p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),axis.line = element_line(color = 'black'))
		p 
}

# Code for analysis where all RC regions are treated as a single large region
#  is now located at the end of the file

	
	data<-read.csv("one.gopast.ixs") # Buffalo data with all of RC treated as a single large region, note that this file was prepared using (non windows) eyedry, with output in long format
	colnames(data) <- c("sequence","subject","item","condition","region","RT") # Use this only for data where the RC is treated as one large region
	summary(data)		 # note that sequence if affected by the number of recallibrations, so it can go over the total number of items
	data <- subset(data,RT>0) # remove rows where RT is 0 (all fixations less than 80ms were already removed, so these are just regions that had no fixations)
	data$RC[data$condition == "1"] <- "OR"
	data$RC[data$condition == "2"] <- "SR"
	data$RC[data$condition == "3"] <- "OR"
	data$RC[data$condition == "4"] <- "SR"
	data$RC <- factor(data$RC)

	data$NP[data$condition == "1"] <- "Full"
	data$NP[data$condition == "2"] <- "Full"
	data$NP[data$condition == "3"] <- "Pro"
	data$NP[data$condition == "4"] <- "Pro"
	data$NP <- factor(data$NP)
	
	data$region[data$region == "1"] <- "mainsubj"
	data$region[data$region == "2"] <- "combinedRC"
	data$region[data$region == "3"] <- "mcverb"
	data$region[data$region == "4"] <- "remainder"
	data$region[data$region == "5"] <- "postsentence"
	data$region <- factor(data$region, levels=c("mainsubj","combinedRC","mcverb","remainder","postsentence"))
	data$item <- factor(data$item)
	data$subject <- factor(data$subject)
	data$condition <- factor(data$condition)	
	
	length(unique(data$subject))	
	length(unique(data$item))
	table(data[,c("subject","item")])
	subjectcounts<-aggregate(item~subject,data=data,FUN=function (x) length(unique(x)))
	colnames(subjectcounts)<- c("subject", "numberOfValiditems")
	subjectcounts$percentvaliditems<-subjectcounts$numberOfValiditems/20
	subjectcounts
	nrow(subset(subjectcounts,percentvaliditems>=.75)) # keep 61/69 participants at 75% 

	nrow(data)  #4745 - this is a sanity check to make sure we're not multiplying the table
	data<-merge(data,subjectcounts)
	nrow(data)  # still produces 4745, so we're ok

	data <- subset(data,percentvaliditems>=.75)	
	head(data)
	data$subject <- factor(data$subject)
	length(unique(data$subject))	
	sort(unique(data$subject))
	# Participants to drop from buffalo data
	data <- subset(data,subject !="19")
	data <- subset(data,subject !="36")	
	length(unique(data$subject))	 # 59 participants left for Buffalo data

	subdata <- subset(data,region == "combinedRC")	 # there should be no differences for any of the other regions, since they aren't affected by how the RC regions are treated
	rc.lmer<-(lmer(RT~c.(RC)*c.(NP)+(1+c.(RC)+c.(NP)|item)+(1+c.(RC)+c.(NP)|subject), data = subdata))
	rc.lmer.trimmed<-(lmer(RT~c.(RC)*c.(NP)+(1+c.(RC)+c.(NP)|item)+(1+c.(RC)+c.(NP)|subject), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))
	rc.lmer.trimmed.min<-(lmer(RT~c.(RC)*c.(NP)+(1|item)+(1|subject), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))
	rc.lmer.trimmed.max<-(lmer(RT~c.(RC)*c.(NP)+(1+c.(RC)*c.(NP)|item)+(1+c.(RC)*c.(NP)|subject), data = subdata, subset = abs(scale(resid(rc.lmer)))<2.5))
	anova(rc.lmer.trimmed,rc.lmer.trimmed.max,rc.lmer.trimmed.min)
	
	fixef(rc.lmer.trimmed.max)
	fixef(rc.lmer.trimmed)
	fixef(rc.lmer.trimmed.min)

	summary(rc.lmer.trimmed.max)
	summary(rc.lmer.trimmed)
	summary(rc.lmer.trimmed.min)
	drawgraph(rc.lmer.trimmed)


