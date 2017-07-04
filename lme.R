# Masa Yano (Tohoku Univ. JSPS)

# f1 = Control: -0.5, OverGenerated: 0.5, Other: 0

library(lme4)
library(lmerTest)

# Load EEG data
data = read.delim(file.choose(), sep = ",", header = TRUE)
# Artefact Rejection
data = subset(data, data$AR != 1)
# head(data)

# Coding
ChName = c("P7", "P8", "P3", "P4", "Pz", "O1", "O2")
ch = 1:length(ChName)
ch = cbind(data.frame(ChName), data.frame(ch))
data = merge(data, ch)
# unique(data$ch)
# unique(data$ChName)

# Factor
data$subj = as.factor(data$subj)
# data$set = as.factor(data$set)
data$ch = as.factor(data$ch)

# LME Model1 = lmer(EEG ~ f1 + (1+f1|subj) + (1+f1|Set) + (1|ch), data = data)
Model1 = lmer(EEG ~ f1 + (1 + f1 | subj) + (1 | ch), data = subset(data,data$f1 != 0))
round(summary(Model1)$coef, digits = 2)
