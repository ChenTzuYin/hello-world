# Masa Yano (Tohoku Univ. JSPS)

# Install package
install.packages('tidyverse')
library(dplyr)

# Set Current Working Directory
setwd("/Users/masayano/Desktop/tutorial20170623/CSV/")
# getwd()

# Import & Bind? CSV files
data = c()
chNum = paste("ch",1:66, sep = "")

for (n in 1:length(list.files(pattern = "csv"))){
  if (n == 1){
    IndivData = read.csv(list.files(pattern = "csv")[n], skip= 6)
    SubjName = read.csv(list.files(pattern = "csv")[n], nrows=1, sep = ",", header = F)
    data = cbind(SubjName, IndivData)
    names(data) = c("Subj","trial",chNum,"AR","Binlabel")
  }else{
    IndivData = read.csv(list.files(pattern = "csv")[n], skip = 6)
    SubjName = read.csv(list.files(pattern = "csv")[n], nrows=1, sep = ",", header = F)
    IndivData = cbind(SubjName, IndivData)
    names(IndivData) = c("Subj","trial",chNum,"AR","Binlabel")
    data = rbind(data, IndivData)
  }
}
head(data)

# sort
data = data[order(data$Subj,data$Binlabel),]

# tidyr
data = tidyr::gather(data = data, key = chNum, value = EEG, ch1:ch66)
head(data)

# Add ChName
ChName = c("Fp1", "Fpz", "Fp2", "AF3", "AF4", "F7", "F5", "F3", "F1", "Fz", "F2", "F4", "F6", "F8", "FT7", "FC5", "FC3", "FC1", "FCz", "FC2", "FC4", "FC6", 
           "FT8", "T7", "C5", "C3", "C1", "Cz", "C2", "C4", "C6", "T8", "A1", "TP7", "CP5", "CP3", "CP1", "CPz", "CP2", "CP4", "CP6", "TP8", "A2", "P7", "P5", 
           "P3", "P1", "Pz", "P2", "P4", "P6", "P8", "PO7", "PO5", "PO3", "POz", "PO4", "PO6", "PO8", "OI1", "O1", "Oz", "O2", "OI2", "VEOG", "HEOG")
ChNumName = cbind(data.frame(chNum), data.frame(ChName))
data = merge(data, ChNumName)
head(data)

# Dummy Coding (if necessary)
data$f1[data$Binlabel == "1"] = -0.5
data$f1[data$Binlabel == "2"] = 0.5
data$f1[data$Binlabel == "3"] = 0
data$f1[data$Binlabel == "4"] = 0
head(data)

# Export
write.table(data, "~/Desktop/EEGdata.csv", quote = FALSE, row.names = FALSE, col.names = TRUE, append = FALSE)
