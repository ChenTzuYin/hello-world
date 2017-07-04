
#Create Date:2017-03-29
#For Linguistic Vanguard
#MF02A has 24 columns from Eprime event

########### 1.package install ###########

library(reshape)  #cast(),melt()
library(ggplot2)
library(lme4)

VarRank <-function(x){ 
print("################The original variance reports#################")
ModelVar<- print(VarCorr(x),comp=c("Variance","Std.Dev"))
ModelVar.frame <- as.data.frame(ModelVar)
ModelVar.frame$var1 <- ifelse(is.na(ModelVar.frame$var1),-1,ModelVar.frame$var1)
ModelVar.frame$var2 <- ifelse(is.na(ModelVar.frame$var2),-1,ModelVar.frame$var2)
ModelVar.frame <- ModelVar.frame[ModelVar.frame$var1!="(Intercept)",]
ModelVar.frame <- ModelVar.frame[ModelVar.frame$grp!="Residual",]
ModelVar.frame <- ModelVar.frame[ModelVar.frame$var2==-1,]
ModelVar.frame <- ModelVar.frame[order(ModelVar.frame$vcov,decreasing=F),]

print("################The factor you should delete#################")
print(ModelVar.frame[1,])

#print("################The variance rank is#################")
#print(ModelVar.frame)
}

#VarRank(m)

########### 2. list the trials ###########

dataList <- list.files(pattern = "chen_practice_New") 
dataList
length(dataList) #check the number of the files
dataList[1]      #check the filename

########### 3. remove trial without fixation ###########

for(n in 1:length(dataList)){
temp <- read.table(dataList[n], head=T, sep="\t", na.string="NA", encoding="UTF-8")
temp <- temp[temp$GazeEventType == "Fixation",]
nrow(temp)
if (nrow(temp) == 0) {
    print(paste("Bad trial!", dataList[n]))
  }
}

#put the bad trials into [Bad trials] folder

########### 4. the column number of eventdata ###########

numcol = 24

          #Column Name
          #"ListNo", "Condition",  "ItemNoA", "ItmeNoB", "C1Type", "C2Type1", "C2Type2", "SoundType", "TrialOrFiller", "Picture", "Sound", "CorrectAnswer", "AOI1", "AOI2", "AOI3", "AOI4", "TypeTag1", "TypeTag2", "TypeTag3", "TypeTag4", "LocationTag1", "LocationTag2", "LocationTag3", "LocationTag4"

          #Example
          #"f", "8", "68", "NP", "NA", "NA", "NPNA", "Filler", "A_fNP08", "NP4NA3", "AOI1", "Target", "CompetitorC", "CompetitorB", "CompetitorA", "NP4NA3", "NP4NA2", "NP4NA1", "NP4NA4", "NP4NA3", "NP4NA4", "NP4NA1", "NP4NA2"
          #there is no "ListNo" in filler condition e and f, I extract them from the original data folder

########### 5. collect to one frame ###########

dataall <- NULL    #creat an empty frame

for(n in 1:length(dataList)){
      print(paste("now access", n))
      temp <- read.table(dataList[n], head=T, sep="\t", na.string="NA", encoding="UTF-8")
      eventdata <- temp[1,]$StudioEventData  

#Delete unnecessary columns; check you only have the right columns.
          #extract necessary columns
  temp <- temp[,c("X.U.FEFF.ParticipantName", "SegmentName", "SegmentStart", "SegmentEnd", "SegmentDuration", "RecordingTimestamp", "FixationIndex", "SaccadeIndex", "GazeEventType", "GazeEventDuration", "FixationPointX..MCSpx.", "FixationPointY..MCSpx.", "PupilLeft", "PupilRight")]
          #rename
  colnames(temp) <- c("ParticipantName", "SegmentName", "SegmentStart", "SegmentEnd", "SegmentDuration", "RecordingTimestamp", "FixationIndex", "SaccadeIndex", "GazeEventType", "GazeEventDuration", "FixationPointX", "FixationPointY", "PupilLeft", "PupilRight")
          #calculate the reaction duration
  temp$rectime <- temp$RecordingTimestamp - temp$SegmentStart
          #extract necessary columns 
          #deleted:SegmantStart, SegmentEnd, SegmentDuration, RecordingTimestamp, PupilLeft, PupilRight
  temp <- temp[,c("ParticipantName", "SegmentName", "FixationIndex", "GazeEventType", "GazeEventDuration", "FixationPointX", "SaccadeIndex", "FixationPointY", "rectime")]
          #extract Fixation and Saccade
  temp <- temp[temp$GazeEventType != "Unclassified",]

          #replace FixationIndex by SaccadeIndex if Fixation is NA
  temp$FixationIndex <- ifelse(is.na(temp$FixationIndex), temp$SaccadeIndex, temp$FixationIndex)
          #replace NA by -1 for easy recognition
  temp$FixationPointX <- ifelse(is.na(temp$FixationPointX), -1, temp$FixationPointX)
  temp$FixationPointY <- ifelse(is.na(temp$FixationPointY), -1, temp$FixationPointY)
          #delete Saccade column
  temp$SaccadeIndex <- NULL
  
  head(temp, 300)


          #ここから、同じFixationになっている時間帯を一つの行にまとめます。（元々は3ms間隔で記録されてきましたが）
   min <- aggregate(temp$rectime, list(temp$ParticipantName, temp$SegmentName, temp$FixationIndex,temp$GazeEventType, temp$GazeEventDuration, temp$FixationPointX, temp$FixationPointY), min)
　　　     #同じFixationになっている時間帯の開始時間をminで算出して、新しいオブジェクトのminにまとめます
   colnames(min) <- c("ParticipantName", "SegmentName", "FixationIndex", "GazeEventType", "GazeEventDuration", "FixationPointX", "FixationPointY", "gstart")
   min <- min[order(min$ParticipantName, min$SegmentName, min$gstart),]
   head(min, 10)
   
   max <- aggregate(temp$rectime, list(temp$ParticipantName, temp$SegmentName, temp$FixationIndex,temp$GazeEventType, temp$GazeEventDuration, temp$FixationPointX, temp$FixationPointY), max)
　　　     #同じFixationになっている時間帯の終了時間をmaxで算出して、新しいオブジェクトのmaxにまとめます
   colnames(max) <- c("ParticipantName", "SegmentName", "FixationIndex", "GazeEventType", "GazeEventDuration", "FixationPointX", "FixationPointY", "gend")
   max <- max[order(max$ParticipantName, max$SegmentName, max$gend),]
   head(max, 10)

   temp2 <- cbind(min, max[,8])
　　　     #minとmaxを合併する（minをベースにして、maxから"gend"情報だけを取り出します）
   colnames(temp2) <- c("ParticipantName", "SegmentName", "FixationIndex", "GazeEventType", "GazeEventDuration", "FixationPointX", "FixationPointY", "gstart", "gend")
   head(temp2, 30)

　　　　　　#ここからEprime情報を区切ってtemp2の右側にマッピングします。
   temp2$StudioEventData <- as.character(eventdata)
　　　　　　#文字形式（Factorでも数値でもない）を確保しておきます。
   list <- unlist(strsplit(temp2$StudioEventData, " "))
　　　　　　#Eprime情報を空白で区切ってlistにまとめます。

    if (length(list)/numcol != nrow(temp2)) {
    print(paste("Bad trial!", dataList[n]))
    }

  mat <- matrix(list, nrow = nrow(temp2), ncol = numcol, byrow=T)

  mat <- as.data.frame(mat)
　　　　　　#matをデータフォーム形式にします
  colnames(mat) <- c("ListNo","Condition",  "ItemNoA", "ItemNoB", "C1Type", "C2Type1", "C2Type2", "SoundType", "TrialOrFiller", "Picture", "Sound", "CorrectAnswer", "AOI1", "AOI2", "AOI3", "AOI4", "TypeTag1", "TypeTag2", "TypeTag3", "TypeTag4", "LocationTag1", "LocationTag2", "LocationTag3", "LocationTag4")
　　　　　　#matにコラム名を付け加えます。
  temp3 <- cbind(temp2, mat)
　　　　　　#temp2(Tobii)とmat(Eprime)を合併します。

  dataall <- rbind(dataall, temp3)
}

########### 6. Map X-Y-aixs to AOI area ###########
　　　　　　　　　　　　　　　　　
          #今まではTobiiのsegement機能で一個一個ドラッグする必要がありましたが、下のスクリプトではそういう必要がなくなります。
dataall$AOI <- ifelse(dataall$FixationPointX >= 0 & dataall$FixationPointX < 960 & dataall$FixationPointY >=0 & dataall$FixationPointY < 540, 1, 0)
dataall$AOI <- ifelse(dataall$FixationPointX >= 960 & dataall$FixationPointX <= 1980 &dataall$FixationPointY >=0 & dataall$FixationPointY < 540, 2, dataall$AOI)
dataall$AOI <- ifelse(dataall$FixationPointX >= 0 & dataall$FixationPointX < 960 & dataall$FixationPointY >= 540 & dataall$FixationPointY <= 1080, 3, dataall$AOI)
dataall$AOI <- ifelse(dataall$FixationPointX >= 960 & dataall$FixationPointX <= 1980 & dataall$FixationPointY >= 540 & dataall$FixationPointY <= 1080, 4, dataall$AOI)

　　　　　　　#ここまでではSaccadeのデータもいらなくなりました（XY軸の情報がないです）。Fixationだけを残せば大丈夫です。
dataall$GazeEventDuration <- NULL; 
dataall$StudioEventData <- NULL; 
dataall$FixationIndex <- NULL; 
dataall$GazeEventType <- NULL
　　　　　　　#必要のない情報を削除します。
head(dataall)
table(dataall$ParticipantName, dataall$SegmentName)
table(dataall$ParticipantName, dataall$Condition)
　　　　　　　　　　　　　　　　　#データ全体のバランスを確認します。
write.csv(dataall, "Output2.csv", row.names=F)
　　　　　　　　　　　　　　　　　#保存します。



########## 3.1 グラフ作成##########

dataall <- read.csv("Output2.csv", header =T)
head(dataall,100)
　　　　　　　　　　　　　　　　　#グラフの全体の時間軸の幅をここで決めます。（複合名詞全体は約8000ms,一文字は約3000ms）
binname <- seq(20,4000,20)
bindata <- matrix(0,nrow = nrow(dataall), ncol = length(binname))
colnames(bindata) <- binname

for (i in 1:length(binname)){
  bindata[,i] <- ifelse((dataall$gstart < (0 + i*20) & dataall$gend > (0 +i*20)), 1, 0)
}
                   #＜調整例＞
　　　　　　　　　　　　　　　　　　　#開始始時点が [0]
                   #  bindata[,i] <- ifelse((dataall$gstart < (0 + i*20) & dataall$gend > (0 +i*20)), 1, 0)
                   #開始始時点が [onset2]
                   #  bindata[,i] <- ifelse((dataall$gstart < (dataall$onset2 + i*20) & dataall$gend > (dataall$onset2 +i*20)), 1, 0)
                   
　　　　　　　　　　　　　　　　　　　#意味：この行のgstart~gendの範囲の中でそのbinの数値（例：40ms）が入ってれば、1と表示する。
                   #[onset2 + i*20]にすれば、onset2が開始時点になって、それ以前の時間帯は見ないことになる。
　　　　　　　　　　　　　　　　　　　#例：i=0であれば、一行目からはonset2のその数値をgstart~gendに当てはめることになる

gr <- cbind(dataall, as.data.frame(bindata))
　　　　　　　　　　　　　　　　　#dataall(trial情報)とbindata（bin注視情報）と合併

　　　　　　　　　　　　　　　　　　　#数値であったAOIを文字に転換する
　　　　　　　　　　　　　　　　　　　#AOI == 0 means looks to marginal area.
gr$target <- ifelse(gr$AOI == 1, as.character(gr$AOI1), "bg")
gr$target <- ifelse(gr$AOI == 2, as.character(gr$AOI2), gr$target)
gr$target <- ifelse(gr$AOI == 3, as.character(gr$AOI3), gr$target)
gr$target <- ifelse(gr$AOI == 4, as.character(gr$AOI4), gr$target)

　　　　　　　　　　　　　　　　　　　#必要なコラムだけを取り出す
　　　　　　　　　　　　　　　　　　　#participant,cond,itemB,(X)trail,AOI,target,bin20~4000
gr <- gr[,c(1, 8, 10, 31, ncol(gr), 32:(ncol(gr)-1))]

                   #melt time bins into one column.
                   #grを立てにする。t20の時にAOI2に入っているのか。
gr2 <- melt(gr,id=c("ParticipantName","Condition","ItemNoB","AOI", "target"))
gr2$variable <- as.numeric(as.character(gr2$variable)) 
                   #melt()かけたら、binがvariable,入っているか(1/0)がvalueになる。



　　　　　　　　　　　　　　　　　#わかりやすいため並べ替えます
gr2 <- gr2[order(gr2$ParticipantName, gr2$ItemNoB),]

                   #aggregate for each trial (participants x items)
gr3 <-aggregate(gr2$value, by=list(gr2$ParticipantName, gr2$ItemNoB, gr2$Condition, gr2$AOI, gr2$target, gr2$variable), FUN=sum, na.rm=TRUE)
                   #Tobiiの記録の中で、一定の期間であるAOIが視野に入ったら一カウントされます。
　　　　　　　　　　　　　　　　　#逆にその期間に視野に入ってないbinの部分には０と表記され、AOIが多く重複されています
　　　　　　　　　　　　　　　　　　　#この式は一つのbinの中に重複のないように、AOIをaggregate()で消します。
                   #違う言い方だとすると、違う区間で111が入ったら、同じ行にまとめられます。

colnames(gr3) = c("subj","item","cond","AOI", "variable", "bin","value")
gr3 <- gr3[order(gr3$subj, gr3$item, gr3$bin),]
gr3$AOI <- NULL

                   #ただし、最初から最後までも見られたのないAOIはどうしてもカウントされません
　　　　　　　　　　　　　　　　　　　#それがカウントされないと、比率の母数が正しく求められません
　　　　　　　　　　　　　　　　　　　#（例：「6回提示の中にtargetが何回見られた」の中、「6回」が0だとしても正しく求められません）
　　　　　　　　　　　　　　　　　　　#そうすると、もう一回cast()でデータを開いて、targetがNAのところを0に入れ替える
gr.temp <- cast(gr3)
                   #cast()を実行すると、gr3の"variable"と"value"にしたがって自動的に分解してもらえます。
　　　　　　　　　　　　　　　　　　　#(例：variableの"CompetitorCompound"などが横になって、"value"がその下に)
 
                   #一回も出たことないtargetがNAになっているはずです。それを0に書き換えます。
gr.temp$Target <- ifelse(is.na(gr.temp$Target),0,gr.temp$Target)
gr.temp$CompetitorA <- ifelse(is.na(gr.temp$CompetitorA), 0, gr.temp$CompetitorA)
gr.temp$CompetitorB <- ifelse(is.na(gr.temp$CompetitorB), 0, gr.temp$CompetitorB)
gr.temp$CompetitorC <- ifelse(is.na(gr.temp$CompetitorC), 0, gr.temp$CompetitorC)


                   #他に見たい組み合わせも簡単にできる
gr.temp$Combined <- gr.temp$Target + gr.temp$CompetitorA
gr.temp$unrelated <- gr.temp$CompetitorB + gr.temp$CompetitorC

write.csv(gr.temp, "Output3.csv", row.names=F)

　　　　　　　　　　　　　　　　　　　#(t1) Combined (t2) TargetCompound (t3) CompetitorCompound (t4) TargetSimplex
#gr.temp$t1 <- gr.temp$
#gr.temp$t2 <- gr.temp$TargetCompound
#gr.temp$t3 <- gr.temp$CompetitorCompound
#gr.temp$t4 <- gr.temp$TargetSimplex
#gr.temp$t5 <- gr.temp$IrrelevantSimplex
#gr.temp$t6 <- gr.temp$IrrelevantCompound

　　　　　　　　　　　　　　　　　　　#aggregate for graph (Use t1~t4)
                   #グラフ作成ためのデータフレーム

gra <- aggregate(gr.temp$Combined, by=list(gr.temp$bin, gr.temp$cond), mean)
colnames(gra) <- c("bin", "cond", "mean")
head(gra)

gra1 <- gra[gra$cond=="c" | gra$cond=="d",]

　　　　　　　　　　　　　　　　　　　#グラフ作成の本番
g <- ggplot(data=gra1, aes(x=bin, y=mean, shape=cond, fill=cond))+ 
  geom_line() +
  geom_point(size=2) +
  scale_shape_manual(values=c(21,25,21,25))+
  scale_fill_manual(values=c("black","black",NA,NA))+
  #scale_linetype_manual(values=c("solid","dashed","solid","dashed"))+
  scale_x_continuous("Time(ms)") + 
  scale_y_continuous(limits=c(0,1),name="Proportion of looks to target") +
  scale_color_discrete("Condition") +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 20))+
  #labs(shape="Condition",fill="Condition")+
  theme(legend.title=element_text(size=10))+  
  theme(legend.text=element_text(size=12))+
  ggtitle("[Visual] cd.Combined")
    
　　　　　　　　　　　　　　　　　　　　#必要に応じて変えます　#名付け：g.ALL24.onset3.TCCC/ g.ALL24.onset3.TC/ g.ALL24.onset3.CC/ g.ALL24.onset3.TS
g
########## 3.2 グラフ保存##########　　　　　　　　　　　　　　　　　　　
####一つの図で保存 library(gridExtra)
ppi<-300　　　　　　　　　　　　#画質をscreen上きれいに映すようにpixel=300ppiに設定
png("g.cd.Combined.png",width=12*ppi,height=6*ppi,res=ppi)
　　　　　　　　　　　　　　　　　　　　#png()でpng拡張子のグラフで外部保存します。ppiに応じて長さと幅の設定は　12*6*ppi
g
dev.off()           #もう一度グラフのオブジェクトを読み込みます#dev.off()で閉じまいとファイルが保存されません　　　　　　　　　　　
####複数の図の一覧表を保存

#ALL24
library(gridExtra)　　#パッケージのggplot2ではなく、別にgridExtraが必要です。
ppi<-300
png("TobiiEachParticipant.ALL24.LatterHalf.onset4.png",width=24*ppi, height=12*ppi,res=ppi)
                    #幅は一個のグラフの2倍の24*ppiになります
graph<-grid.arrange(g.ALL24.LatterHalf.onset4.TCCC,g.ALL24.LatterHalf.onset4.TC,g.ALL24.LatterHalf.onset4.CC,g.ALL24.LatterHalf.onset4.TS,ncol=2)
                    #grid.arrange()でR内でできたグラフのオブジェクトをまとめます。（外部のpngファイルではなく）
dev.off()

########## permutation analysis:by modifier################
###########################################################

dataall <- read.csv("Output3.csv", header =T)
head(dataall)

data <- aggregate(dataall$CompetitorA, by=list(dataall$subj, dataall$item, dataall$cond, dataall$bin), mean)
colnames(data) <- c("subj", "item", "cond","bin","look")
data <- data[order(data$subj, data$item, data$cond, data$bin),]
head(data)

data$modifier <- ifelse(data$cond=="a" | data$cond=="b","t3","non-t3")
data$head <- ifelse(data$cond=="a" | data$cond=="c","Competitor.T3","Competitor.non-T3")
data1<-data

means.df1 <- aggregate(look ~ bin + modifier + head, data, mean)

head(means.df1)

###add Target
dataall <- read.csv("Output3.csv", header =T)
head(dataall)

data <- aggregate(dataall$Target, by=list(dataall$subj, dataall$item, dataall$cond, dataall$bin), mean)
colnames(data) <- c("subj", "item", "cond","bin","look")
data <- data[order(data$subj, data$item, data$cond, data$bin),]
head(data)

data$modifier <- ifelse(data$cond=="a" | data$cond=="b","t3","non-t3")
data$head <- ifelse(data$cond=="a" | data$cond=="c","Target.T3","Target.non-T3")
data2<-data

means.df2 <- aggregate(look ~ bin + modifier + head, data, mean)
head(means.df2)

means.df <- rbind(means.df1, means.df2)
head(means.df)
means.df.store<-means.df
means.df <-means.df.store
means.df <-means.df[means.df$head=="Target.non-T3"|means.df$head=="Competitor.non-T3",]

data.store<-rbind(data1, data2)
data<-data.store
data <-data[data$head=="Target.non-T3"|data$head=="Competitor.non-T3",]
###



#draw the basic graph
p = ggplot(means.df, aes(x=bin, y=look, colour=head))
p = p + geom_line()
p = p + scale_colour_brewer(palette="Set1")
p = p + theme_bw()
p = p + ylab("Looks to Target and Competitor")
p = p + ylim(0,1)
p = p + facet_wrap(~modifier, ncol=1)
p = p + geom_vline(xintercept = 1000, colour="black", linetype = 2)
p = p + geom_vline(xintercept = 2000, colour="black", linetype = 2)
p = p + geom_vline(xintercept = 3000, colour="black", linetype = 2)
p

#do a standard mixed model analysis using 500 ms windows

data$win = as.integer(-0.001 + data$bin/500)
head(data)
subjmeans.df = aggregate(look ~ win + modifier + head + subj, data, mean)
subjmeans.df$cwin = subjmeans.df$win-mean(subjmeans.df$win)
subjmeans.df$cmodifier = ifelse(subjmeans.df$modifier =="t3", 0.5, -0.5)
#subjmeans.df$chead = ifelse(subjmeans.df$head == "T3", 0.5, -0.5)
subjmeans.df$chead = ifelse(subjmeans.df$head == "Target.non-T3", 0.5, -0.5)
model = lmer(look ~cwin*cmodifier*chead + (1|subj), subjmeans.df)
print(summary(model))
 
#creat copy of data frame
pdata=data
#pdata$chead = ifelse(pdata$head=="T3", 0.5, -0.5)
pdata$chead = ifelse(pdata$head=="Target.non-T3", 0.5, -0.5)

#creat data frame which averages over subjects.
#This also stores the results of the permutation analysis
means.df = aggregate (look ~ head + chead + bin + modifier, pdata, mean)
means.df$pstr = 1000

#permutation analysis uses simple tests to find significant regions.
#Here is a regression on target based on structure for one time window (400ms) in T3 modifier
onetime = subset(pdata, bin==2000 & modifier == "non-t3")

#do regression model on target using structure condition
onemodel = lm(look ~ chead, onetime)
print(summary(onemodel))
print(abs(coef(summary(onemodel))[2,4]))



#we do this for each 20ms window in the data
timelist = unique(pdata$bin)

#l="t3"
#t=500 #for check one trial

for (l in c("t3","non-t3")){
  for (t in timelist){
   #create data frame for ONE timebin for each modifier
   onetime = subset(pdata, bin == t & modifier == l)
   #do regression model on target using head condition
   onemodel = lm (look ~ chead, onetime)
   #print(summary(onemodel))
   #this is the t-value for structure
   targetT = coef(summary(onemodel))[2,3] #observed t-value
   targetP = abs(coef(summary(onemodel))[2,4]) #observed p-value
   means.df$tstr[means.df$bin == t & means.df$modifier == l] = targetT
   means.df$pstr[means.df$bin == t & means.df$modifier == l] = targetP
 }
}

#to see these p-values, we draw them arbitrarily on the graph at 0.2
#when the p-value < 0.05, we draw a blue line above 0.2
#when the p-value > 0.05, we draw an orange line below 0.2

wsize = 8
pliney = 0.2
plinemax = 0.1
means.df$pline = pliney+plinemax*(0.05-means.df$pstr)
means.df$plinecol = ifelse(means.df$pstr < 0.05,"a","b")

p = ggplot(means.df , aes( x = bin, y = look, colour=head))
p = p + geom_line()
p = p + scale_colour_brewer(palette="Set1")
p = p + scale_fill_brewer(palette="Set2")
p = p + theme_bw()   # change background to white
p = p + ylab("Looks to passive match")
p = p + ylim(0,1)
p = p + facet_wrap(~ modifier, ncol=1)
p = p + geom_rect(aes(xmin=bin-wsize, xmax=bin+wsize, ymin = pliney, ymax= pline, colour=NA,fill=plinecol),show.legend=FALSE)
p

#cluster
cnum = 1
lastpval = 100
means.df$cnum = 1
for (l in c("t3","non-t3")){
  for (t in timelist){
    onetime = subset(means.df,bin == t & modifier == l & head == "Target.non-T3")
    pval = abs(onetime$pstr)
    tdir = onetime$tstr
    if (pval < 0.05 & lastpval > 0.05 ){
      cnum = cnum + 1  # increase cluster number when entering a significant cluster from a non-significant cluster
    }
    if (pval > 0.05 ){  
      cnum = cnum + 1 # increase cluster number when not significant
    }else{
      # if t value flips direction, even if both are signif, 
      # we should treat those as separate clusters
      if (lasttdir*tdir < 0){
        cnum = cnum + 1 
      }
    }
    lastpval = pval
    lasttdir = tdir
    means.df$cnum[means.df$bin == t & means.df$modifier == l] = cnum
  }
  cnum=cnum+1  # new cluster for different languages
}
head(means.df,10)


p = ggplot(means.df , aes( x = bin, y = look, colour=head, label=cnum))
p = p + scale_colour_brewer(palette="Set1")
p = p + scale_fill_brewer(palette="Set2")
p = p + theme_bw()   # change background to white
p = p + ylab("Looks to Target and Competitoer Objects")
p = p + ylim(0,1)
p = p + facet_wrap(~ modifier, ncol=1)
p = p + geom_rect(aes(xmin=bin-wsize, xmax=bin+wsize, ymin = pliney, ymax= pline, colour=NA,fill=plinecol),show.legend=FALSE)
p = p + geom_text(size=2)
p

#significant clusters
meansonlyact.df = subset(means.df, head=="Target.non-T3")
sigcluster = subset(meansonlyact.df, abs(pstr) < 0.05)
print(sigcluster)

sumcluster  = aggregate(tstr ~ cnum + modifier, meansonlyact.df, sum)
print(sumcluster)

n=1000
exptestvalue = data.frame()
for (s in 1:length(sigcluster$bin)){
  cl = sigcluster$cnum[s]
  b = sigcluster$bin[s]
  l = sigcluster$modifier[s]
  print(paste("b ", b, " modifier", l))
  onetime = subset(pdata, modifier== l & bin %in% b)
  randSet = onetime

  for (i in 1:n){
  randSet$chead = sample(randSet$chead, length(randSet$chead))
  randmodel = lm(look ~ chead, randSet)
  t = coef(summary(randmodel))[2,3]
  df = data.frame(t = t, cluster=cl, bin=b, modifier=l, sim=i)
  exptestvalue = rbind(exptestvalue, df)
 }
}
#write.csv(means.df, "means.df.MF02A.TargetCompeditorJunction.non-T3Head.csv", row.names=F)
#means.df <- read.csv("means.df.MF02A.TargetCompeditorJunction.non-T3Head.csv", header =T)


sumt.df = aggregate (t ~ modifier + cluster + sim, exptestvalue, sum)
head(sumt.df)

p = ggplot(sumt.df, aes(x=t))
p = p + geom_histogram(binwidth=0.5)
p = p + facet_wrap(~modifier)
p + theme_bw()

maxclusterdist = data.frame()
for (l in unique(sumt.df$modifier)){
  for (s in unique(sumt.df$sim)) {
    # get all results for one simulation in one language
    onesim = subset(sumt.df,sim == s & modifier == l)
    onesim$astruct = abs(onesim$t)
    # find max t-value
    maxrow = onesim[order(onesim$astruct,decreasing = T),]
    maxclusterdist = rbind(maxclusterdist,maxrow[1,])
  }
}

head(maxclusterdist)

maxclusterdist2 = maxclusterdist[order(maxclusterdist$modifier,maxclusterdist$t),]
end = data.frame(modifier=c("t3","t3","non-t3","non-t3"),xint = maxclusterdist2[c(25,975,1025,1975),]$t)
p = ggplot(maxclusterdist,aes(x = t))
p = p +geom_histogram(binwidth=0.5)
p = p +geom_vline(end,mapping=aes(xintercept=xint))
p = p + facet_wrap(~ modifier)
p+theme_bw()


emaxclusterdist = maxclusterdist$t[1:1000]
jmaxclusterdist = maxclusterdist$t[1001:2000]

for (cl in unique(sumcluster$cnum)){
  bins = unique(means.df[means.df$cnum == cl,]$bin)
  lan = sumcluster$modifier[sumcluster$cnum == cl][1]
  
  tstr = abs(sumcluster$tstr[sumcluster$cnum==cl])
  # permutation t distribution depends on language
  if (lan == "t3"){
    permtdist = emaxclusterdist
  }else{
    permtdist = jmaxclusterdist
  }
  # permtdist is the dist t-values, 
  # so p value is proportion of values greater than observed t-value.
  pstr = sum(abs(permtdist) > tstr, na.rm = TRUE)/length(permtdist)
  if (tstr > 2){
  print(paste("Cluster ",cl," Observed sum t ",round(tstr,3)," Proportion dist > observed (p-value) ",pstr))
  }
  means.df$permtestp[means.df$bin %in% bins & means.df$modifier == lan] = pstr
}


############### now we update our plot
#write.csv(means.df, "means.df.MF02Combined10000.csv", row.names=F)
#means.df <- read.csv("means.df.MF02Combined10000.csv", header =T)

#means.df2<-means.df
#means.df<-means.df2


colnames(means.df)<- c("head","chead", "bin","modifier","look","pstr","tstr","pline","pvalue","cnum","permtestp")
means.df$pvalue <- ifelse(means.df$pvalue=="a",">0.05","<0.05")
means.df$modifier <- ifelse(means.df$modifier=="t3","(1) Modifier t3","(2) Modifier non-t3")
#means.df$head <- ifelse(means.df$head=="T3","(1) T3","(2) non-T3")
means.df$head <- ifelse(means.df$head=="Target.non-T3","(1) Target.non-T3","(2) Competitor.T3")

#p = ggplot(means.df , aes( x = bin, y = look, colour=head))
p = ggplot(means.df , aes( x = bin, y = look, colour=head))
meansigStr = subset(means.df,permtestp < 0.025) 
if (length(meansigStr$bin) > 0){
  p = p + geom_rect(data=meansigStr,aes(xmin=bin-10, xmax=bin+10, ymin = pliney, ymax= 1.0),,colour=NA,fill="grey90",show.legend=FALSE)
}
p = p + geom_line(size=1.5)
p = p + scale_colour_brewer(palette="Set1")
p = p + scale_fill_brewer(palette="Set2")
p = p + scale_linetype_manual(values=c("solid","dashed"))
p = p + theme_bw()   # change background to white
p = p + ylab("Looks to Target + Compeitoer Object ")
p = p + xlab("Time(ms)")
p = p + ylim(0,1)
p = p + facet_wrap(~ modifier, ncol=1)
#p = p + facet_wrap(~ head, ncol=1)
p = p + geom_vline(xintercept = 0,colour="black", linetype = 2)
p = p + geom_vline(xintercept = 1000,colour="black", linetype = 2) 
p = p + geom_vline(xintercept = 2000,colour="black", linetype = 2) 
p = p + geom_vline(xintercept = 3000,colour="black", linetype = 2) 
p = p + geom_vline(xintercept = 4000,colour="black", linetype = 2) 
p = p + geom_rect(aes(xmin=bin-wsize, xmax=bin+wsize, ymin = pliney-0.1, ymax= pline-0.1, colour=NA,fill=pvalue),show.legend=FALSE)
#move the pvalue bar: ymin=pliney-0.1, ymax=pline-0.1
p

#ggsave("g.MF02A.TargetCompeditorJunction.non-T3.png")


########## 4.1 統計検定Test of Significance##########　

data <- read.csv("Output2.csv", header =T)
 
　　　　　　　　　　　　　　　　　　　　#グラフの開始点と関係なし。最初からやり直した方が無難です
head(data)
#summary(data)

                       #データ全体のバランスを確認します
table(data$ParticipantName, data$Condition)
table(data$Condition)

　　　　　　　　　　　　　　　　　　　　　　　#実際のAOIの名前をマッピングします　
data$target <- ifelse(data$AOI == 1, as.character(data$AOI1), "bg")
data$target <- ifelse(data$AOI == 2, as.character(data$AOI2), data$target)
data$target <- ifelse(data$AOI == 3, as.character(data$AOI3), data$target)
data$target <- ifelse(data$AOI == 4, as.character(data$AOI4), data$target)


#data <-data[order(data$ParticipantName,data$itemB,data$gstart),]

##########
　　　　　　　　　　　　　　　　　　　　　　　　#onset時間点：どのonsetから切り揃えるでしょうか
data$slapse <- data$gstart - 0
data$elapse <- data$gend - 0

　　　　　　　　　　　　　　　　　　　　　　　　#例：onset1からでしたら0,
                        #data$slapse <- data$gstart - 0
                        #data$elapse <- data$gend - 0
　　　　　　　　　　　　　　　　　　　　　　　　#onset2からでしたらdata$onset2
　　　　　　　　　　　　　　　　　　　　　　　　#data$slapse <- data$gstart - data$onset2
　　　　　　　　　　　　　　　　　　　　　　　　#data$elapse <- data$gend - data$onset2

　　　　　　　　　　　　　　　　　　　　　　　　#そうしたら、onset以外の部分は負数になって、
　　　　　　　　　　　　　　　　　　　　　　　　#後で時間区間の長さを調整する部分と合わせて0以下は0と入れ替えられて処理しないことになる
　　　　　　　　　　　　　　　　　　　　　　　　#  slapse elapse
　　　　　　　　　　　　　　　　　　　　　　　　#1   -824   -670
　　　　　　　　　　　　　　　　　　　　　　　　#2   -603   -397
　　　　　　　　　　　　　　　　　　　　　　　　#3      3    173
　　　　　　　　　　　　　　　　　　　　　　　　#4    177    680
　　　　　　　　　　　　　　　　　　　　　　　　#5    926   1523
　　　　　　　　　　　　　　　　　　　　　　　　#6   1220   1603

##########
　　　　　　　　　　　　　　　　　　　　　　　　#onset内の時間区間の長さを調整
data$slapse <- ifelse(data$slapse < 1700, 1700, data$slapse)
data$elapse <- ifelse(data$elapse >= 2080, 2080, data$elapse)
　
　　　　　　　　　　　　　　　　　　　　　　　　#例：開始点は0,終了点は1000->0以下は0で入れ替え、1000以上は1000で入れ替えます
　　　　　　　　　　　　　　　　　　　　　　　　#相殺してしまうと、概念上slapseとelapse以外の点が外されてしまします。（すごい技！）
　　　　　　　　　　　　　　　　　　　　　　　　#data$slapse <- ifelse(data$slapse < 0, 0, data$slapse)
　　　　　　　　　　　　　　　　　　　　　　　　#data$elapse <- ifelse(data$elapse >= 1000, 1000, data$elapse)

　　　　　　　　　　　　　　　　　　　　　　　　#  slapse elapse
　　　　　　　　　　　　　　　　　　　　　　　　#1      0      0
　　　　　　　　　　　　　　　　　　　　　　　　#2      0      0
　　　　　　　　　　　　　　　　　　　　　　　　#3      3      3
　　　　　　　　　　　　　　　　　　　　　　　　#4    177    177
　　　　　　　　　　　　　　　　　　　　　　　　#5    926   1000
　　　　　　　　　　　　　　　　　　　　　　　　#6   1220   1000


                        #時間内の比率を見るから、durだけを見ていい（開始点と終了点はもう無視していい）
data$dur <- data$elapse - data$slapse
                        #万が一0以下（オーバーした区間）を除外
data$dur <- ifelse(data$dur < 0, 0, data$dur)

                        #必要なコラムだけを残ります
#data <- data[,c("ParticipantName", "itemB", "target", "cond", "trial", "slapse","elapse","dur")]
data <- data[,c("ParticipantName", "ItemNoB", "target", "Condition", "slapse","elapse","dur")]

data <-data[order(data$ParticipantName,data$ItemNoB,data$slapse),]
                        ########Check##################
                        #data1 <-aggregate(data$dur, by=list(data$ParticipantName,data$itemB,data$cond,data$trial), FUN=sum, na.rm=TRUE)
                        #colnames(data1) = c("subj","item","cond", "trial","sum")
                        #table(data1$cond, data1$subj)
                        ################################

head(data)

                        #CALUCULATING SUM (aggregation for each trial)
                        #同じ区間の中もし別に他の注視時間があれば全部合算する。
data <-aggregate(data$dur, by=list(data$ParticipantName,data$ItemNoB,data$target,data$Condition), FUN=sum, na.rm=TRUE)
#colnames(data) = c("subj","item","AOI","cond", "trial","sum")
colnames(data) = c("subj","item","AOI","cond", "sum")
                        #sort
#data <- data[order(data$subj, data$trial),]

                        #"variable","value"に書き換えないと、cast()が実行できません
#colnames(data) = c("subj","item","variable","cond","trial","value")
colnames(data) = c("subj","item","variable","cond","value")
                        #cast creates separate columns for each object fixated
                        #各絵に分けたら、それぞれの絵にどれくらい見ているのか、後で分けて計算しやすい。（いちいち取り出すではなく、コラム単位で計算できる）
data2 <- cast(data)

                        #replace NULL 
data2$bg <- ifelse(is.na(data2$bg), 0, data2$bg)
data2$Target <- ifelse(is.na(data2$Target), 0, data2$Target)
data2$CompetitorA <- ifelse(is.na(data2$CompetitorA), 0, data2$CompetitorA)
data2$CompetitorB <- ifelse(is.na(data2$CompetitorB), 0, data2$CompetitorB)
data2$CompetitorC <- ifelse(is.na(data2$CompetitorC), 0, data2$CompetitorC)


                        #calculate ALL column
data2$all <- data2$bg + data2$Target + data2$CompetitorA + data2$CompetitorB + data2$CompetitorC
#data2$all <- data2$bg + data2$CompetitorCompound + data2$filler + data2$IrrelevantCompoundA + data2$IrrelevantCompoundB + data2$IrrelevantSimplex + data2$SingleHead + data2$TargetCompound + data2$TargetSimplex

                        ###########
                        #検証したい絵の条件はここで変更
　　　　　　　　　　　　　　　　　　　　　　　　#条件は一つの絵のみの場合
　　　　　　　　　　　　　　　　　　　　　　　　#data2$logit <- log((data2$TargetSimplex + 0.5) / (data2$all - data2$TargetSimplex + 0.5))
　　　　　　　　　　　　　　　　　　　　　　　　#条件はCompetitorCompound+TargetCompoundの場合
　　　　　　　　　　　　　　　　　　　　　　　　#data2$Competitor_TargetCompound<-data2$CompetitorCompound+data2$TargetCompound
　　　　　　　　　　　　　　　　　　　　　　　　#data2$logit <- log((data2$Competitor_TargetCompound + 0.5) / (data2$all - data2$Competitor_TargetCompound + 0.5))
　　　　　　　　　　　　　　　　　　　　　　　　############

data2$Combined <- data2$Target + data2$CompetitorA
#data2$Competitor_TargetCompound<-data2$CompetitorCompound+data2$TargetCompound
                        #基本四つのパタン.odds率を計算。0.5は微調整（分母が0でしたら計算できなくなります。）

data2$logit1 <- log((data2$Combined + 0.5) / (data2$all - data2$Combined + 0.5))
data2$logit2 <- log((data2$Target + 0.5) / (data2$all - data2$Target + 0.5))

　　　　　　　　　　　　
#下位条件を付けます
data2$tone3 <- ifelse(data2$cond == "a" | data2$cond == "b",1,0)
data2$comp <- ifelse(data2$cond == "a" | data2$cond == "c",1,0)

                        #中心化
data2$tone3 <- scale(data2$tone3, scale=F)
data2$comp <- scale(data2$comp, scale=F)
                        

                        #転換しやすいため
                        #logit1:Competitor_TargetCompound
                        #logit2:TargetCompound
                        #logit3:CompetitorCompound
                        #logit4:TargetSimplex

data2$logit<-data2$logit1

tapply(data2$logit, list(data2$tone3, data2$comp), mean)


########################

1.2nd Combined interection
                        #転換しやすいため
                        #logit1:Competitor_TargetCompound
                        #logit2:TargetCompound
                        #logit3:CompetitorCompound
                        #logit4:TargetSimplex

data2$logit<-data2$logit1
tapply(data2$logit, list(data2$tone3, data2$comp), mean)

m10 <- lmer(logit ~ tone3*comp + (1+tone3*comp|subj) + (1+tone3*comp|item), data = data2)
summary(m10)
m9 <- lmer(logit ~ tone3*comp + (1+comp+tone3:comp|subj) + (1+tone3*comp|item), data = data2)
summary(m9)
m8 <- lmer(logit ~ tone3*comp + (1+comp+tone3:comp|subj) + (1+comp+tone3:comp|item), data = data2)
summary(m8)
m7 <- lmer(logit ~ tone3*comp + (1+comp+tone3:comp|subj) + (1+tone3:comp|item), data = data2)
summary(m7)
m6 <- lmer(logit ~ tone3*comp + (1+comp+tone3:comp|subj) + (1|item), data = data2)
summary(m6)
m5 <- lmer(logit ~ tone3*comp + (1+tone3:comp|subj) + (1|item), data = data2)
summary(m5)
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item), data = data2)
summary(m0)

anova(m10,m9,m8,m7,m6,m5,m0)

#### p-value  ########

#Tone main effect
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item), data = data2)
m0i  <-  lmer(logit ~ comp+tone3:comp + (1|subj) + (1|item),, data = data2)
anova(m0, m0i)

#Structure main effect
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item),, data = data2)
m0i  <-  lmer(logit ~ tone3+tone3:comp + (1|subj) + (1|item),, data = data2)
anova(m0, m0i)

#interaction
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item),, data = data2)
m0i  <-  lmer(logit ~ tone3+comp + (1|subj) + (1|item),, data = data2)
anova(m0, m0i)

#########下位検定############
3.2nd combined subeffect test
#compound d b
data2.comp<-data2[data2$cond=="b"|data2$cond=="d",]
summary(data2.comp)
m10 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1+tone3|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ tone3 + (1|subj) + (1+tone3|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ tone3 + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

#simplex a c
data2.comp<-data2[data2$cond=="a"|data2$cond=="c",]
m10 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1+tone3|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ tone3 + (1|subj) + (1+tone3|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ tone3 + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

m0  <-  lmer(logit ~ tone3 + (1|subj) + (1|item), data = data2.comp)
m0i  <-  lmer(logit ~  (1|subj) + (1|item), data = data2.comp)
anova(m0,m0i)

#T3 a b
data2.comp<-data2[data2$cond=="a"|data2$cond=="b",]
head(data2.comp)
m10 <- lmer(logit ~ comp + (1+comp|subj) + (1+comp|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ comp + (1|subj) + (1+comp|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ comp + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

#non-T3 c d
data2.comp<-data2[data2$cond=="c"|data2$cond=="d",]
#
m10 <- lmer(logit ~ comp + (1+comp|subj) + (1+comp|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ comp + (1+comp|subj) + (1|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ comp + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)


##################
##################
##################
4. 2nd Target Compound interection
                        #転換しやすいため
                        #logit1:Competitor_TargetCompound
                        #logit2:TargetCompound
                        #logit3:CompetitorCompound
                        #logit4:TargetSimplex

data2$logit<-data2$logit2
tapply(data2$logit, list(data2$tone3, data2$comp), mean)

m10 <- lmer(logit ~ tone3*comp + (1+tone3*comp|subj) + (1+tone3*comp|item), data = data2)
summary(m10)
m9 <- lmer(logit ~ tone3*comp + (1+tone3+tone3:comp|subj) + (1+tone3*comp|item), data = data2)
summary(m9)
m8 <- lmer(logit ~ tone3*comp + (1+tone3:comp|subj) + (1+tone3*comp|item), data = data2)
summary(m8)
m7 <- lmer(logit ~ tone3*comp + (1+tone3:comp|subj) + (1+tone3+comp|item), data = data2)
summary(m7)
m6 <- lmer(logit ~ tone3*comp + (1+tone3:comp|subj) + (1+tone3|item), data = data2)
summary(m6)
m5 <- lmer(logit ~ tone3*comp + (1+tone3:comp|subj) + (1|item), data = data2)
summary(m5)
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item), data = data2)
summary(m0)

anova(m10,m9,m8,m7,m6,m5,m0)

#### p-value  ########

#Tone main effect
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item), data = data2)
m0i  <-  lmer(logit ~ comp+tone3:comp + (1|subj) + (1|item), data = data2)
anova(m0, m0i)

#Structure main effect
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item), data = data2)
m0i  <-  lmer(logit ~ tone3+tone3:comp + (1|subj) + (1|item), data = data2)
anova(m0, m0i)

#interaction
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item), data = data2)
m0i  <-  lmer(logit ~ tone3+comp + (1|subj) + (1|item), data = data2)

anova(m0, m0i)


#########下位検定############
6. 2nd Target Compound sub effect test
#compound d b
data2.comp<-data2[data2$cond=="b"|data2$cond=="d",]
m10 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1+tone3|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ tone3 + (1|subj) + (1+tone3|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ tone3 + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

#simplex a c
data2.comp<-data2[data2$cond=="a"|data2$cond=="c",]
m10 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1+tone3|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ tone3 + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

#T3 a b
data2.comp<-data2[data2$cond=="a"|data2$cond=="b",]
head(data2.comp)
#
m10 <- lmer(logit ~ comp + (1+comp|subj) + (1+comp|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ comp + (1|subj) + (1+comp|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ comp + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

#non-T3 c d
data2.comp<-data2[data2$cond=="c"|data2$cond=="d",]
#
m10 <- lmer(logit ~ comp + (1+comp|subj) + (1+comp|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ comp + (1|subj) + (1+comp|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ comp + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

##################

#######################

The 3rd syllable

#######################
########################

1.3rd Combined interection
                        #転換しやすいため
                        #logit1:Competitor_TargetCompound
                        #logit2:TargetCompound
                        #logit3:CompetitorCompound
                        #logit4:TargetSimplex

data2$logit<-data2$logit1
tapply(data2$logit, list(data2$tone3, data2$comp), mean)

m10 <- lmer(logit ~ tone3*comp + (1+tone3*comp|subj) + (1+tone3*comp|item), data = data2)
summary(m10)
m9 <- lmer(logit ~ tone3*comp + (1+tone3+tone3:comp|subj) + (1+tone3*comp|item), data = data2)
summary(m9)
m8 <- lmer(logit ~ tone3*comp + (1+tone3+tone3:comp|subj) + (1+tone3+tone3:comp|item), data = data2)
summary(m8)
m7 <- lmer(logit ~ tone3*comp + (1+tone3+tone3:comp|subj) + (1+tone3:comp|item), data = data2)
summary(m7)
m6 <- lmer(logit ~ tone3*comp + (1+tone3:comp|subj) + (1+tone3:comp|item), data = data2)
summary(m6)
m5 <- lmer(logit ~ tone3*comp + (1+tone3:comp|subj) + (1|item), data = data2)
summary(m5)
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item), data = data2)
summary(m0)

anova(m10,m9,m8,m7,m6,m5,m0)

#### p-value  ########

#Tone main effect
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item), data = data2)
m0i  <-  lmer(logit ~ comp+tone3:comp + (1|subj) + (1|item), data = data2)
anova(m0, m0i)

#Structure main effect
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item), data = data2)
m0i  <-  lmer(logit ~ tone3+tone3:comp + (1|subj) + (1|item), data = data2)
anova(m0, m0i)

#interaction
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item), data = data2)
m0i  <-  lmer(logit ~ tone3+comp + (1|subj) + (1|item), data = data2)
anova(m0, m0i)

#########下位検定############
3.3rd combined subeffect test
#compound d b
data2.comp<-data2[data2$cond=="b"|data2$cond=="d",]
summary(data2.comp)
m10 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1+tone3|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ tone3 + (1|subj) + (1+tone3|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ tone3 + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

#simplex a c
data2.comp<-data2[data2$cond=="a"|data2$cond=="c",]
m10 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1+tone3|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ tone3 + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

#T3 a b
data2.comp<-data2[data2$cond=="a"|data2$cond=="b",]
head(data2.comp)
m10 <- lmer(logit ~ comp + (1+comp|subj) + (1+comp|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ comp + (1+comp|subj) + (1|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ comp + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

#non-T3 c d
data2.comp<-data2[data2$cond=="c"|data2$cond=="d",]
m10 <- lmer(logit ~ comp + (1+comp|subj) + (1+comp|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ comp + (1+comp|subj) + (1|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ comp + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)


##################
##################
##################
4. 3rd Target Compound interection
                        #転換しやすいため
                        #logit1:Competitor_TargetCompound
                        #logit2:TargetCompound
                        #logit3:CompetitorCompound
                        #logit4:TargetSimplex

data2$logit<-data2$logit2
tapply(data2$logit, list(data2$tone3, data2$comp), mean)

m10 <- lmer(logit ~ tone3*comp + (1+tone3*comp|subj) + (1+tone3*comp|item), data = data2)
summary(m10)
m9 <- lmer(logit ~ tone3*comp + (1+comp+tone3:comp|subj) + (1+tone3*comp|item), data = data2)
summary(m9)
m8 <- lmer(logit ~ tone3*comp + (1+comp|subj) + (1+tone3*comp|item), data = data2)
summary(m8)
m7 <- lmer(logit ~ tone3*comp + (1+comp|subj) + (1+tone3+tone3:comp|item), data = data2)
summary(m7)
m6 <- lmer(logit ~ tone3*comp + (1|subj) + (1+tone3+tone3:comp|item), data = data2)
summary(m6)
m5 <- lmer(logit ~ tone3*comp + (1|subj) + (1+tone3:comp|item), data = data2)
summary(m5)
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item), data = data2)
summary(m0)

anova(m10,m9,m8,m7,m6,m5,m0)

#### p-value  ########

#Tone main effect
m6  <-  lmer(logit ~ tone3*comp + (1|subj) + (1+tone3+tone3:comp|item), data = data2)
m6i  <-  lmer(logit ~ comp+tone3:comp + (1|subj) + (1+tone3+tone3:comp|item), data = data2)
anova(m6, m6i)

#Structure main effect
m6  <-  lmer(logit ~ tone3*comp + (1|subj) + (1+tone3+tone3:comp|item), data = data2)
m6i  <-  lmer(logit ~ tone3+tone3:comp + (1|subj) + (1+tone3+tone3:comp|item), data = data2)
anova(m6, m6i)

#interaction
m6  <-  lmer(logit ~ tone3*comp + (1|subj) + (1+tone3+tone3:comp|item), data = data2)
m6i  <-  lmer(logit ~ tone3+comp + (1|subj) + (1|item), data = data2)
anova(m6, m6i)


#########下位検定############
6. 3rd Target Compound sub effect test
#compound d b
data2.comp<-data2[data2$cond=="b"|data2$cond=="d",]
head(data2.comp)
m10 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1+tone3|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ tone3 + (1|subj) + (1+tone3|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ tone3 + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

#simplex a c
data2.comp<-data2[data2$cond=="a"|data2$cond=="c",]
m10 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1+tone3|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ tone3 + (1|subj) + (1+tone3|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ tone3 + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

#T3 a b
data2.comp<-data2[data2$cond=="a"|data2$cond=="b",]
head(data2.comp)
#
m10 <- lmer(logit ~ comp + (1+comp|subj) + (1+comp|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ comp + (1+comp|subj) + (1|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ comp + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

#non-T3 c d
data2.comp<-data2[data2$cond=="c"|data2$cond=="d",]
#
m10 <- lmer(logit ~ comp + (1+comp|subj) + (1+comp|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ comp + (1+comp|subj) + (1|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ comp + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

##################
#######################
#######################

The 4th syllable

#######################
########################

1.4th Combined interection
                        #転換しやすいため
                        #logit1:Competitor_TargetCompound
                        #logit2:TargetCompound
                        #logit3:CompetitorCompound
                        #logit4:TargetSimplex

data2$logit<-data2$logit1
tapply(data2$logit, list(data2$tone3, data2$comp), mean)

m10 <- lmer(logit ~ tone3*comp + (1+tone3*comp|subj) + (1+tone3*comp|item), data = data2)
summary(m10)
m9 <- lmer(logit ~ tone3*comp + (1+tone+tone3:comp|subj) + (1+tone3*comp|item), data = data2)
summary(m9)
m8 <- lmer(logit ~ tone3*comp + (1+tone3:comp|subj) + (1+tone3*comp|item), data = data2)
summary(m8)
m7 <- lmer(logit ~ tone3*comp + (1|subj) + (1+tone3*comp|item), data = data2)
summary(m7)
m6 <- lmer(logit ~ tone3*comp + (1|subj) + (1+tone3+comp|item), data = data2)
summary(m6)
m5 <- lmer(logit ~ tone3*comp + (1|subj) + (1+comp|item), data = data2)
summary(m5)
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item), data = data2)
summary(m0)

anova(m10,m9,m8,m7,m6,m5,m0)

#### p-value  ########

#Tone main effect
m5  <-  lmer(logit ~ tone3*comp + (1|subj) + (1+comp|item), data = data2)
m5i  <-  lmer(logit ~ comp+tone3:comp + (1|subj) + (1+comp|item), data = data2)
anova(m5, m5i)

#Structure main effect
m5  <-  lmer(logit ~ tone3*comp + (1|subj) + (1+comp|item), data = data2)
m5i  <-  lmer(logit ~ tone3+tone3:comp + (1|subj) + (1+comp|item), data = data2)
anova(m5, m5i)

#interaction
m5  <-  lmer(logit ~ tone3*comp + (1|subj) + (1+comp|item), data = data2)
m5i  <-  lmer(logit ~ tone3+comp + (1|subj) + (1+comp|item), data = data2)
anova(m5, m5i)

#########下位検定############
3.4th combined subeffect test
#compound d b
data2.comp<-data2[data2$cond=="b"|data2$cond=="d",]
summary(data2.comp)
m10 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1+tone3|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ tone3 + (1|subj) + (1+tone3|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ tone3 + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

#simplex a c
data2.comp<-data2[data2$cond=="a"|data2$cond=="c",]
m10 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1+tone3|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ tone3 + (1|subj) + (1+tone3|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ tone3 + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

#T3 a b
data2.comp<-data2[data2$cond=="a"|data2$cond=="b",]
head(data2.comp)
m10 <- lmer(logit ~ comp + (1+comp|subj) + (1+comp|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ comp + (1|subj) + (1+comp|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ comp + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

#non-T3 c d
data2.comp<-data2[data2$cond=="c"|data2$cond=="d",]
m10 <- lmer(logit ~ comp + (1+comp|subj) + (1+comp|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ comp + (1|subj) + (1+comp|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ comp + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)


##################
##################
##################
4. 4th Target Compound interection
                        #転換しやすいため
                        #logit1:Competitor_TargetCompound
                        #logit2:TargetCompound
                        #logit3:CompetitorCompound
                        #logit4:TargetSimplex

data2$logit<-data2$logit2
tapply(data2$logit, list(data2$tone3, data2$comp), mean)

m10 <- lmer(logit ~ tone3*comp + (1+tone3*comp|subj) + (1+tone3*comp|item), data = data2)
summary(m10)
m9 <- lmer(logit ~ tone3*comp + (1+tone3+tone3:comp|subj) + (1+tone3*comp|item), data = data2)
summary(m9)
m8 <- lmer(logit ~ tone3*comp + (1+tone3|subj) + (1+tone3*comp|item), data = data2)
summary(m8)
m7 <- lmer(logit ~ tone3*comp + (1|subj) + (1+tone3*comp|item), data = data2)
summary(m7)
m6 <- lmer(logit ~ tone3*comp + (1|subj) + (1+comp+tone3:comp|item), data = data2)
summary(m6)
m5 <- lmer(logit ~ tone3*comp + (1|subj) + (1+tone3:comp|item), data = data2)
summary(m5)
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item), data = data2)
summary(m0)

anova(m10,m9,m8,m7,m6,m5,m0)

#### p-value  ########

#Tone main effect
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item), data = data2)
m0i  <-  lmer(logit ~ comp+tone3:comp + (1|subj) + (1|item), data = data2)
anova(m0, m0i)

#Structure main effect
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item), data = data2)
m0i  <-  lmer(logit ~ tone3+tone3:comp + (1|subj) + (1|item), data = data2)
anova(m0, m0i)

#interaction
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item), data = data2)
m0i  <-  lmer(logit ~ tone3+comp + (1|subj) + (1|item), data = data2)
anova(m0, m0i)


#########下位検定############
6. 4th Target Compound sub effect test
#compound d b
data2.comp<-data2[data2$cond=="b"|data2$cond=="d",]
head(data2.comp)
m10 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1+tone3|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ tone3 + (1|subj) + (1+tone3|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ tone3 + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

#simplex a c
data2.comp<-data2[data2$cond=="a"|data2$cond=="c",]
m10 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1+tone3|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ tone3 + (1|subj) + (1+tone3|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ tone3 + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

#T3 a b
data2.comp<-data2[data2$cond=="a"|data2$cond=="b",]
head(data2.comp)
#
m10 <- lmer(logit ~ comp + (1+comp|subj) + (1+comp|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ comp + (1|subj) + (1+comp|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ comp + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

#non-T3 c d
data2.comp<-data2[data2$cond=="c"|data2$cond=="d",]
#
m10 <- lmer(logit ~ comp + (1+comp|subj) + (1+comp|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ comp + (1|subj) + (1+comp|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ comp + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

##################
##################
##################
7. 4th Target Simplex interection
                        #転換しやすいため
                        #logit1:Competitor_TargetCompound
                        #logit2:TargetCompound
                        #logit3:CompetitorCompound
                        #logit4:TargetSimplex

data2$logit<-data2$logit4
tapply(data2$logit, list(data2$tone3, data2$comp), mean)

m10 <- lmer(logit ~ tone3*comp + (1+tone3*comp|subj) + (1+tone3*comp|item), data = data2)
summary(m10)
m9 <- lmer(logit ~ tone3*comp + (1+tone3*comp|subj) + (1+tone3+tone3:comp|item), data = data2)
summary(m9)
m8 <- lmer(logit ~ tone3*comp + (1+tone3+tone3:comp|subj) + (1+tone3+tone3:comp|item), data = data2)
summary(m8)
m7 <- lmer(logit ~ tone3*comp + (1+tone3:comp|subj) + (1+tone3+tone3:comp|item), data = data2)
summary(m7)
m6 <- lmer(logit ~ tone3*comp + (1|subj) + (1+tone3+tone3:comp|item), data = data2)
summary(m6)
m5 <- lmer(logit ~ tone3*comp + (1|subj) + (1+tone3:comp|item), data = data2)
summary(m5)
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item), data = data2)
summary(m0)

anova(m10,m9,m8,m7,m6,m5,m0)

#### p-value  ########

#Tone main effect
m10  <-  lmer(logit ~ tone3*comp + (1+tone3*comp|subj) + (1+tone3*comp|item), data = data2)
m10i  <-  lmer(logit ~ comp+tone3:comp + (1+tone3*comp|subj) + (1+tone3*comp|item), data = data2)
anova(m10, m10i)

#Structure main effect
m10  <-  lmer(logit ~ tone3*comp + (1+tone3*comp|subj) + (1+tone3*comp|item), data = data2)
m10i  <-  lmer(logit ~ tone3+tone3:comp + (1+tone3*comp|subj) + (1+tone3*comp|item), data = data2)
anova(m10, m10i)

#interaction
m10  <-  lmer(logit ~ tone3*comp + (1+tone3*comp|subj) + (1+tone3*comp|item), data = data2)
m10i  <-  lmer(logit ~ tone3+comp + (1+tone3*comp|subj) + (1+tone3*comp|item), data = data2)
anova(m10, m10i)


#########下位検定############
9. 4th Target Compound sub effect test

#compound d b
data2.comp<-data2[data2$cond=="b"|data2$cond=="d",]
m10 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1+tone3|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ tone3 + (1|subj) + (1+tone3|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ tone3 + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

#simplex a c
data2.comp<-data2[data2$cond=="a"|data2$cond=="c",]
m10 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1+tone3|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ tone3 + (1|subj) + (1+tone3|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ tone3 + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

#T3 a b
data2.comp<-data2[data2$cond=="a"|data2$cond=="b",]
head(data2.comp)
#
m10 <- lmer(logit ~ comp + (1+comp|subj) + (1+comp|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ comp + (1|subj) + (1+comp|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ comp + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

#non-T3 c d
data2.comp<-data2[data2$cond=="c"|data2$cond=="d",]
#
m10 <- lmer(logit ~ comp + (1+comp|subj) + (1+comp|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ comp + (1+comp|subj) + (1|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ comp + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

#######################


########## permutation analysis:by head################
###########################################################

dataall <- read.csv("Output3.csv", header =T)
head(dataall)

data <- aggregate(dataall$Target, by=list(dataall$subj, dataall$item, dataall$cond, dataall$bin), mean)
colnames(data) <- c("subj", "item", "cond","bin","look")
data <- data[order(data$subj, data$item, data$cond, data$bin),]
head(data)
data$modifier <- ifelse(data$cond=="a" | data$cond=="b","t3","non-t3")
data$head <- ifelse(data$cond=="a" | data$cond=="c","T3","non-T3")

means.df <- aggregate(look ~ bin + modifier + head, data, mean)
head(means.df)
means.df$modifier <- as.factor(means.df$modifier)
means.df$head <- as.factor(means.df$head)
summary(means.df)

#draw the basic graph
p = ggplot(means.df, aes(x=bin, y=look, colour=modifier))
p = p + geom_line()
p = p + scale_colour_brewer(palette="Set1")
p = p + theme_bw()
p = p + ylab("Looks to Target and Competitor")
p = p + ylim(0,1)
p = p + facet_wrap(~head, ncol=1)
p = p + geom_vline(xintercept = 1000, colour="black", linetype = 2)
p = p + geom_vline(xintercept = 2000, colour="black", linetype = 2)
p = p + geom_vline(xintercept = 3000, colour="black", linetype = 2)
p

#do a standard mixed model analysis using 500 ms windows

data$win = as.integer(-0.001 + data$bin/500)
head(data)
subjmeans.df = aggregate(look ~ win + modifier + head + subj, data, mean)
subjmeans.df$cwin = subjmeans.df$win-mean(subjmeans.df$win)
subjmeans.df$cmodifier = ifelse(subjmeans.df$modifier =="t3", 0.5, -0.5)
subjmeans.df$chead = ifelse(subjmeans.df$head == "T3", 0.5, -0.5)
model = lmer(look ~cwin*cmodifier*chead + (1|subj), subjmeans.df)
print(summary(model))
 
#creat copy of data frame
pdata=data
pdata$chead = ifelse(pdata$head=="T3", 0.5, -0.5)
pdata$cmodifier = ifelse(pdata$modifier=="t3", 0.5, -0.5)

#creat data frame which averages over subjects.
#This also stores the results of the permutation analysis
#means.df1 = aggregate (look ~ head + chead + bin + modifier, pdata, mean)
means.df = aggregate (look ~ head + bin + modifier, pdata, mean)
means.df$pstr = 1000

#permutation analysis uses simple tests to find significant regions.
#Here is a regression on target based on structure for one time window (400ms) in T3 modifier
onetime = subset(pdata, bin==2000 & head == "non-T3")

#do regression model on target using structure condition
onemodel = lm(look ~ cmodifier, onetime)
print(summary(onemodel))
print(abs(coef(summary(onemodel))[2,4]))



#we do this for each 20ms window in the data
timelist = unique(pdata$bin)

#l="t3"
#t=500 #for check one trial

for (l in c("T3","non-T3")){
  for (t in timelist){
   #create data frame for ONE timebin for each modifier
   onetime = subset(pdata, bin == t & head == l)
   #do regression model on target using head condition
   onemodel = lm (look ~ cmodifier, onetime)
   #print(summary(onemodel))
   #this is the t-value for structure
   targetT = coef(summary(onemodel))[2,3] #observed t-value
   targetP = abs(coef(summary(onemodel))[2,4]) #observed p-value
   means.df$tstr[means.df$bin == t & means.df$head == l] = targetT
   means.df$pstr[means.df$bin == t & means.df$head == l] = targetP
 }
}

#to see these p-values, we draw them arbitrarily on the graph at 0.2
#when the p-value < 0.05, we draw a blue line above 0.2
#when the p-value > 0.05, we draw an orange line below 0.2

wsize = 8
pliney = 0.2
plinemax = 0.1
means.df$pline = pliney+plinemax*(0.05-means.df$pstr)
means.df$plinecol = ifelse(means.df$pstr < 0.05,"a","b")

p = ggplot(means.df , aes( x = bin, y = look, colour=modifier))
p = p + geom_line()
p = p + scale_colour_brewer(palette="Set1")
p = p + scale_fill_brewer(palette="Set2")
p = p + theme_bw()   # change background to white
p = p + ylab("Looks to passive match")
p = p + ylim(0,1)
p = p + facet_wrap(~ head, ncol=1)
p = p + geom_rect(aes(xmin=bin-wsize, xmax=bin+wsize, ymin = pliney, ymax= pline, colour=NA,fill=plinecol),show.legend=FALSE)
p

#cluster
cnum = 1
lastpval = 100
means.df$cnum = 1
for (l in c("T3","non-T3")){
  for (t in timelist){
    onetime = subset(means.df,bin == t & head == l & modifier == "t3")
    pval = abs(onetime$pstr)
    tdir = onetime$tstr
    if (pval < 0.05 & lastpval > 0.05 ){
      cnum = cnum + 1  # increase cluster number when entering a significant cluster from a non-significant cluster
    }
    if (pval > 0.05 ){  
      cnum = cnum + 1 # increase cluster number when not significant
    }else{
      # if t value flips direction, even if both are signif, 
      # we should treat those as separate clusters
      if (lasttdir*tdir < 0){
        cnum = cnum + 1 
      }
    }
    lastpval = pval
    lasttdir = tdir
    means.df$cnum[means.df$bin == t & means.df$head == l] = cnum
  }
  cnum=cnum+1  # new cluster for different languages
}
head(means.df,10)


p = ggplot(means.df , aes( x = bin, y = look, colour=modifier, label=cnum))
p = p + scale_colour_brewer(palette="Set1")
p = p + scale_fill_brewer(palette="Set2")
p = p + theme_bw()   # change background to white
p = p + ylab("Looks to Target and Competitoer Objects")
p = p + ylim(0,1)
p = p + facet_wrap(~ head, ncol=1)
p = p + geom_rect(aes(xmin=bin-wsize, xmax=bin+wsize, ymin = pliney, ymax= pline, colour=NA,fill=plinecol),show.legend=FALSE)
p = p + geom_text(size=2)
p

#significant clusters
meansonlyact.df = subset(means.df, modifier=="t3")
sigcluster = subset(meansonlyact.df, abs(pstr) < 0.05)
print(sigcluster)

sumcluster  = aggregate(tstr ~ cnum + head, meansonlyact.df, sum)
print(sumcluster)

n=1000
exptestvalue = data.frame()
for (s in 1:length(sigcluster$bin)){
  cl = sigcluster$cnum[s]
  b = sigcluster$bin[s]
  l = sigcluster$head[s]
  print(paste("b ", b, " head", l))
  onetime = subset(pdata, head== l & bin %in% b)
  randSet = onetime

  for (i in 1:n){
  randSet$cmodifier = sample(randSet$cmodifier, length(randSet$cmodifier))
  randmodel = lm(look ~ cmodifier, randSet)
  t = coef(summary(randmodel))[2,3]
  df = data.frame(t = t, cluster=cl, bin=b, head=l, sim=i)
  exptestvalue = rbind(exptestvalue, df)
 }
}

sumt.df = aggregate (t ~ head + cluster + sim, exptestvalue, sum)
head(sumt.df)

p = ggplot(sumt.df, aes(x=t))
p = p + geom_histogram(binwidth=0.5)
p = p + facet_wrap(~head)
p + theme_bw()

maxclusterdist = data.frame()
for (l in unique(sumt.df$head)){
  for (s in unique(sumt.df$sim)) {
    # get all results for one simulation in one language
    onesim = subset(sumt.df,sim == s & head == l)
    onesim$astruct = abs(onesim$t)
    # find max t-value
    maxrow = onesim[order(onesim$astruct,decreasing = T),]
    maxclusterdist = rbind(maxclusterdist,maxrow[1,])
  }
}

head(maxclusterdist)

maxclusterdist2 = maxclusterdist[order(maxclusterdist$head,maxclusterdist$t),]
end = data.frame(head=c("T3","T3","non-T3","non-T3"),xint = maxclusterdist2[c(25,975,1025,1975),]$t)
p = ggplot(maxclusterdist,aes(x = t))
p = p +geom_histogram(binwidth=0.5)
p = p +geom_vline(end,mapping=aes(xintercept=xint))
p = p + facet_wrap(~ head)
p+theme_bw()


emaxclusterdist = maxclusterdist$t[1:1000]
jmaxclusterdist = maxclusterdist$t[1001:2000]

for (cl in unique(sumcluster$cnum)){
  bins = unique(means.df[means.df$cnum == cl,]$bin)
  lan = sumcluster$head[sumcluster$cnum == cl][1]
  
  tstr = abs(sumcluster$tstr[sumcluster$cnum==cl])
  # permutation t distribution depends on language
  if (lan == "T3"){
    permtdist = emaxclusterdist
  }else{
    permtdist = jmaxclusterdist
  }
  # permtdist is the dist t-values, 
  # so p value is proportion of values greater than observed t-value.
  pstr = sum(abs(permtdist) > tstr, na.rm = TRUE)/length(permtdist)
  if (tstr > 2){
  print(paste("Cluster ",cl," Observed sum t ",round(tstr,3)," Proportion dist > observed (p-value) ",pstr))
  }
  means.df$permtestp[means.df$bin %in% bins & means.df$head == lan] = pstr
}


############### now we update our plot
write.csv(means.df, "means.df.MF02A_TargetbyHead.csv", row.names=F)
means.df <- read.csv("means.df.MF02A_TargetbyHead.csv", header =T)

#means.df2<-means.df
#means.df<-means.df2


colnames(means.df)<- c("head","bin","modifier","look","pstr","tstr","pline","pvalue","cnum","permtestp")
means.df$pvalue <- ifelse(means.df$pvalue=="a",">0.05","=<0.05")
means.df$modifier <- ifelse(means.df$modifier=="t3","(1) Modifier t3","(2) Modifier non-t3")
means.df$head <- ifelse(means.df$head=="T3","(1)Head T3","(2)Head non-T3")

#p = ggplot(means.df , aes( x = bin, y = look, colour=head))
p = ggplot(means.df , aes( x = bin, y = look, colour=modifier))
meansigStr = subset(means.df,permtestp < 0.025) 
if (length(meansigStr$bin) > 0){
  p = p + geom_rect(data=meansigStr,aes(xmin=bin-10, xmax=bin+10, ymin = pliney, ymax= 1.0),,colour=NA,fill="grey90",show.legend=FALSE)
}
p = p + geom_line(size=1.5)
p = p + scale_colour_brewer(palette="Set1")
p = p + scale_fill_brewer(palette="Set2")
p = p + scale_linetype_manual(values=c("solid","dashed"))
p = p + theme_bw()   # change background to white
p = p + ylab("Looks to Target + Compeitoer Object ")
p = p + xlab("Time(ms)")
p = p + ylim(0,1)
#p = p + facet_wrap(~ modifier, ncol=1)
p = p + facet_wrap(~ head, ncol=1)
p = p + geom_vline(xintercept = 0,colour="black", linetype = 2)
p = p + geom_vline(xintercept = 1000,colour="black", linetype = 2) 
p = p + geom_vline(xintercept = 2000,colour="black", linetype = 2) 
p = p + geom_vline(xintercept = 3000,colour="black", linetype = 2) 
p = p + geom_vline(xintercept = 4000,colour="black", linetype = 2) 
p = p + geom_rect(aes(xmin=bin-wsize, xmax=bin+wsize, ymin = pliney-0.1, ymax= pline-0.1, colour=NA,fill=pvalue),show.legend=FALSE)
#move the pvalue bar: ymin=pliney-0.1, ymax=pline-0.1
p

ggsave("g.MF02A.Target.byHead.png")


#######################

2420-2980 ms

#######################
########################

#1.2420-2980ms Combined interection
#2.2420-2980ms Target
#3.3600-3820ms Combined interection
#4.3600-3820ms Target

                        #転換しやすいため
                        #logit1:Competitor_TargetCompound
                        #logit2:TargetCompound
                        #logit3:CompetitorCompound
                        #logit4:TargetSimplex

data2$logit<-data2$logit1
tapply(data2$logit, list(data2$tone3, data2$comp), mean)

m10 <- lmer(logit ~ tone3*comp + (1+tone3*comp|subj) + (1+tone3*comp|item), data = data2)
summary(m10)
VarRank(m10)
m9 <- lmer(logit ~ tone3*comp + (1+tone3+tone3:comp|subj) + (1+tone3*comp|item), data = data2)
summary(m9)
VarRank(m9)
m8 <- lmer(logit ~ tone3*comp + (1+tone3+tone3:comp|subj) + (1+tone3+comp|item), data = data2)
summary(m8)
VarRank(m8)
m7 <- lmer(logit ~ tone3*comp + (1+tone3+tone3:comp|subj) + (1+comp|item), data = data2)
summary(m7)
VarRank(m7)
m6 <- lmer(logit ~ tone3*comp + (1+tone3:comp|subj) + (1+comp|item), data = data2)
summary(m6)
VarRank(m6)
m5 <- lmer(logit ~ tone3*comp + (1|subj) + (1+comp|item), data = data2)
summary(m5)
VarRank(m5)
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item), data = data2)
summary(m0)

anova(m10,m9,m8,m7,m6,m5,m0)

#### p-value  ########

#Tone main effect
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item), data = data2)
m0i  <-  lmer(logit ~ comp+tone3:comp + (1|subj) + (1|item), data = data2)
anova(m0, m0i)

#Structure main effect
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item), data = data2)
m0i  <-  lmer(logit ~ tone3+tone3:comp + (1|subj) + (1|item), data = data2)
anova(m0, m0i)

#interaction
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item), data = data2)
m0i  <-  lmer(logit ~ tone3+comp + (1|subj) + (1|item), data = data2)
anova(m0, m0i)

#########下位検定############
combined subeffect test
#compound d b
data2.comp<-data2[data2$cond=="b"|data2$cond=="d",]
#summary(data2.comp)
m10 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1+tone3|item), data = data2.comp)
summary(m10)
VarRank(m10)
m9 <- lmer(logit ~ tone3 + (1|subj) + (1+tone3|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ tone3 + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

m0  <-  lmer(logit ~ tone3 + (1|subj) + (1|item), data = data2.comp)
m0i  <-  lmer(logit ~ 1 + (1|subj) + (1|item), data = data2.comp)
anova(m0, m0i)

#simplex a c
data2.comp<-data2[data2$cond=="a"|data2$cond=="c",]
m10 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1+tone3|item), data = data2.comp)
summary(m10)
VarRank(m10)
m9 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ tone3 + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

m0  <-  lmer(logit ~ tone3 + (1|subj) + (1|item), data = data2.comp)
m0i  <-  lmer(logit ~ 1 + (1|subj) + (1|item), data = data2.comp)
anova(m0, m0i)

#T3 a b
data2.comp<-data2[data2$cond=="a"|data2$cond=="b",]
#head(data2.comp)
m10 <- lmer(logit ~ comp + (1+comp|subj) + (1+comp|item), data = data2.comp)
summary(m10)
VarRank(m10)
m9 <- lmer(logit ~ comp + (1|subj) + (1+comp|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ comp + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

m0  <-  lmer(logit ~ comp + (1|subj) + (1|item), data = data2.comp)
m0i  <-  lmer(logit ~ 1 + (1|subj) + (1|item), data = data2.comp)
anova(m0, m0i)

#non-T3 c d
data2.comp<-data2[data2$cond=="c"|data2$cond=="d",]
m10 <- lmer(logit ~ comp + (1+comp|subj) + (1+comp|item), data = data2.comp)
summary(m10)
VarRank(m10)
m9 <- lmer(logit ~ comp + (1+comp|subj) + (1|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ comp + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

m0  <-  lmer(logit ~ comp + (1|subj) + (1|item), data = data2.comp)
m0i  <-  lmer(logit ~ 1 + (1|subj) + (1|item), data = data2.comp)
anova(m0, m0i)

