
　　　　　　　　　　　　　　　 　#[TobiiDataProcess_v3.2]はTX300で測定した眼球運動のデータを分析するスクリプトです。
　　　　　　　　　　　　　　　　 #新井学さんと中村智栄さんのスクリプトを元にして作られたものです。（一部「MF実験」に合わせて変更したところがあります）
　　　　　　　　　　　　　　　 　#作成中にもお二人に多大な助けをいただいて、大変感謝いたします。

########### 1　基本のデータtrimming#############
########### 1.1 必要なパッケージ#############

　　　　　　　　　　　　　　　 　#lme4にbugが多いため、代わりにlme4.0を使います
　　　　　　　　　　　　　　　 　#ただし、lme4.0はCRANミラーサイトから直接ダウンロードできないため、かわりに下の行を実行してください
　　　　　　　　　　　　　　　 　#lme4.0パッケージはRの最新版ではなく、バージョン3.0.2以下を用いる必要があります。
　　　　　　　　　　　　　　　 　#同じパソコンに違うバージョンのRを(例：v3.0.2と最新版両方をインストール)並行してかまいません。

#install.packages("lme4.0",repos=c("http://lme4.r-forge.r-project.org/repos", getOption("repos")[["CRAN"]]))
 
　　　　　　　　　　　　　　　 　#それでもうまく走らない場合は最新坂の"Matrix"パッケージをインストールしてからもう一度上のコマンドを走らせてみてください
　　　　　　　　　　　　　　　 　#install.packages("Matrix")
　　　　　　　　　　　　　　　 　#"LmerTest"
　　　　　　　　　　　　　　　 　#"car"

library(ggplot2)   #グラフの作成
library(reshape)  　#cast(), melt()などデータの分解しなおし・立てなおし
library(lme4)    #線形混合モデル

numcol = 23        #EprimeからTobiiに送る信号は13項目あります。実験内容に応じて変更する必要があります。
                   #("list", "trial", "itemA", "itemB", "cond", "AOI1", "AOI2", "AOI3", "AOI4", "AOI5", "AOI6", "AOI7", "AOI8"))

　　　　　　　　　　　　　　　　　#Set working directory. 
setwd("C:/Users/TZUYIN/Desktop/temptemp/temp3")
#setwd("I:/Desktop/chen_temp")　　　　
　　　　　　　　　　　　　　　　　#Tobiiには大容量データはDisk[I]に保存されています。DesktopもDisk[I]に属しています。
#setwd("C:/Users/manabu/Dropbox/ChenData/2015-03-24ChenData")
　　　　　　　　　　　　　　　　　#新井さん、広瀬先生とDropbox>>ChenDataを共有しています。


　　　　　　　　　　　　　　　　　#Tobiiからデータを吐き出すには、trialごとに「individual file」に区切ってください。[MF01]では一人に24　filesになります
　　　　　　　　　　　　　　　　　#そうしないとデータが重くて、間違いがあっても見つかりにくくなります。

　　　　　　　　　　　　　　　　　#あとで一遍に処理しやすいため、ファイル名をまずリストに作っておきます。
dataList <- list.files(pattern = "chen_practice_New")
dataList　　　　　　　　  #ファイルの個数、中身を確認
#dataList[1]       #dataListリストの何個目は何ファイル名なのかを確認したい時

dataall <- NULL　　  #処理した全員分のデータを後でループ式で足し続けるため、空白のdataallを作っております。

########## 1.2 確認:fixationのないファイル ##########

　　　　　　　　　　　　　　　　　#一つのtrialの中に、一回もfixation出てこなかった場合(Unspecifiedばかり)データになれないから、
　　　　　　　　　　　　　　　　　#事前に探し出して手動で削除しておきます。

for(n in 1:length(dataList)){
　　　　　　　　　　　　　　　　 #dataListリストの中から一個目のデータから最後のデータまでに対して実行します：   
temp <- read.table(dataList[n], head=T, sep="\t", na.string="NA", encoding="UTF-8")
　　　　　　　　　　　　　　　　 #Tobiiから吐き出されたデータに必ず文字コードをUTF-8に指定しないといけません
　　　　　　　　　　　　　　　　　#そうしないと"Some incorrect multi characters are included"というエラーが出ます。

temp <- temp[temp$GazeEventType == "Fixation",]
　　　　　　　　　　　　　　　　　#Fixationだけの行を取り出します。
nrow(temp)
if (nrow(temp) == 0) {
  print(paste("Bad trial!", dataList[n]))
 }
}
　　　　　　　　　　　　　　　　　#もし取り出されたら行数が0になったら、おかしいデータになってますので、分析対象から除外されます。


########## 1.3 全員分のデータを一つのファイルにまとめる##########
　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　
dataList <- list.files(pattern = "chen_practice_New")
length(dataList)
　　　　　　　　　　　　　　　　　#取り出すファイルがあったかもしれませんので、念のためもう一度ファイル名リストのdataListと作ってファイルの数を確認します。
　　　
for(n in 1:length(dataList)){ 
　　　　　　　　　　　　　　　　　#一個目のファイルから最後のファイルまで  
  print(paste("now access", n))
　　　　　　　　　　　　　　　　　#何個目まで処理するのかを表示されます。（バグが出たらどこで止まっているのかたどり着くことができるから）
　　temp <- read.table(dataList[n], head=T, sep="\t", na.string="NA", encoding="UTF-8")
　　　　　　　　　　　　　　　　　#データの読み込み。"encoding="UTF-8"を入れないと文字コードによるエラーが出ます。
　　　　　　　　　　　　　　　　　#"na.string"というのは、全ての空欄のところNAを挿入します。
　　eventdata <- temp[1,]$StudioEventData  
　　　　　　　　　　　　　　　　　#EprimeからTobiiに送る信号はデータの「StudioEventData」に記録されます。それを取り出しておいて後で区切ります。 

#Delete unnecessary columns; check you only have the right columns.
  temp <- temp[,c("X.U.FEFF.ParticipantName", "SegmentName", "SegmentStart", "SegmentEnd", "SegmentDuration", "RecordingTimestamp", "FixationIndex", "SaccadeIndex", "GazeEventType", "GazeEventDuration", "FixationPointX..MCSpx.", "FixationPointY..MCSpx.", "PupilLeft", "PupilRight")]
　　　　　　　　　　　　　　　#必要なデータだけを取りだします。文字コード変換のせいでParticipantNameはX.U.FEFF.ParticipantNameなどになってます。
  colnames(temp) <- c("ParticipantName", "SegmentName", "SegmentStart", "SegmentEnd", "SegmentDuration", "RecordingTimestamp", "FixationIndex", "SaccadeIndex", "GazeEventType", "GazeEventDuration", "FixationPointX", "FixationPointY", "PupilLeft", "PupilRight")
　　　　　　　　　　　　　　　　　#X.U.FEFF.ParticipantNameなどのコラム名をParticipantNameに書き直します。

  temp$rectime <- temp$RecordingTimestamp - temp$SegmentStart
                 #反応時間の間隔を算出します。（SegmentStartはそのtrialの開始時間点,RecordingTimestampは各recording pointの時間点）
  temp <- temp[,c("ParticipantName", "SegmentName", "FixationIndex", "GazeEventType", "GazeEventDuration", "FixationPointX", "SaccadeIndex", "FixationPointY", "rectime")]
　　　　　　　　　　　　　　　　　#必要なコラムだけを取り出します。（削除したのは：SegmantStart, SegmentEnd, SegmentDuration, RecordingTimestamp, PupilLeft, PupilRight）

　　temp <- temp[temp$GazeEventType != "Unclassified",]
　　　　　　　　　　　　　　　　　#Gazeタイプの中からFixationとSaccadeだけを取り出す。
　　　　　　　　　　　　　　　　　#ここの話はかなり深いです。とりあえずSaccadeIndexもFixationIndexにマークするのに使いますので、下手に消したらFixationの時間間隔が正確にとれることができなくなります

  temp$FixationIndex <- ifelse(is.na(temp$FixationIndex), temp$SaccadeIndex, temp$FixationIndex)
  temp$FixationPointX <- ifelse(is.na(temp$FixationPointX), -1, temp$FixationPointX)
  temp$FixationPointY <- ifelse(is.na(temp$FixationPointY), -1, temp$FixationPointY)
　　　　　　　　　　　　　　　　　#FixationIndexにNAである場合、SaccadeIndexで補います。（あとでまとめる時にちゃんと違うタイプに分類されますので、代入されても構いません。）
  　　　　　　　　　　　　　　　#あとで確認しやすいため、全てのNAを-1に変換します。（-1はNAより目立つ）
　　temp$SaccadeIndex <- NULL
　　　　　　　　　　　　　　　　　#Indexの情報を全てFixationIndexにまとまりましたので、SaccadeIndexは削除して構いません。　　
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
    }            #listの行数はnumcolの倍数のはずです。そうでないと、エラーを出すように設定します。
　　　　　　　　　　　　　　　　　#智栄さんの実験ではnumcolは7だけでしたが、私の[MF01]ではAOI8まで増やすなど、numcolが13になっています。

  mat <- matrix(list, nrow = nrow(temp2), ncol = numcol, byrow=T)
　　　　　　　　　　　　　　　　　#一列しかなかったlistを13個ずつ切って横にして、表にします。
  mat <- as.data.frame(mat)
　　　　　　　　　　　　　　　　　#matをデータフォーム形式にします
  colnames(mat) <- c("cond", "itemA", "itemB", "C1Type", "C2Type1", "C2Type2", "SoundType", "TrialOrFiller", "Picture", "Sound", "CorrectAnswer",    "AOI1", "AOI2", "AOI3", "AOI4", "TypeTag1", "TypeTag2", "TypeTag3", "TypeTag4", "LocationTag1",  "LocationTag2",  "LocationTag3", "LocationTag4")


　　　　　　　　　　　　　　　　　#matにコラム名を付け加えます。
　　temp3 <- cbind(temp2, mat)
　　　　　　　　　　　　　　　　　#ｔemp2(Tobii)とmat(Eprime)を合併します。

  dataall <- rbind(dataall, temp3)
　　　　　　　　　　　　　　　　　#できたデータを一人分一人分でdataallの下から付け加えます。
}

########## 1.4 注視点のXY座標をAOIにマッピングする##########

　　　　　　　　　　　　　　　　　#今まではTobiiのsegement機能で一個一個ドラッグする必要がありましたが、下のスクリプトではそういう必要がなくなります。
dataall$AOI <- ifelse(dataall$FixationPointX >= 245 & dataall$FixationPointX < 725 & dataall$FixationPointY >= 0 & dataall$FixationPointY < 361, 1, 0)
dataall$AOI <- ifelse(dataall$FixationPointX >= 725 & dataall$FixationPointX < 1205 & dataall$FixationPointY >= 0 & dataall$FixationPointY < 361, 2, dataall$AOI)
dataall$AOI <- ifelse(dataall$FixationPointX >= 1205 & dataall$FixationPointX < 1685 & dataall$FixationPointY >= 0 & dataall$FixationPointY < 361, 3, dataall$AOI)
dataall$AOI <- ifelse(dataall$FixationPointX >= 245 & dataall$FixationPointX < 725 & dataall$FixationPointY >= 361 & dataall$FixationPointY < 721, 4, dataall$AOI)
dataall$AOI <- ifelse(dataall$FixationPointX >= 1205 & dataall$FixationPointX < 1685 & dataall$FixationPointY >= 361 & dataall$FixationPointY < 721, 5, dataall$AOI)
dataall$AOI <- ifelse(dataall$FixationPointX >= 245 & dataall$FixationPointX < 725 & dataall$FixationPointY >= 721 & dataall$FixationPointY <= 1080, 6, dataall$AOI)
dataall$AOI <- ifelse(dataall$FixationPointX >= 725 & dataall$FixationPointX < 1205 & dataall$FixationPointY >= 721 & dataall$FixationPointY <= 1080, 7, dataall$AOI)
dataall$AOI <- ifelse(dataall$FixationPointX >= 1205 & dataall$FixationPointX < 1685 & dataall$FixationPointY >= 721 & dataall$FixationPointY <= 1080, 8, dataall$AOI)

dataall <-dataall[dataall$GazeEventType == "Fixation",]
　　　　　　　　　　　　　　　　　#ここまでではSaccadeのデータもいらなくなりました（XY軸の情報がないです）。Fixationだけを残せば大丈夫です。
dataall$GazeEventDuration <- NULL; dataall$StudioEventData <- NULL; dataall$FixationIndex <- NULL; dataall$GazeEventType <- NULL
　　　　　　　　　　　　　　　　　#必要のない情報を削除します。
head(dataall)
table(dataall$ParticipantName, dataall$SegmentName)
　　　　　　　　　　　　　　　　　#データ全体のバランスを確認します。
write.csv(dataall, "Output2.csv", row.names=F)
　　　　　　　　　　　　　　　　　#保存します。

########## 2 音声ファイルのonsetをマッピングする##########

　　　　　　　　　　　　　　　　　#違うonsetからの分析が必要となっていますが、Eprimeを組み立てるとき音声ファイルの情報は入れ忘れましたので、ここで改めてマッピングします。

　　　　　　　　　　　　　　　　　#Eprimeで使われてる4リスト(各trailと音声)
 list <- read.csv("list.csv", head=T, sep=",", na.string="NA", encoding="UTF-8")
　list
 list$OnsetId<-paste(list$ItemNoA,"_",list$Condition,sep="")
 head(list)

　　　　　　　　　　　　　　　　　#praatで作ったonsetファイル（音声ファイルとonset時間）
 onset <- read.csv("onset.csv", head=T, sep=",", na.string="NA", encoding="UTF-8")
 onset
 head(onset)

　　　　　　　　　　　　　　　　　#OnsetとEprimeリストとmergeします
　combine<-merge(list,onset,by.x="Sound",by.y="Word",all.x=TRUE)
 head(combine)
 combine<-combine[order(combine$ListNo,combine$TrailNo),]
 head(combine,50)

　　　　　　　　　　　　　　　　　#PartIでtrimmedされたOutput2.csvを読み込みます 
 dataall <- read.csv("Output2.csv", header =T)
 head(dataall)
 dataall$OnsetId<-paste(dataall$itemA,"_",dataall$cond,sep="")
 head(dataall)
 
　　　　　　　　　　　　　　　　　#combineとOuputとmergeします
 dataall<-merge(dataall,combine,by.x="OnsetId",by.y="OnsetId",all.x=TRUE)
 head(dataall)
 dataall<-dataall[order(dataall$ParticipantName,dataall$SegmentName,dataall$gstart),]
 head(dataall)
####################################################
                   #IrrelevantCompoundを抽出ため
　ISMapping <- read.csv("TargetMapping03.csv", header =T)
 head(ISMapping,30)

　dataall<-merge(dataall,ISMapping,by.x="OnsetId",by.y="OnsetId",all.x=TRUE)
　head(dataall,200)
 dataall<-dataall[order(dataall$ParticipantName,dataall$SegmentName,dataall$gstart),]
 head(dataall)

 colnames(dataall)
　dataall<-dataall[,c("ParticipantName","SegmentName","FixationPointX","FixationPointY","gstart","gend","list","trial","itemB","cond","AOI1.x","AOI2.x","AOI3.x","AOI4.x","AOI5.x","AOI6.x","AOI7.x","AOI8.x","AOI","Sound","onset2","onset3","onset4","onset5","endword5","ICA","ICB")]
 colnames(dataall)<-c("ParticipantName","SegmentName","FixationPointX","FixationPointY","gstart","gend","list","trial","itemB","cond","AOI1","AOI2","AOI3","AOI4","AOI5","AOI6","AOI7","AOI8","AOI","Sound","onset2","onset3","onset4","onset5","endword5","ICA","ICB")
 head(dataall)
                   #vowel[a]+tone1を抽出ため
　SoundMapping <- read.csv("SoundMapping03.csv", header =T)
 head(SoundMapping)
 dataall<-merge(dataall,SoundMapping,by.x="OnsetId",by.y="OnsetId",all.x=TRUE)
 head(dataall,200)
 dataall<-dataall[order(dataall$ParticipantName,dataall$SegmentName,dataall$gstart),]
 head(dataall)

 colnames(dataall)
　dataall<-dataall[,c("ParticipantName","SegmentName","FixationPointX","FixationPointY","gstart","gend","list","trial","itemB","cond","AOI1.x","AOI2.x","AOI3.x","AOI4.x","AOI5.x","AOI6.x","AOI7.x","AOI8.x","AOI","Sound","onset2","onset3","onset4","onset5","endword5","a_1")]
 colnames(dataall)<-c("ParticipantName","SegmentName","FixationPointX","FixationPointY","gstart","gend","list","trial","itemB","cond","AOI1","AOI2","AOI3","AOI4","AOI5","AOI6","AOI7","AOI8","AOI","Sound","onset2","onset3","onset4","onset5","endword5","a_1")
 head(dataall)

######################################################
　　　　　　　　　　　　　　　　　#必要なコラムだけを取り出してコラム名を付け直します。
                   #MF01では提示される絵がbetween designのため（n3,nxと33,3xは全く違う絵）,itemはitemAではなくitemBで統計をかける
colnames(dataall)
dataall<-dataall[,c("ParticipantName","SegmentName","FixationPointX","FixationPointY","gstart","gend","list","trial","itemB","cond","AOI1.x","AOI2.x","AOI3.x","AOI4.x","AOI5.x","AOI6.x","AOI7.x","AOI8.x","AOI","Sound","onset2","onset3","onset4","onset5","endword5")]
 colnames(dataall)<-c("ParticipantName","SegmentName","FixationPointX","FixationPointY","gstart","gend","list","trial","itemB","cond","AOI1","AOI2","AOI3","AOI4","AOI5","AOI6","AOI7","AOI8","AOI","Sound","onset2","onset3","onset4","onset5","endword5")
 head(dataall)
　　　　　　　　　　　　　　　　　#保存します。
 write.csv(dataall, "Output3.csv", row.names=F)



########## 3.1 グラフ作成##########

#dataall <- read.csv("Output2.csv", header =T)
dataall <- read.csv("Output3.trimmed02.csv", header =T)

#dataall<-dataall[dataall$trial>18,]
#dataall<-dataall[dataall$ICA=="IC"|dataall$ICB=="IC",]
#dataall.1<-dataall[dataall$ICA=="IC",]
#dataall.2<-dataall[dataall$ICB=="IC",]
#dataall <- rbind(dataall.1, dataall.2)
#dataall[dataall$ICA=="IC"&dataall$ICB=="IC",]
#dataall$ICA<-NULL;dataall$ICB<-NULL
#dataall<-dataall[dataall$a_1 != 0,]
#dataall$cond<-dataall$a_1;dataall$a_1<-NULL
head(dataall,100)
　　　　　　　　　　　　　　　　　#グラフの全体の時間軸の幅をここで決めます。（複合名詞全体は約8000ms,一文字は約3000ms）
binname <- seq(20,4000,20)
bindata <- matrix(0,nrow = nrow(dataall), ncol = length(binname))
colnames(bindata) <- binname
　　　　　　　　　　　　　　　　　#binnameでまず20~6000msを20ms単位でbinのリストを作っておいて、
　　　　　　　　　　　　　　　　　　　#それをコラム数にしてdataallと同じ行数で行列bindataを作って後でdataallの右側につっくけます。

　　　　　　　　　　　　　　　　　　　#グラフの開始時点を揃う基準となる音声ファイルのonsetはここで決めます。
　　　　　　　　　　　　　　　　　　　#（MF01ではonset3がcritical regionです）
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
gr$target <- ifelse(gr$AOI == 5, as.character(gr$AOI5), gr$target)
gr$target <- ifelse(gr$AOI == 6, as.character(gr$AOI6), gr$target)
gr$target <- ifelse(gr$AOI == 7, as.character(gr$AOI7), gr$target)
gr$target <- ifelse(gr$AOI == 8, as.character(gr$AOI8), gr$target)

　　　　　　　　　　　　　　　　　　　#必要なコラムだけを取り出す
　　　　　　　　　　　　　　　　　　　#participant,cond,itemB,Xtrial,AOI,target,bin20~8000
gr <- gr[,c(1, 9, 8, 18, ncol(gr), 20:(ncol(gr)-1))]
#gr <- gr[,c(1, 7, 9, 18, ncol(gr), 20:(ncol(gr)-1))]

                   #melt time bins into one column.
                   #grを立てにする。t20の時にAOI2に入っているのか。
gr2 <- melt(gr,id=c("ParticipantName","cond","itemB","AOI", "target"))
gr2$variable <- as.numeric(as.character(gr2$variable)) 
                   #melt()かけたら、binがvariable,入っているか(1/0)がvalueになる。

　　　　　　　　　　　　　　　　　#わかりやすいため並べ替えます
gr2 <- gr2[order(gr2$ParticipantName, gr2$itemB),]

                   #aggregate for each trial (participants x items)
gr3 <-aggregate(gr2$value, by=list(gr2$ParticipantName, gr2$itemB, gr2$cond, gr2$AOI, gr2$target, gr2$variable), FUN=sum, na.rm=TRUE)
                   #Tobiiの記録の中で、一定の期間であるAOIが視野に入ったら一カウントされます。
　　　　　　　　　　　　　　　　　#逆にその期間に視野に入ってないbinの部分には０と表記され、AOIが多く重複されています
　　　　　　　　　　　　　　　　　　　#この式は一つのbinの中に重複のないように、AOIをaggregate()で消します。
                   #違う言い方だとすると、違う区間で111が入ったら、同じ行にまとめられます。

colnames(gr3) = c("subj","item","cond","AOI", "variable", "bin","value")
gr3$AOI <- NULL

                   #ただし、最初から最後までも見られたのないAOIはどうしてもカウントされません
　　　　　　　　　　　　　　　　　　　#それがカウントされないと、比率の母数が正しく求められません
　　　　　　　　　　　　　　　　　　　#（例：「6回提示の中にtargetが何回見られた」の中、「6回」が0だとしても正しく求められません）
　　　　　　　　　　　　　　　　　　　#そうすると、もう一回cast()でデータを開いて、targetがNAのところを0に入れ替える
gr.temp <- cast(gr3)
                   #cast()を実行すると、gr3の"variable"と"value"にしたがって自動的に分解してもらえます。
　　　　　　　　　　　　　　　　　　　#(例：variableの"CompetitorCompound"などが横になって、"value"がその下に)
 
                   #一回も出たことないtargetがNAになっているはずです。それを0に書き換えます。
gr.temp$bg<-ifelse(is.na(gr.temp$bg),0,gr.temp$bg)
gr.temp$CompetitorCompound <- ifelse(is.na(gr.temp$CompetitorCompound),0,gr.temp$CompetitorCompound)
gr.temp$filler <- ifelse(is.na(gr.temp$filler), 0, gr.temp$filler)
gr.temp$IrrelevantCompoundA <- ifelse(is.na(gr.temp$IrrelevantCompoundA), 0, gr.temp$IrrelevantCompoundA)
gr.temp$IrrelevantCompoundB <- ifelse(is.na(gr.temp$IrrelevantCompoundB), 0, gr.temp$IrrelevantCompoundB)
gr.temp$IrrelevantSimplex <- ifelse(is.na(gr.temp$IrrelevantSimplex), 0, gr.temp$IrrelevantSimplex)
gr.temp$SingleHead <- ifelse(is.na(gr.temp$SingleHead), 0, gr.temp$SingleHead)
gr.temp$TargetCompound <- ifelse(is.na(gr.temp$TargetCompound), 0, gr.temp$TargetCompound)
gr.temp$TargetSimplex <- ifelse(is.na(gr.temp$TargetSimplex), 0, gr.temp$TargetSimplex)

                   #他に見たい組み合わせも簡単にできる
gr.temp$Combined <- gr.temp$TargetCompound + gr.temp$CompetitorCompound
gr.temp$IrrelevantCompound <- gr.temp$IrrelevantCompoundA + gr.temp$IrrelevantCompoundB

　　　　　　　　　　　　　　　　　　　#(t1) Combined (t2) TargetCompound (t3) CompetitorCompound (t4) TargetSimplex
gr.temp$t1 <- gr.temp$Combined
gr.temp$t2 <- gr.temp$TargetCompound
gr.temp$t3 <- gr.temp$CompetitorCompound
gr.temp$t4 <- gr.temp$TargetSimplex
gr.temp$t5 <- gr.temp$IrrelevantSimplex
gr.temp$t6 <- gr.temp$IrrelevantCompound

write.csv(gr.temp, "gr.temp.csv", row.names=F)

　　　　　　　　　　　　　　　　　　　#aggregate for graph (Use t1~t4)
                   #グラフ作成ためのデータフレーム
gra <- aggregate(gr.temp$t1, by=list(gr.temp$bin, gr.temp$cond),mean)
colnames(gra) <-c("bin","cond","mean")
head(gra)
　　　　　　　　　　　　　　　　　　　#グラフ作成の本番
g.MAPLL.Combind.4colors.all <- ggplot(data=gra, aes(x=bin, y=mean, shape=cond, fill=cond))+ 
  geom_line() +
  geom_point(size=4) +
  scale_shape_manual(values=c(21,25,21,25))+
  scale_fill_manual(values=c("#333333","#333333",NA,NA))+
  #scale_linetype_manual(values=c("solid","dashed","solid","dashed"))+
  scale_x_continuous("Time(ms)") + 
  scale_y_continuous(limits=c(0,1),name="Proportion of looks to target") +
  scale_color_discrete("Condition") +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 16))+
  theme(legend.position=c(1,.5),legend.justification=c(1,0))+
  #labs(shape="Condition",fill="Condition")+
  theme(legend.title=element_text(size=10))+  
  theme(legend.text=element_text(size=12))+
  ggtitle("[Visual] Target Compound + Competitor Compound")
  
　　　　　　　　　　　　　　　　　　　　#必要に応じて変えます　#名付け：g.ALL24.onset3.TCCC/ g.ALL24.onset3.TC/ g.ALL24.onset3.CC/ g.ALL24.onset3.TS
g.MAPLL.Combind.4colors.all

##########color change#############
write.csv(gra, "Output5.csv", row.names=F)
gra<- read.csv("Output5.csv", header =T)

g.MAPLL.Compound.4colors.all <- ggplot(data=gra, aes(x=bin, y=mean, shape=Condition, fill=Condition))+ 
  geom_line() +
  geom_point(size=4) +
  scale_shape_manual(values=c(21,25,21,25))+ #X is no.4
  scale_fill_manual(values=c("#0066CC","#FF0000","#CCFF33","#669900"))+
  #scale_linetype_manual(values=c("solid","dashed","solid","dashed"))+
  scale_x_continuous("Time(ms)") + 
  scale_y_continuous(limits=c(0,1),name="Proportion of looks to target") +
  scale_color_discrete("Condition") +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 16))+
  theme(legend.position=c(1,.5),legend.justification=c(1,.9))+
  #labs(shape="Condition",fill="Condition")+
  theme(legend.title=element_text(size=10))+  
  theme(legend.text=element_text(size=12))+
  ggtitle("[Visual] Target Compound + Competitor Compound")
  
　　　　　　　　　　　　　　　　　　　　#必要に応じて変えます　#名付け：g.ALL24.onset3.TCCC/ g.ALL24.onset3.TC/ g.ALL24.onset3.CC/ g.ALL24.onset3.TS
g.MAPLL.Single.4colors.nonT3


##########ＡＭＬＡＰ change color and point#############

g.AMLAP.Single.4colors.nopoint.all <- ggplot(data=gra, aes(x=bin, y=mean, colour= Condition))+ 
  geom_line(aes(linetype=Condition, size=Condition)) +
  #geom_point() +
  #scale_shape_manual(values=c(21,25,21,25))+ #X is no.4
  #scale_fill_manual(values=c("#0066CC","#FF0000","#CCFF33","#669900"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))+
scale_colour_manual(values=c("#990066","#FF0000","#339900", "#006600"))+
  scale_size_manual(values=c(1, 1,1,1))+
  scale_x_continuous("Time(ms)") + 
  scale_y_continuous(limits=c(0,1),name="Proportion of looks to target") +
  #scale_color_discrete("Condition") +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 16))+
  theme(legend.position=c(1,.5),legend.justification=c(1,.9))+
  #labs(shape="Condition",fill="Condition")+
  theme(legend.title=element_text(size=10))+  
  theme(legend.text=element_text(size=12))+
  ggtitle("[Visual] Target Single")
  
g.AMLAP.Single.4colors.nopoint.all

########## 3.2 グラフ保存##########　　　　　　　　　　　　　　　　　　　
####一つの図で保存 library(gridExtra)
ppi<-300　　　　　　　　　　　　#画質をscreen上きれいに映すようにpixel=300ppiに設定
png("g.AMLAP.Single.4colors.nopoint.all.png",width=12*ppi,height=6*ppi,res=ppi)
　　　　　　　　　　　　　　　　　　　　#png()でpng拡張子のグラフで外部保存します。ppiに応じて長さと幅の設定は　12*6*ppi
g.AMLAP.Single.4colors.nopoint.all 
dev.off()           
#もう一度グラフのオブジェクトを読み込みます#dev.off()で閉じまいとファイルが保存されません　　　　　　　　　　　
####複数の図の一覧表を保存

#ALL24
library(gridExtra)　　#パッケージのggplot2ではなく、別にgridExtraが必要です。
ppi<-300
png("TobiiEachParticipant.ALL24.LatterHalf.onset4.png",width=24*ppi, height=12*ppi,res=ppi)
                    #幅は一個のグラフの2倍の24*ppiになります
graph<-grid.arrange(g.ALL24.LatterHalf.onset4.TCCC,g.ALL24.LatterHalf.onset4.TC,g.ALL24.LatterHalf.onset4.CC,g.ALL24.LatterHalf.onset4.TS,ncol=2)
                    #grid.arrange()でR内でできたグラフのオブジェクトをまとめます。（外部のpngファイルではなく）
dev.off()

#################permutation analysis#####################
dataall <- read.csv("gr.temp.csv", header =T)

head(dataall)

data <- aggregate(dataall$Combined, by=list(dataall$subj, dataall$item, dataall$cond, dataall$bin), mean)
colnames(data) <- c("subj", "item", "cond","bin","look")
data <- data[order(data$subj, data$item, data$cond, data$bin),]
head(data)
data$modifier <- ifelse(data$cond=="a" | data$cond=="b","t3","non-t3")
data$head <- ifelse(data$cond=="a" | data$cond=="c","T3","non-T3")

means.df <- aggregate(look ~ bin + modifier + head, data, mean)
head(means.df)

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
subjmeans.df$chead = ifelse(subjmeans.df$head == "T3", 0.5, -0.5)
model = lmer(look ~cwin*cmodifier*chead + (1|subj), subjmeans.df)
print(summary(model))
 
#creat copy of data frame
pdata=data
pdata$chead = ifelse(pdata$head=="T3", 0.5, -0.5)

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
    onetime = subset(means.df,bin == t & modifier == l & head == "T3")
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
meansonlyact.df = subset(means.df, head=="T3")
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
write.csv(means.df, "means.df.MF02B.Combined.csv", row.names=F)
means.df <- read.csv("means.df.MF02B.Combined.csv", header =T)

means.df2<-means.df
means.df<-means.df2


colnames(means.df)<- c("head","chead", "bin","modifier","look","pstr","tstr","pline","pvalue","cnum","permtestp")
means.df$pvalue <- ifelse(means.df$pvalue=="a",">0.05","<=0.05")
means.df$tone <- ifelse(means.df$modifier=="t3","(1) C1 T3","(2) C1 non-T3")
means.df$structure <- ifelse(means.df$head=="T3","(1) single","(2) compound")



p = ggplot(means.df , aes( x = bin, y = look, colour=structure))
meansigStr = subset(means.df,permtestp < 0.025) 
if (length(meansigStr$bin) > 0){
  p = p + geom_rect(data=meansigStr,aes(xmin=bin-10, xmax=bin+10, ymin = pliney-0.1, ymax= 1.0),,colour=NA,fill="grey90",show.legend=FALSE)
}
p = p + geom_line(size=1.3,aes(linetype=structure))
p = p + scale_linetype_manual(values=c("twodash", "solid"))
#p = p + scale_colour_brewer(palette="Set1") #for line
p = p + scale_color_manual(values=c("#0066CC", "#CC3300"))#for line
p = p + scale_fill_brewer(palette="Set2") #for bar
#p = p + scale_fill_manual(values=c("#99CC99", "#FF3300"))#for bar
p = p + theme_bw()   # change background to white
p = p + ylab("Looks to Target Compound + Compeitoer Compound Object ")
p = p + xlab("Time(ms)")
p = p + ylim(0,1)
p = p + facet_wrap(~ tone, ncol=1)
p = p + geom_vline(xintercept = 0,colour="black", linetype = 2)
p = p + geom_vline(xintercept = 1000,colour="black", linetype = 2) 
p = p + geom_vline(xintercept = 2000,colour="black", linetype = 2) 
p = p + geom_vline(xintercept = 3000,colour="black", linetype = 2) 
p = p + geom_vline(xintercept = 4000,colour="black", linetype = 2) 
p = p + geom_rect(aes(xmin=bin-wsize, xmax=bin+wsize, ymin = pliney-0.1, ymax= pline-0.1, colour=NA,fill=pvalue),show.legend=FALSE)
#move the pvalue bar: ymin=pliney-0.1, ymax=pline-0.1
p

ggsave("g.MF02B.Combined.permutation.png")

########## 4.1 統計検定Test of Significance##########　

data <- read.csv("Output3.trimmed02.csv", header =T) 
　　　　　　　　　　　　　　　　　　　　#グラフの開始点と関係なし。最初からやり直した方が無難です
head(data)
#summary(data)

                       #データ全体のバランスを確認します
table(data$ParticipantName, data$cond)
table(data$cond)

　　　　　　　　　　　　　　　　　　　　　　　#実際のAOIの名前をマッピングします　
data$target <- ifelse(data$AOI == 1, as.character(data$AOI1), "bg")
data$target <- ifelse(data$AOI == 2, as.character(data$AOI2), data$target)
data$target <- ifelse(data$AOI == 3, as.character(data$AOI3), data$target)
data$target <- ifelse(data$AOI == 4, as.character(data$AOI4), data$target)
data$target <- ifelse(data$AOI == 5, as.character(data$AOI5), data$target)
data$target <- ifelse(data$AOI == 6, as.character(data$AOI6), data$target)
data$target <- ifelse(data$AOI == 7, as.character(data$AOI7), data$target)
data$target <- ifelse(data$AOI == 8, as.character(data$AOI8), data$target)

data <-data[order(data$ParticipantName,data$itemB,data$gstart),]

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
data$slapse <- ifelse(data$slapse < 3000, 3000, data$slapse)
data$elapse <- ifelse(data$elapse >= 4000, 4000, data$elapse)
　
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
data <- data[,c("ParticipantName", "itemB", "target", "cond",  "slapse","elapse","dur")]
data <-data[order(data$ParticipantName,data$itemB,data$slapse),]
                        ########Check##################
                        #data1 <-aggregate(data$dur, by=list(data$ParticipantName,data$itemB,data$cond,data$trial), FUN=sum, na.rm=TRUE)
                        #colnames(data1) = c("subj","item","cond", "trial","sum")
                        #table(data1$cond, data1$subj)
                        ################################

head(data)

                        #CALUCULATING SUM (aggregation for each trial)
                        #同じ区間の中もし別に他の注視時間があれば全部合算する。
data <-aggregate(data$dur, by=list(data$ParticipantName,data$itemB,data$target,data$cond), FUN=sum, na.rm=TRUE)
colnames(data) = c("subj","item","AOI","cond","sum")

                        #sort
data <- data[order(data$subj),]
                        #"variable","value"に書き換えないと、cast()が実行できません
colnames(data) = c("subj","item","variable","cond","value")

                        #cast creates separate columns for each object fixated
                        #各絵に分けたら、それぞれの絵にどれくらい見ているのか、後で分けて計算しやすい。（いちいち取り出すではなく、コラム単位で計算できる）
data2 <- cast(data)

                        #replace NULL 
data2$bg <- ifelse(is.na(data2$bg), 0, data2$bg)
data2$CompetitorCompound <- ifelse(is.na(data2$CompetitorCompound), 0, data2$CompetitorCompound)
data2$filler <- ifelse(is.na(data2$filler), 0, data2$filler)
data2$IrrelevantCompoundA <- ifelse(is.na(data2$IrrelevantCompoundA), 0, data2$IrrelevantCompoundA)
data2$IrrelevantCompoundB <- ifelse(is.na(data2$IrrelevantCompoundB), 0, data2$IrrelevantCompoundB)
data2$IrrelevantSimplex <- ifelse(is.na(data2$IrrelevantSimplex), 0, data2$IrrelevantSimplex)
data2$SingleHead <- ifelse(is.na(data2$SingleHead), 0, data2$SingleHead)
data2$TargetCompound <- ifelse(is.na(data2$TargetCompound), 0, data2$TargetCompound)
data2$TargetSimplex <- ifelse(is.na(data2$TargetSimplex), 0, data2$TargetSimplex)

                        #calculate ALL column
data2$all <- data2$bg + data2$CompetitorCompound + data2$filler + data2$IrrelevantCompoundA + data2$IrrelevantCompoundB + data2$IrrelevantSimplex + data2$SingleHead + data2$TargetCompound + data2$TargetSimplex

                        ###########
                        #検証したい絵の条件はここで変更
　　　　　　　　　　　　　　　　　　　　　　　　#条件は一つの絵のみの場合
　　　　　　　　　　　　　　　　　　　　　　　　#data2$logit <- log((data2$TargetSimplex + 0.5) / (data2$all - data2$TargetSimplex + 0.5))
　　　　　　　　　　　　　　　　　　　　　　　　#条件はCompetitorCompound+TargetCompoundの場合
　　　　　　　　　　　　　　　　　　　　　　　　#data2$Competitor_TargetCompound<-data2$CompetitorCompound+data2$TargetCompound
　　　　　　　　　　　　　　　　　　　　　　　　#data2$logit <- log((data2$Competitor_TargetCompound + 0.5) / (data2$all - data2$Competitor_TargetCompound + 0.5))
　　　　　　　　　　　　　　　　　　　　　　　　############

data2$Competitor_TargetCompound<-data2$CompetitorCompound+data2$TargetCompound

#write.csv(data2, "Output3.trimmed.csv", row.names=F)
#data2 <- read.csv("data2ForPairs.MF02.3syllable02.csv", header =T) 
head(data2)
                        #基本四つのパタン.odds率を計算。0.5は微調整（分母が0でしたら計算できなくなります。）
data2$logit1 <- log((data2$Competitor_TargetCompound + 0.5) / (data2$all - data2$Competitor_TargetCompound + 0.5))
data2$logit2 <- log((data2$TargetCompound + 0.5) / (data2$all - data2$TargetCompound + 0.5))
data2$logit3 <- log((data2$CompetitorCompound + 0.5) / (data2$all - data2$CompetitorCompound + 0.5))
data2$logit4 <- log((data2$TargetSimplex + 0.5) / (data2$all - data2$TargetSimplex + 0.5))
data2$logit5 <- log((data2$IrrelevantSimplex + 0.5) / (data2$all - data2$IrrelevantSimplex + 0.5))
　　　　　　　　　　　　　　　　　　　　　　　　#下位条件を付けます


data2$tone3 <- ifelse(data2$cond == "a" | data2$cond == "b",1,0)
data2$comp <- ifelse(data2$cond == "b" | data2$cond == "d",1,0)

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
#tapply(data2$logit, data2$comp, mean)

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
m8 <- lmer(logit ~ tone3*comp + (1+comp+tone3:comp|subj) + (1+tone3+tone3:comp|item), data = data2)
summary(m8)
m7 <- lmer(logit ~ tone3*comp + (1+comp|subj) + (1+tone3+tone3:comp|item), data = data2)
summary(m7)
m6 <- lmer(logit ~ tone3*comp + (1+comp|subj) + (1+tone3:comp|item), data = data2)
summary(m6)
m5 <- lmer(logit ~ tone3*comp + (1+comp|subj) + (1|item), data = data2)
summary(m5)
m0  <-  lmer(logit ~ tone3*comp + (1|subj) + (1|item), data = data2)
summary(m0)

anova(m10,m9,m8,m7,m6,m5,m0)

#### p-value  ########

#Tone main effect
m5  <-  lmer(logit ~ tone3*comp + (1+comp|subj) + (1|item), data = data2)
m5i  <-  lmer(logit ~ comp+tone3:comp + (1+comp|subj) + (1|item), data = data2)
anova(m5, m5i)

#Structure main effect
m5  <-  lmer(logit ~ tone3*comp + (1+comp|subj) + (1|item), data = data2)
m5i  <-  lmer(logit ~ tone3+tone3:comp + (1+comp|subj) + (1|item), data = data2)
anova(m5, m5i)

#interaction
m5  <-  lmer(logit ~ tone3*comp + (1+comp|subj) + (1|item), data = data2)
m5i  <-  lmer(logit ~ tone3+comp + (1+comp|subj) + (1|item), data = data2)
anova(m5, m5i)

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
#
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
m9 <- lmer(logit ~ comp + (1|subj) + (1+comp|item), data = data2.comp)
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
m9 <- lmer(logit ~ tone3*comp + (1+tone3*comp|subj) + (1+tone3+tone3:comp|item), data = data2)
summary(m9)
m8 <- lmer(logit ~ tone3*comp + (1+tone3+tone3:comp|subj) + (1+tone3+tone3:comp|item), data = data2)
summary(m8)
m7 <- lmer(logit ~ tone3*comp + (1+tone3|subj) + (1+tone3+tone3:comp|item), data = data2)
summary(m7)
m6 <- lmer(logit ~ tone3*comp + (1|subj) + (1+tone3+tone3:comp|item), data = data2)
summary(m6)
m5 <- lmer(logit ~ tone3*comp + (1|subj) + (1+tone3|item), data = data2)
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
##################
##################
7. 2nd Target Simplex interection
                        #転換しやすいため
                        #logit1:Competitor_TargetCompound
                        #logit2:TargetCompound
                        #logit3:CompetitorCompound
                        #logit4:TargetSimplex

data2$logit<-data2$logit4
tapply(data2$logit, list(data2$tone3, data2$comp), mean)

m10 <- lmer(logit ~ tone3*comp + (1+tone3*comp|subj) + (1+tone3*comp|item), data = data2)
summary(m10)
m9 <- lmer(logit ~ tone3*comp + (1+comp+tone3:comp|subj) + (1+tone3*comp|item), data = data2)
summary(m9)
m8 <- lmer(logit ~ tone3*comp + (1+comp+tone3:comp|subj) + (1+comp+tone3:comp|item), data = data2)
summary(m8)
m7 <- lmer(logit ~ tone3*comp + (1+comp+tone3:comp|subj) + (1+tone3:comp|item), data = data2)
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
9. 2nd Target Compound sub effect test

#compound d b
data2.comp<-data2[data2$cond=="b"|data2$cond=="d",]
m10 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1+tone3|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1|item), data = data2.comp)
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
m9 <- lmer(logit ~ comp + (1|subj) + (1+comp|item), data = data2.comp)
summary(m9)
m0  <-  lmer(logit ~ comp + (1|subj) + (1|item), data = data2.comp)
summary(m0)
anova(m10,m9,m0)

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
##################
##################
7. 3rd Target Simplex interection
                        #転換しやすいため
                        #logit1:Competitor_TargetCompound
                        #logit2:TargetCompound
                        #logit3:CompetitorCompound
                        #logit4:TargetSimplex

data2$logit<-data2$logit4
tapply(data2$logit, list(data2$tone3, data2$comp), mean)

m10 <- lmer(logit ~ tone3*comp + (1+tone3*comp|subj) + (1+tone3*comp|item), data = data2)
summary(m10)
m9 <- lmer(logit ~ tone3*comp + (1+comp+tone3:comp|subj) + (1+tone3*comp|item), data = data2)
summary(m9)
m8 <- lmer(logit ~ tone3*comp + (1+comp+tone3:comp|subj) + (1+tone3+tone3:comp|item), data = data2)
summary(m8)
m7 <- lmer(logit ~ tone3*comp + (1+comp+tone3:comp|subj) + (1+tone3:comp|item), data = data2)
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
m5  <-  lmer(logit ~ tone3*comp + (1+tone3:comp|subj) + (1|item), data = data2)
m5i  <-  lmer(logit ~ comp+tone3:comp + (1+tone3:comp|subj) + (1|item), data = data2)
anova(m5, m5i)

#Structure main effect
m5  <-  lmer(logit ~ tone3*comp + (1+tone3:comp|subj) + (1|item), data = data2)
m5i  <-  lmer(logit ~ tone3+tone3:comp + (1+tone3:comp|subj) + (1|item), data = data2)
anova(m5, m5i)

#interaction
m5  <-  lmer(logit ~ tone3*comp + (1+tone3:comp|subj) + (1|item), data = data2)
m5i  <-  lmer(logit ~ tone3+comp + (1+tone3:comp|subj) + (1|item), data = data2)
anova(m5, m5i)


#########下位検定############
9. 3rd Target Compound sub effect test

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
m7  <-  lmer(logit ~ tone3*comp + (1+comp|subj) + (1+tone3+tone3:comp|item), data = data2)
m7i  <-  lmer(logit ~ comp+tone3:comp + (1+comp|subj) + (1+tone3+tone3:comp|item), data = data2)
anova(m7, m7i)

#Structure main effect
m7  <-  lmer(logit ~ tone3*comp + (1+comp|subj) + (1+tone3+tone3:comp|item), data = data2)
m7i  <-  lmer(logit ~ tone3+tone3:comp + (1+comp|subj) + (1+tone3+tone3:comp|item), data = data2)
anova(m7, m7i)

#interaction
m7  <-  lmer(logit ~ tone3*comp + (1+comp|subj) + (1+tone3+tone3:comp|item), data = data2)
m7i  <-  lmer(logit ~ tone3+comp + (1+comp|subj) + (1+tone3+tone3:comp|item), data = data2)
anova(m7, m7i)

#########下位検定############
3.4th combined subeffect test
#compound d b
data2.comp<-data2[data2$cond=="b"|data2$cond=="d",]
summary(data2.comp)
m10 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1+tone3|item), data = data2.comp)
summary(m10)
m9 <- lmer(logit ~ tone3 + (1+tone3|subj) + (1|item), data = data2.comp)
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
m9 <- lmer(logit ~ comp + (1+comp|subj) + (1|item), data = data2.comp)
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
m9 <- lmer(logit ~ tone3*comp + (1+comp+tone3:comp|subj) + (1+tone3*comp|item), data = data2)
summary(m9)
m8 <- lmer(logit ~ tone3*comp + (1+tone3:comp|subj) + (1+tone3*comp|item), data = data2)
summary(m8)
m7 <- lmer(logit ~ tone3*comp + (1|subj) + (1+tone3*comp|item), data = data2)
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
m9  <-  lmer(logit ~ tone3*comp + (1+comp+tone3:comp|subj) + (1+tone3*comp|item), data = data2)
m9i  <-  lmer(logit ~ comp+tone3:comp + (1+comp+tone3:comp|subj) + (1+tone3*comp|item), data = data2)
anova(m9, m9i)

#Structure main effect
m9  <-  lmer(logit ~ tone3*comp + (1+comp+tone3:comp|subj) + (1+tone3*comp|item), data = data2)
m9i  <-  lmer(logit ~ tone3+tone3:comp + (1+comp+tone3:comp|subj) + (1+tone3*comp|item), data = data2)
anova(m9, m9i)

#interaction
m9  <-  lmer(logit ~ tone3*comp + (1+comp+tone3:comp|subj) + (1+tone3*comp|item), data = data2)
m9i  <-  lmer(logit ~ tone3+comp + (1+comp+tone3:comp|subj) + (1+tone3*comp|item), data = data2)
anova(m9, m9i)


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
m9 <- lmer(logit ~ comp + (1+comp|subj) + (1|item), data = data2.comp)
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


