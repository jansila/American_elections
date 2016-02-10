#Computational task 1 - Data mining and neural networks
setwd("~/Desktop/Data mining")

library(xlsx)
elections<-read.csv("elections_dataset.csv", sheetIndex=1, header = TRUE,as.data.frame=TRUE)
elections<-elections_dataset #if data added manually
rm(elections_dataset)

#presidential victories
p<-data.frame(elections[which(elections$Winner=="p"),])
#opposition victories
o<-data.frame(elections[which(elections$Winner=="o"),])

myFreq(elections)

require(stargazer)
 
#Basic counts output
write(stargazer(myFreq(elections[which(elections$Winner=="p"),]), summary=FALSE, title="Presidential wins - question counts"), file="Pres_distrib.tex")
write(stargazer(myFreq(elections[which(elections$Winner=="o"),]), summary=FALSE, title="Oppositions wins - question counts"), file="Op_distrib.tex")
#myFreq(p)  #counts for presidential
#myFreq(o)  #counts for opposition


write(stargazer(findQ(), summary=rep(FALSE,2), title="Best questions output"), file="questions2.tex")
#findQ()    #uncomment to see the output without writing a file

  # Q2 
# Run through the data fixing Q4 and compute counts for combinations all combinations
write(stargazer(pairs(),summary=rep(FALSE,3), title="Pairs question counts (best selected)"), file="pairs2.tex")
#pairs()    #uncomment to see the output without writing a file

# Q3
write(stargazer(triple(),summary=rep(FALSE,3), title="Trios question counts (best selected)"), file="triplets2.tex")
#triple() #uncomment to see the output without writing a file



################################################################
#                           Functions                          #
################################################################


findQ<-function(data=elections){
  q1p<-myFreq(data[1:17,])  
  q1o<-myFreq(data[18:30,])  
  correctOut<-c()
  
  bigtable<-c()
  
  
for(i in 1:12){
  y<-q1p[1,i]
  yo<-q1o[1,i]
  n<-q1p[2,i]
  no<-q1o[2,i]
 correct<-c(max(y,yo),max(n,no))
 correctOut<-cbind(correctOut,sum(correct))
 bigtable<-as.data.frame(cbind(bigtable,cbind(rbind(y,n),rbind(yo,no))))
}
  
correctOut<-as.data.frame(rbind(correctOut,30-correctOut), row.names=c("Correct","Mistakes"))
colnames(correctOut)<-colnames(elections)[2:13]
odd<-seq(from=1,by=2,to=23)
even<-seq(from=2,by=2,to=24)
names<-rep("a",24)
names[odd]<-c("pres")
names[even]<-c("opp")
colnames(bigtable)<-names
 return(list(predictions=correctOut,counts=bigtable)) }


myFreq<-function(data=elections){
  out<-c()
  for(i in 1:12){
    temp<-c(length(which(data[,i+1]=="y")),length(which(data[,i+1]=="n")))
    out<-cbind(out,temp)}
  colnames(out)<-colnames(data[,2:13])
  rownames(out)<-c("y","n")
  return(as.data.frame(out))
  }

####################

pairs<-function(data=elections){
q4p<-data[1:17,]$Q4
q4o<-data[18:30,]$Q4
p<-data[1:17,]
o<-data[18:30,]

subsetp<-cbind(p[,2:4],p[,6:13])
subseto<-cbind(o[,2:4],o[,6:13])

require(plyr)
bigtable<-c()
correctOut<-c()
for(i in 1:11){
#columnbind the columns and counts occurences of each  
pairs<-paste(subsetp[,i],q4p,sep="")  
count<-count(as.data.frame(pairs))
count<-data.frame(freq=count$freq,row.names=c(as.character(count$pairs)))

yn<-count["yn",]
if(is.na(yn)) yn<-0
ny<-count["ny",]
if(is.na(ny)) ny<-0
yy<-count["yy",]
if(is.na(yy)) yy<-0
nn<-count["nn",]
if(is.na(nn)) nn<-0
#porovnat ty yy,yn,ny,nn pro obe otazky a zase hledat pro ktery krize je tam nejvetsi cislo...

pairs2<-paste(subseto[,i],q4o,sep="")  
counto<-count(as.data.frame(pairs2))
counto<-data.frame(freq=counto$freq,row.names=c(as.character(counto$pairs2)))
yno<-counto["yn",]
if(is.na(yno)) yno<-0
nyo<-counto["ny",]
if(is.na(nyo)) nyo<-0
yyo<-counto["yy",]
if(is.na(yyo)) yyo<-0
nno<-counto["nn",]
if(is.na(nno)) nno<-0

correct<-c(max(yn,yno),max(ny,nyo),max(yy,yyo),max(nn,nno))
correctOut<-cbind(correctOut,sum(correct))
bigtable<-as.data.frame(cbind(bigtable,cbind(rbind(yn,ny,yy,nn),rbind(yno,nyo,yyo,nno))))
}
odd<-seq(from=1,by=2,to=21)
even<-seq(from=2,by=2,to=22)
names<-rep("a",22)
names[odd]<-c("pres")
names[even]<-c("opp")
correctOut<-as.data.frame(correctOut)
colnames(correctOut)<-colnames(subsetp)
colnames(bigtable)<-names

return(list(correct=correctOut,mistakes=30-correctOut, bigtable=bigtable))
}




triple<-function(data=elections){
  q4p<-data[1:17,]$Q4
  q4o<-data[18:30,]$Q4
  q12p<-data[1:17,]$Q12
  q12o<-data[18:30,]$Q12
  p<-data[1:17,]
  o<-data[18:30,]
  
  subsetp<-cbind(p[,2:4],p[,6:12])
  subseto<-cbind(o[,2:4],o[,6:12])

  require(plyr)
  correctOut<-c()
  bigtable<-c()
  
  for(i in 1:10){
    #columnbind the columns and counts occurences of each  
    pairs<-paste(subsetp[,i],q12p,q4p,sep="")  
    count<-count(as.data.frame(pairs))
    count<-data.frame(freq=count$freq,row.names=c(as.character(count$pairs)))
    
    yyn<-count["yyn",]
    if(is.na(yyn)) yyn<-0
    nyn<-count["nyn",]
    if(is.na(nyn)) nyn<-0
    yny<-count["yny",]
    if(is.na(yny)) yny<-0
    nny<-count["nny",]
    if(is.na(nny)) nny<-0
    yyy<-count["yyy",]
    if(is.na(yyy)) yyy<-0
    nyy<-count["nyy",]
    if(is.na(nyy)) nyy<-0
    ynn<-count["ynn",]
    if(is.na(ynn)) ynn<-0
    nnn<-count["nnn",]
    if(is.na(nnn)) nnn<-0
    #porovnat ty yy,yn,ny,nn pro obe otazky a zase hledat pro ktery krize je tam nejvetsi cislo...
    
    pairs2<-paste(subseto[,i],q12o,q4o,sep="")  
    counto<-count(as.data.frame(pairs2))
    counto<-data.frame(freq=counto$freq,row.names=c(as.character(counto$pairs2)))
   
    yyno<-counto["yyn",]
    if(is.na(yyno)) yyno<-0
    nyno<-counto["nyn",]
    if(is.na(nyno)) nyno<-0
    ynyo<-counto["yny",]
    if(is.na(ynyo)) ynyo<-0
    nnyo<-counto["nny",]
    if(is.na(nnyo)) nnyo<-0
    yyyo<-counto["yyy",]
    if(is.na(yyyo)) yyyo<-0
    nyyo<-counto["nyy",]
    if(is.na(nyyo)) nyyo<-0
    ynno<-counto["ynn",]
    if(is.na(ynno)) ynno<-0
    nnno<-counto["nnn",]
    if(is.na(nnno)) nnno<-0
    
  
    correct<-c(max(yyn,yyno),max(nyn,nyno),max(yny,ynyo),max(nny,nnyo),max(yyy,yyyo),max(nyy,nyyo),max(ynn,ynno),max(nnn,nnno))
    correctOut<-cbind(correctOut,sum(correct))
    bigtable<-as.data.frame(cbind(bigtable,cbind(rbind(yyn,nyn,yny,nny,yyy,nyy,ynn,nnn),rbind(yyno,nyno,ynyo,nnyo,yyyo,nyyo,ynno,nnno))))
  }
  
  odd<-seq(from=1,by=2,to=19)
  even<-seq(from=2,by=2,to=20)
  names<-rep("a",20)
  names[odd]<-c("pres")
  names[even]<-c("opp")
  
  correctOut<-as.data.frame(correctOut)
  colnames(correctOut)<-colnames(subsetp)
  colnames(bigtable)<-names
  return(list(correct=correctOut,mistakes=30-correctOut, bigtable=bigtable))
}
