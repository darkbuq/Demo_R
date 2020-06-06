library(magrittr)
library(data.table)
library(openxlsx) # read.xlsx
library(readxl) #read_excel
library(ggplot2)
library(broom) # 如凌亂輸出lm，nls或t.test，並將其轉換為整齊的數據幀  函數 tidy()
library(corrplot)
library(igraph)
library(Scale)
library(rpart) # decision tree
library(rpart.plot)

rm(list=ls())



# 第一段  送原始 google表單 -------------------------------------------------------
Qfile='200 space'

form=paste0('./input/',Qfile,'.csv') %>% read.csv %>% setDT

form=form[-1,]
colnames(form)[1:5] 
colnames(form)[2:3]=c('type','number')
colnames(form)[1:5] 

# 第二段  睡眠計算 ---------------------------------------------------------------

# 上床
gotobed=data.table(gotobed=form$gotobed, 
                   section=substr(form$gotobed,start=10,stop=11), 
                   hr=substr(form$gotobed,start=1,stop=2) %>% as.numeric,
                   min=substr(form$gotobed,start=4,stop=5) %>% as.numeric)

gotobed[section=='PM',new.hr := as.character(hr+12)]
gotobed[section=='PM' & hr==12 & min>0,new.hr := '00']
gotobed[section=='AM' & hr<10 ,new.hr := paste0('0',hr)]
gotobed[section=='AM' & hr>=10 ,new.hr := as.character(hr)]
gotobed[min<10, new.min := paste0('0',min)]
gotobed[min>=10, new.min := as.character(min)]

gotobed$ppaste=paste0(Sys.Date() %>% as.character,' ',gotobed$new.hr,':',gotobed$new.min,':00')
gotobed$result=as.POSIXct(gotobed$ppaste,format="%Y-%m-%d %H:%M:%S")

# 起床
wake=data.table(wake=form$wake,
                section=substr(form$wake,start=10,stop=11),
                hr=substr(form$wake,start=1,stop=2) %>% as.numeric,
                min=substr(form$wake,start=4,stop=5) %>% as.numeric)

wake[section=='PM', new.hr := as.character(hr+12)]
wake[section=='PM' & hr==12 & min>0,new.hr := '00']
wake[section=='AM' & hr<10 ,new.hr := paste0('0',hr)]
wake[section=='AM' & hr>=10 ,new.hr := as.character(hr)]
wake[min<10, new.min := paste0('0',min)]
wake[min>=10, new.min := as.character(min)]

wake$ppaste=paste0(Sys.Date() %>% as.character,' ',wake$new.hr,':',wake$new.min,':00')
wake$result=as.POSIXct(wake$ppaste,format="%Y-%m-%d %H:%M:%S")

# 比較 & 相減
dff=data.table(wake=wake$result, gotobed=gotobed$result)
dff$test=dff$wake-dff$gotobed

dff[test>0, new.wake := wake]
dff[test<=0, new.wake := wake+(24*60*60)]
dff[, new.gotobed := gotobed]
dff$test2=dff$new.wake-dff$new.gotobed

dff[test2>15, test2 := test2-12] # 手動減12小時

# 實睡
dff$sleep.hr=form$sleep.hr %>% as.character %>% as.numeric
dff$sleep.min=form$sleep.min %>% as.character %>% as.numeric

dff$real.sleep=dff$sleep.hr+(dff$sleep.min/60)
dff[, result := real.sleep/as.numeric(test2)]

dff$conversion=NA
dff[,conversion := as.integer(conversion)]
dff[result>=0.85,conversion := 0]
dff[result>=0.75 & result<0.85,conversion := 1]
dff[result>=0.65 & result<0.75,conversion := 2]
dff[result<0.65,conversion := 3]

form$In.bed.time=dff$test2
form$sleeping.time=dff$real.sleep
form$sleep.efficiency=dff$conversion

rm(dff,gotobed,wake)

# 第三段  生成 form.q  計算 score  -----------------------------------------------
merge.table=openxlsx::read.xlsx(xlsxFile=paste0('./input/table200.xlsx'),sheet = 1)

form.q=NA
i=unique(merge.table$topic)[50]
for (i in unique(merge.table$topic)) {  # unique 去掉多餘重複的（重複的只保留一個），得到沒有重複數據的序列
  
  print(i)
  
  eval(parse(text=paste0('q=data.frame( type=form$type, number=form$number, topic=NA,options=form$',i,')')))
  q$topic=i
  
  q=merge(x=q, y=merge.table[,c("topic","options","options_code")], by=c("topic","options"))
  
  q=q[,c("type","number","topic","options","options_code")]
  
  form.q=rbind(form.q,q)
  rm(q)
}
rm(merge.table)

form.q=form.q[-1,]; form.q$options <- NULL

form.q=dcast(form.q, type + number ~ topic, value.var="options_code")
is.na(form.q) %>% sum

form.q$In.bed.time=form$In.bed.time
form.q$sleeping.time=form$sleeping.time

Q300=read.csv('./input/200person Q300.csv')  # 心血管
form.q=merge(x=form.q, y=Q300, by=c('type','number'))
rm(Q300)

form.q$Q200=form$sleep.efficiency

# 算BMI
form$height=form$height %>% as.character %>% as.numeric
form$weight=form$weight %>% as.character %>% as.numeric
form$BMI=form$weight/((form$height/100)^2)

form.q=merge(x=form.q, y=form[,c('number','height','weight','BMI')], by='number')

write.csv(form.q,paste0('./result/',Qfile,' form_q.csv'))


f.formula=openxlsx::read.xlsx(xlsxFile=paste0('./input/table200.xlsx'),sheet = 2)

# form.q=read.csv('./input/form 12-9.csv')
form.q %>% class
form.q %>% setDT
setnames(form.q,'Q200','sleep.efficiency')
# setnames(form.q,'person_no','number')

i=5
for (i in 1:length(f.formula$formula)) {
  print(f.formula$item[i])
  print(paste0('form.q$',f.formula$item[i],'=form.q[, .(',f.formula$formula[i],')]'))
  eval(parse(text=paste0('form.q$',f.formula$item[i],'=form.q[, .(',f.formula$formula[i],')]')))
  Sys.sleep(0.1)
}

score=data.table(type=form.q$type, number=form.q$number,
                 In.bed.time=form.q$In.bed.time,
                 sleeping.time=form.q$sleeping.time,
                 form.q[ ,which(colnames(form.q)==f.formula$item[1]) : ncol(form.q)])

score$睡眠潛伏期=sapply(score$睡眠潛伏期,function(x) {if (is.finite(x)==FALSE) {0} else {x}}) # 先排除非正常數字  賦值給0
score$睡眠潛伏期=sapply(score$睡眠潛伏期,function(x) {if (x==1 | x==2) {1} else if (x==3 | x==4) {2} else if (x==5 | x==6) {3} else {0}})

score$睡眠困擾=sapply(score$睡眠困擾,function(x) {if (is.finite(x)==FALSE) {0} else {x}}) # 先排除非正常數字  賦值給0
score$睡眠困擾=sapply(score$睡眠困擾,function(x) {if (x>=1 & x<=9) {1} else if (x>=10 & x<=18) {2} else if (x>=19 & x<=27) {3} else {0}})

score$白天功能運作情況=sapply(score$白天功能運作情況,function(x) {if (is.finite(x)==FALSE) {0} else {x}}) # 先排除非正常數字  賦值給0
score$白天功能運作情況=sapply(score$白天功能運作情況,function(x) {if (x==1 | x==2) {1} else if (x==3 | x==4) {2} else if (x==5 | x==6) {3} else {0}})

i=length(f.formula$formula)
eval(parse(text=paste0('score$PSQI=score[, .(',f.formula$formula[i],')]')))

score %>% class
is.na(score) %>% sum
# score[is.na(score)] <- 0  # 改NA成0
score$心血管風險等級=form.q$Q300

form.q$睡眠效率=score$睡眠效率
form.q$睡眠潛伏期=score$睡眠潛伏期
form.q$睡眠困擾=score$睡眠困擾
form.q$白天功能運作情況=score$白天功能運作情況
form.q$PSQI=score$PSQI

rm(f.formula)

# merge 血液 ----------------------------------------------------------------

blood=read.csv('./input/blood.csv')
colnames(blood)[1]='number'

score$number %>% class
score$number=score$number %>% as.integer()

blood$number %>% class



score=merge(x=score, y=blood, by='number')
rm(blood)

# merge 睡眠 ----------------------------------------------------------------

sleepp=read.csv('./input/sleep.csv')
score=merge(x=score, y=sleepp, by='number')
rm(sleepp)

# merge 步數 ----------------------------------------------------------------

stepcount=read.csv('./input/step new.csv') %>% setDT
setnames(stepcount,'ksteps','步數平均')
score=merge(x=score, y=stepcount, by='number')
rm(stepcount)

# merge 心率 ----------------------------------------------------------------

RH=read.csv('./input/wcv_rcv.csv')
RH %>% colnames
RH=RH[,-1]
score=merge(x=score, y=RH, by='number')
rm(RH)

write.csv(score,paste0('./result/',Qfile,'_fraction.csv'))

# merge 基本資料 ------------------------------------------------------------------

Q.conv.topic=openxlsx::read.xlsx(xlsxFile=paste0('./input/table200.xlsx'),sheet = 5) %>% setDT

i=1
for (i in 1:nrow(Q.conv.topic)) {
  eval(parse(text=paste0('score$',Q.conv.topic$new[i],'=form.q$',Q.conv.topic$old[i])))
}
rm(Q.conv.topic)

score$number %>% class
form$number %>% class
form$number = form$number %>% as.integer

score=merge(x=score, y=form[,c('number','height','weight','BMI')], by='number')
score[type=='control',輪班狀況 :=0]
score[type=='exp',輪班狀況 :=1]

# 第四段 敘述性統計 ---------------------------------------------------------------

stats=openxlsx::read.xlsx(xlsxFile=paste0('./input/table200.xlsx'),sheet = 4) %>% setDT

stats$scale_item %>% unique
score %>% colnames

kk=paste(stats$scale_item,collapse = ",")  #特殊用法 把data.frame其中一個col堆疊 加,號  串成一個字串
kk=paste0('type,number,',kk)
stats %>% class

eval(parse(text=paste0('dff=score[,.(',kk,')]')))
dff %>% colnames

dff %>% setDT
dff$number %>% class
dff$number=dff$number %>% as.character %>% as.numeric

dff1=dff[, lapply(.SD, summary),by='type']
dff1$number=dff1$number %>% as.character
#####dff1$number=c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")

dff2=dff[, lapply(.SD, sd, na.rm=TRUE),by='type']
dff2$number=dff2$number %>% as.character
dff2$number='SD'

dff3=dff[, lapply(.SD, var, na.rm=TRUE),by='type']
dff3$number=dff3$number %>% as.character
dff3$number='Var'

write.table(dff1,paste0("./result/",Qfile,"_Descriptive statistics.csv"),sep=",",row.names = F)
write.table(dff2,paste0("./result/",Qfile,"_Descriptive statistics.csv"),append=TRUE,sep=",",row.names = F,col.names =F)
write.table(dff3,paste0("./result/",Qfile,"_Descriptive statistics.csv"),append=TRUE,sep=",",row.names = F,col.names =F)
rm(dff1,dff2,dff3,kk)

# 第五段 T檢定 -----------------------------------------------------------------

# t.test(control$PSQI,expp$PSQI, paired=TRUE) # 兩種都行
# 兩種都行 這種可以內含分群功能再做T檢定
t.test(PSQI~type,dff, paired=TRUE) %>% tidy  # 兩種都行
# t.test(PSQI~type,dff, paired=F) %>% tidy  # 這不是我們要的
dff4=NA
i=62
for (i in 3:ncol(dff)) {
  print(colnames(dff)[i])
  
  if (colnames(dff)[i]!='輪班狀況') {
    paste0('ttest=t.test(',colnames(dff)[i],'~type,dff, paired=TRUE) %>% tidy') %>% print
    eval(parse(text=paste0('ttest=t.test(',colnames(dff)[i],'~type,dff, paired=TRUE) %>% tidy')))
    rownames(ttest)=colnames(dff)[i]
    
    dff4=rbind(dff4,ttest)
    rm(ttest)
  }
}

dff4=dff4[-1,]
dff4$kind=dff4 %>% rownames
dff4 %>% colnames
dff4=dff4[,c("kind","estimate","statistic","parameter","conf.low","conf.high","method","alternative","p.value")]

dff4 %>% setDT  # 會失去rownames
dff4$Significant=NA
dff4$Significant=dff4$Significant %>% as.character
# P>0.05        不顯著
# 0.01<P<=0.05  顯著
# P<=0.01       非常顯著
dff4[p.value >0.05, Significant := '不顯著']
dff4[p.value >0.01 & is.na(Significant), Significant := '顯著']
dff4[p.value <=0.01, Significant := '非常顯著']

write.table(dff4,paste0("./result/",Qfile,"_Ttest.csv"),sep=",",row.names = F)
rm(dff4)

# 第六段 GLM -----------------------------------------------------------------

glm.table=openxlsx::read.xlsx(xlsxFile=paste0('./input/table200.xlsx'),sheet = 7)
glm.result=NA

i=1
for (i in 1:ncol(glm.table)) {
  dff=copy(score)
  dff %>% setDT
  
  kk1=glm.table[,i][complete.cases(glm.table[,i])] %>% data.frame
  colnames(kk1)='x'
  kk2=paste(kk1$x[2:nrow(kk1)],collapse = "+")
  
  kk=paste0(kk1$x[1],'~',kk2)
  
  eval(parse(text=paste0('dff$',kk1$x[1],' = dff$',kk1$x[1],' %>% as.character %>% as.numeric')))
  eval(parse(text=paste0('dff[',kk1$x[1],'==0, .(number,',kk1$x[1],')]')))
  eval(parse(text=paste0('dff[',kk1$x[1],'==0, ',kk1$x[1],':=0.01]')))
  
  print(kk)
  print(paste0('dff5=glm(',kk,', data=dff, family=Gamma(link="inverse")) %>% tidy'))
  eval(parse(text=paste0('dff5=glm(',kk,', data=dff, family=Gamma(link="inverse")) %>% tidy')))
  
  dff5=data.frame(type=colnames(glm.table)[i],dff5)
  
  glm.result=rbind(glm.result,dff5)
}

glm.result=glm.result[-1,]


glm.result$Significant=NA
glm.result$Significant=glm.result$Significant %>% as.character

# P>0.05        不顯著
# P<0.05       顯著(significant)
# P<0.01   高度顯著(highly significant)
# P<0.001  非常顯著(extremely significant)
glm.result %>% setDT
glm.result[p.value >=0.05, Significant := '不顯著']
glm.result[p.value >=0.01 & is.na(Significant), Significant := '顯著(significant)']
glm.result[p.value >=0.001 & is.na(Significant), Significant := '高度顯著(highly significant)']
glm.result[p.value <0.001, Significant := '非常顯著(extremely significant)']

write.table(glm.result,paste0("./result/",Qfile,"_glm.csv"),sep=",",row.names = F)
rm(dff5,glm.result,glm.table)

# 第七段  盒狀圖 ----------------------------------------------------------------

stats=openxlsx::read.xlsx(xlsxFile=paste0('./input/table200.xlsx'),sheet = 4) %>% setDT

kk=paste(stats$scale_item,collapse = ",")  #特殊用法 把data.frame其中一個col堆疊 加,號  串成一個字串
kk=paste0('type,number,',kk)
stats %>% class

eval(parse(text=paste0('dff=score[,.(',kk,')]')))

dff %>% setDF
dff$number %>% class
dff$number=dff$number %>% as.character %>% as.numeric

i=3
for (i in 3:ncol(dff)) {
  print(i)
  print(colnames(dff)[i])
  Sys.sleep(0.1)
  boxdf=data.frame(type=dff$type,number=dff$number,target=dff[,i])
  
  Sys.sleep(0.2)
  
  
  ggplot(boxdf, aes(x=type, y=target, fill=type))+
    geom_boxplot()+
    geom_jitter(shape=16, position=position_jitter(0.2))+ # 補點
    labs(title=paste0(colnames(dff)[i],' 盒狀圖及散布點'), y = colnames(dff)[i])+
    theme(legend.position="none",
          axis.text=element_text(size=20),
          axis.title=element_text(size=25,face="bold"),
          plot.title = element_text(size=30,face="bold"))
  Sys.sleep(0.4)
  ggsave(paste0('./result/box_',colnames(dff)[i],'.png'), width = 10, height = 8, dpi = 300)
  Sys.sleep(0.4)
  rm(boxdf)
  Sys.sleep(0.1)
}
rm(kk,kk1,kk2,dff,stats)

# 第八段  相關性分析 --------------------------------------------------------------

cor.data=copy(score)
cor.factor=openxlsx::read.xlsx(xlsxFile=paste0('./input/table200.xlsx'),sheet = 6) %>% setDT  # 沒改變數

i=6
for (i in 1:ncol(cor.factor)) {
  print(i)
  print(colnames(cor.factor)[i])
  
  paste0('col.list=cor.factor$',colnames(cor.factor)[i]) %>% print
  eval(parse(text=paste0('col.list=cor.factor$',colnames(cor.factor)[i])))
  
  col.list=col.list[complete.cases(col.list)]
  col.list=paste(col.list,collapse=",")
  print(col.list)
  
  cor.data2=copy(cor.data)
  
  paste0('cor.data2=cor.data2[,.(',col.list,')]') %>% print
  eval(parse(text=paste0('cor.data2=cor.data2[,.(',col.list,')]')))
  
  # 查缺值
  is.na(cor.data2) %>% sum
  # cor.data2[is.na(cor.data2)] <- 0  # 改NA成0
  
  dfff <- cor(cor.data2)
  is.na(dfff) %>% sum
  
  if (is.na(dfff) %>% sum !=0) {
    dfff[is.na(dfff)] <- 0 # 改NA成0
  }
  
  
  png(filename=paste0('./result/corrplot_',i,'.png'),width =1200, height =1200)
  par(oma=c(0,0,6,0))
  corrplot(dfff,method="color", addCoef.col="black",tl.col = "black",mar = c(0.5, 0,1, 0),
           tl.cex = 1.5)
  
  mtext(paste0(colnames(cor.factor)[i],'相關性分析'), side=3, line=0, cex=4, outer=TRUE)
  dev.off()
  
  Sys.sleep(0.2)
  file.rename(from = paste0('./result/corrplot_',i,'.png'), to = paste0('./result/corrplot_',colnames(cor.factor)[i],'.png'))
  Sys.sleep(0.2)
}
rm(cor.data,cor.data2,cor.factor,dfff,col.list)

# 第九段  SNA ----------------------------------------------------------------

radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}

cor.data=copy(score)
cor.factor=openxlsx::read.xlsx(xlsxFile=paste0('./input/table200.xlsx'),sheet = 6) %>% setDT  # 沒改變數


i=10  # i=8血液 i=9睡眠 i=10心率 
for (i in 7:ncol(cor.factor)) {
  print(colnames(cor.factor)[i])
  
  paste0('col.list=cor.factor$',colnames(cor.factor)[i]) %>% print
  eval(parse(text=paste0('col.list=cor.factor$',colnames(cor.factor)[i])))
  
  col.list=col.list[complete.cases(col.list)]
  col.list=paste(col.list,collapse=",")
  print(col.list)
  
  cor.data2=copy(cor.data)
  
  paste0('cor.data2=cor.data2[,.(',col.list,')]') %>% print
  eval(parse(text=paste0('cor.data2=cor.data2[,.(',col.list,')]')))
  
  # 查缺值
  is.na(cor.data2) %>% sum
  # cor.data2[is.na(cor.data2)] <- 0  # 改NA成0
  
  dfff <- cor(cor.data2)
  is.na(dfff) %>% sum
  
  if (is.na(dfff) %>% sum !=0) {
    dfff[is.na(dfff)] <- 0 # 改NA成0
  }
  
  j=1
  for (j in 1:2) {
    if (j==1) {
      # SNA 社會網絡分析 #
      test=dfff  # 借用
      test[test<0]=0 # 負值改零
      test[test<=0.4 & test>=-0.4]<-0 # 放棄一些線
      
      g=graph.adjacency(test,weighted = T,mode ='undirected')
      g=simplify(g) #不要自己指自己
      
      ncol(test)
      lab.locs <- radian.rescale(x=1:ncol(test), direction=-1, start=0)
      
      sna.point='goldenrod1'
      line1='firebrick1'
      line2='cyan3'
      
      # 有負值配色
      E(g)[ weight > 0.4 ]$color <- line1
      E(g)[ weight <= 0.4 & weight > 0]$color <- "thistle1"
      E(g)[ weight < -0.4 ]$color <- line2
      E(g)[ weight < 0 & weight >= -0.4 ]$color <- "slategray1"
      
      png(filename=paste0('./result/SNA_',i,j,'.png'),width =1500, height =1500)
      par(oma=c(0,0,6,0))
      
      plot(g,layout=layout.circle,  # 原來的layout=layout1  另一種layout=layout_with_lgl  layout.circle
           vertex.size=abs(strength(g))*5,
           vertex.color=sna.point,
           vertex.label.dist=1,
           vertex.frame.color=sna.point, vertex.label.color="black",
           vertex.label.cex=2,
           vertex.shape='circle',
           vertex.label.degree=lab.locs,
           edge.color=E(g)$color,
           edge.width=abs(E(g)$weight)*10)
      
      mtext(paste0(colnames(cor.factor)[i],'社會網路分析'), side=3, line=0, cex=4, outer=TRUE)
      dev.off()
      Sys.sleep(0.2)
    } else {
      # SNA 社會網絡分析 #
      test=dfff  # 借用
      test[test>0]=0 # 正值改零
      test[test<=0.4 & test>=-0.4]<-0 # 放棄一些線
      
      g=graph.adjacency(test,weighted = T,mode ='undirected')
      g=simplify(g) #不要自己指自己
      
      ncol(test)
      lab.locs <- radian.rescale(x=1:ncol(test), direction=-1, start=0)
      
      sna.point='goldenrod1'
      line1='firebrick1'
      line2='cyan3'
      
      # 有負值配色
      E(g)[ weight > 0.4 ]$color <- line1
      E(g)[ weight <= 0.4 & weight > 0]$color <- "thistle1"
      E(g)[ weight < -0.4 ]$color <- line2
      E(g)[ weight < 0 & weight >= -0.4 ]$color <- "slategray1"
      
      png(filename=paste0('./result/SNA_',i,j,'.png'),width =1500, height =1500)
      par(oma=c(0,0,6,0))
      
      plot(g,layout=layout.circle,  # 原來的layout=layout1  另一種layout=layout_with_lgl  layout.circle
           vertex.size=abs(strength(g))*5,
           vertex.color=sna.point,
           vertex.label.dist=1,
           vertex.frame.color=sna.point, vertex.label.color="black",
           vertex.label.cex=2,
           vertex.shape='circle',
           vertex.label.degree=lab.locs,
           edge.color=E(g)$color,
           edge.width=abs(E(g)$weight)*10)
      
      mtext(paste0(colnames(cor.factor)[i],'社會網路分析'), side=3, line=0, cex=4, outer=TRUE)
      dev.off()
      Sys.sleep(0.2)
    }
  }
}
rm(cor.data,cor.data2,cor.factor,dfff,col.list,line1,line2,sna.point,lab.locs,g,test)


# 第十段  決策樹 ----------------------------------------------------------------

tree=read.csv('./input/decision tree.csv') %>% setDT

tree$plot_name %>% unique
kk=paste(tree$plot_name %>% unique,collapse = ",")
kk=paste0('type,number,',kk)

eval(parse(text=paste0('dff=score[,.(',kk,')]')))

dff=melt(dff, id=c("type","number"))

colnames(dff)=c("type","number","plot_name","num")
dff[, num := as.numeric(num)]
tree[, c("start","end") := list(as.numeric(start), as.numeric(end))]

# non-equi join  # 不對等(non Equi)聯結(join)
# gg=tree[dff, .(person, person_no, plot_name, num, Conversion), on = .(start <= num, end >= num, plot_name==plot_name), allow.cartesian=TRUE]
dff=tree[dff, .(type, number, plot_name, num, Conversion), on = .(plot_name==plot_name,start <= num, end > num)]

subset(dff,!complete.cases(dff$Conversion))  # 檢查某col是否為空

dff=dcast(dff[,c("type","number","plot_name","Conversion")], type + number ~ plot_name, value.var="Conversion")
dff %>% colnames
unique(tree$title)
i=7
for (i in 1:length(unique(tree$plot_name))) {
  print(i)
  print(unique(tree$plot_name)[i] %>% as.character)
  print(unique(tree$Related_factors)[i] %>% as.character)
  
  dff1=form
  
  dff1 %>% class
  dff1[type=='control',輪班狀況 :=0]
  dff1[type=='exp',輪班狀況 :=1]
  
  eval(parse(text=paste0('dff1=dff1[, .(type,number,',unique(tree$Related_factors)[i],',輪班狀況)]')))
  
  dff1=merge(x=dff1, y=score[,c('type','number','心血管風險等級')], by=c('type','number'))
  
  print(colnames(dff)[i+2])
  eval(parse(text=paste0('dff1=cbind(dff$',colnames(dff)[i+2],',dff1)')))
  colnames(dff1)[1]=colnames(dff)[i+2]
  
  dff1$type=NULL
  dff1$number=NULL
  
  eval(parse(text=paste0('cart.model<- rpart(',colnames(dff)[i+2],' ~ . , data=dff1, method="class" )')))
  
  #Qfile='50person v01'
  #Qfile='200 space'
  if (Qfile=='50person v01') {
    png(filename=paste0('./result/decision tree_',unique(tree$plot_name)[i] %>% as.character,'.png'),width =1900, height =1000)
  } else {
    png(filename=paste0('./result/decision tree_',unique(tree$plot_name)[i] %>% as.character,'.png'),width =3500, height =1500)
  }
  
  
  par(oma=c(0,0,6,0))
  prp(cart.model,         # 模型
      faclen=0,           # 呈現的變數不要縮寫
      fallen.leaves=F, # 讓樹枝以垂直方式呈現
      shadow.col="gray",  # 最下面的節點塗上陰影
      # number of correct classifications / number of observations in that node
      extra=2,
      cex = 3)
  mtext(unique(tree$title)[i], side=3, line=0, cex=5, outer=TRUE)  
  dev.off()
  
}
rm(dff,dff1,tree,cart.model,kk,i)
