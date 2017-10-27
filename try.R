anscombe
a <-anscombe
apply(a,2,mean) #2 column,1 row
plot(x,y,by=group) 

?anscombe #寻求帮助
require(stats); require(graphics)
summary(anscombe)

##-- now some "magic" to do the 4 regressions in a loop:
ff <- y ~ x
mods <- setNames(as.list(1:4), paste0("lm", 1:4))
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  ## or   ff[[2]] <- as.name(paste0("y", i))
  ##      ff[[3]] <- as.name(paste0("x", i))
  mods[[i]] <- lmi <- lm(ff, data = anscombe)
  print(anova(lmi))
}

## See how close they are (numerically!)
sapply(mods, coef)
lapply(mods, function(fm) coef(summary(fm)))

## Now, do what you should have done in the first place: PLOTS
op <- par(mfrow = c(2, 2), mar = 0.1+c(4,4,1,1), oma =  c(0, 0, 2, 0))
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  plot(ff, data = anscombe, col = "red", pch = 21, bg = "orange", cex = 1.2,
       xlim = c(3, 19), ylim = c(3, 13))
  abline(mods[[i]], col = "blue")
}
mtext("Anscombe's 4 Regression data sets", outer = TRUE, cex = 1.5)
par(op)
#作业：如何更改anscombe的数据结构
#anscombe dataset
library(dplyr)
library(ggplot2)
library(reshape2)
#方法二
d1 <-data.frame(
  x <-anscombe[,1],
  y <-anscombe[,5],
  type <-"a"
)
d2 <-data.frame(
  x <-anscombe[,2],
  y <-anscombe[,6],
  type <-"b"
)
d3 <-data.frame(
  x <-anscombe[,3],
  y <-anscombe[,7],
  type <-"c"
)
d4 <-data.frame(
  x <-anscombe[,4],
  y <-anscombe[,8],
  type <-"d"
)
data <-rbind(d1,d2,d3,d4)






#方法四：library（reshape2）

#求X,Y两个向量中元素同升同降占向量长度的比例（一种相关性的度量方法）
#方法一
set.seed(123)
x <- sample(1:50,10,replace = T) #replace有放回的抽样
y <- sample(1:50,10,replace = T)
x1<-x[-1]-x[-length(x)] # diff(x)
x2<-ifelse(x1>0,1,0) #sign(x)取符号
y1<-y[-1]-y[-length(y)]
y2 <- ifelse(y1>0,1,0)
c <- ifelse(x2==y2,1,0)
mean(c)

#方法三
udcorr<-function(x,y) mean(sign(diff(x))==sign(diff(y)))

#一阶差分
x1 <-sign(diff(x))
y1 <-sign(diff(y))
mean{ifelse(x1==y1,1,0)}

x2 <-x[-1]-x[-length(x)] #diff()
c(NA,x[-1]-x[-length(x)])

data() #sample data

faithful
f <-faithful
view(f)
str(f) #数据结构
hist(f$eruptions,breaks = 20) #此图形分布可能来自两个不同的总体

Titanic
t <-Titanic
head(iris)

#批量处理
#导入1000只股票
#方法一：保存成独立的文件
fileName <- dir("data/csv")
scode<-substr(fileName,1,6)
nfile<-length(fileName)  

for(i in 1:nfile){  
  assign(paste("s",scode[i], sep=""),read.csv(fileName[i],header=TRUE))
}  
#方法二：保存成 list，并合并成一个文件
fileName <- dir("D:/tempdata/csv")
cls <- c("character","character","character",
         "numeric","numeric","numeric","numeric")
stocklist<- lapply(fileName,function(x) read.csv(x,header=TRUE,colClasses=cls,
                                                 stringsAsFactors=F))
allstcok<- do.call(rbind,stocklist)


#关于性能
# Create the data frame
col1 <- runif (12^5, 0, 2) #均匀分布
col2 <- rnorm (12^5, 0, 2) #正态分布
col3 <- rpois (12^5, 3)   #poisson分布
col4 <- rchisq (12^5, 2) #卡方分布
df <- data.frame (col1, col2, col3, col4)
#未经优化
system.time(
  {
    for (i in 1:nrow(df)){
      if (df[i,1]+df[i,2]+df[i,3]+df[i,4]>4) {df[i,5]<-"greatthan4"}
      else {df[,5]<-"lessthan4"}
    }
  }  
)
#优化
system.time(
  {
    want = which(rowSums(df) > 4)
    output = rep("less than 4", times = nrow(df))
    output[want] = "greater than 4"
  }  
)

#数据特性-变量名中包含值
#计算不同宗教类别中哪个工资水平的人数最多
library(reshape2)
raw <- read.csv("F://software learning//R//R lesson//pew.csv", check.names = F)
View(raw)
tidy <- melt(raw, id = "religion") #melt
View(tidy)
# We can now fix the column names
names(tidy) <- c("religion", "income", "n")
# Alternatively
tidy <- melt(raw, id = "religion",variable.name = "income", value.name = "n")
View(tidy)

#数据特性-变量名在单元格中
计算每天最低气温和最高气温的差值
raw <- read.delim("F://software learning//R//R lesson//weather.txt",check.names = F, na.strings = ".")
View(raw)
# na.rm = TRUE is useful if the missing values don't have any meaning
raw.tidy <- melt(raw,id = c("year", "month", "element"),variable.name = "day", na.rm = TRUE)
View(raw.tidy)
# reordering columns
raw <- raw.tidy[, c("year", "month", "day","element", "value")]
head(raw)
tidy <- dcast(raw, year + month + day ~ element,value.var = "value")
View(tidy)
head(tidy)
 
#泰塔尼克存活率计算
#如何通过原始表，计算存活率

#计算不同类别的存活率 = 存活人数/（存活人数 + 死亡人数）
titanic2 <-read.csv("F://software learning//R//R lesson//titanic2.csv",stringsAsFactors = FALSE)
View(titanic2)
#Step 1
tidy <- melt(titanic2, id = c("class", "age", "fate"),variable.name = "gender")
View(tidy)
#Step 2
tidy <- dcast(tidy, class + age + gender ~ fate,value.var = "value")
View(tidy)
#Step 3
tidy$rate <- round(tidy$survived /(tidy$survived + tidy$perished), 2)
head(tidy)


