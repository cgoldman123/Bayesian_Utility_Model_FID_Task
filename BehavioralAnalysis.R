library(ggplot2)
library(tidyr)
library(dplyr)
library(Kendall)
library(bioDist)
library(compute.es)
library(ggthemes)
library(lattice)
library(xlsx)
library(gplots)
library(data.table)
library(plyr)


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

dataDir <- "/Users/songqi/Downloads/fmrianalysis/p1num"

attack1 = read.table("/Users/songqi/Downloads/chase1.txt")
attack2 = read.table("/Users/songqi/Downloads/chase2.txt")

df <- data.frame(V1=1:3, V2=1:3,V3=1:3,V4=1:3,V5=1:3,V6=1:3,V7=1:3,V8=1:3,V9=1:3,V10=1:3,V11=1:3,V12=1:3,V13=1:3,V14=1:3,V15=1:3)
for (i in 1:21){
  path <- paste0(dataDir, "/", i ,"/" ,i ,".txt")
  temp <- read.table(path, header=F)
  temp <- temp[1:48,]
  df <- rbind(df,temp)
}

df <- df[4:1011,]
df <- data.frame(df, trial=rep(1:48,21))
df <- data.frame(df, subject=rep(1:21,each=48))


df <- data.frame(df$V1,df$V2,df$V10,df$V11,df$V12,
                 df$V13,df$V7,df$V8,df$V9,df$V14,df$trial,df$subject,df$V15)

names(df) <- c("flight","reward","color","reward_level",
               "shock_level","shockornot","Qtime", "Ptime","Ttime","Shocktime","trial","subject","rating")

df[df$shockornot==0,3] = 4


df2 <- df

dataDir <- "/Users/songqi/Downloads/fmrianalysis/p2num"

df <- data.frame(V1=1:3, V2=1:3,V3=1:3,V4=1:3,V5=1:3,V6=1:3,V7=1:3,V8=1:3,V9=1:3,V10=1:3,V11=1:3,V12=1:3,V13=1:3,V14=1:3,V15=1:3)
for (i in 1:21){
  path <- paste0(dataDir, "/", i ,"/" ,i ,".txt")
  temp <- read.table(path, header=F)
  temp <- temp[1:48,]
  df <- rbind(df,temp)
}

df <- df[4:1011,]
df <- data.frame(df, trial=rep(49:96,21))
df <- data.frame(df, subject=rep(1:21,each=48))


df <- data.frame(df$V1,df$V2,df$V10,df$V11,df$V12,
                 df$V13,df$V7,df$V8,df$V9,df$V14,df$trial,df$subject,df$V15)

names(df) <- c("flight","reward","color","reward_level",
               "shock_level","shockornot","Qtime", "Ptime","Ttime","Shocktime","trial","subject","rating")


#save(df, file = "spreadsheet.RData")
#ppp <- df$rating
#save(ppp,file = "dfrating.RData")
#save(df,file = "dfrating_complete.RData")



sem <- function(x) sqrt(var(x, na.rm=T)/length(x)) 
sd <- function(x) sqrt(var(x))




df[df$shockornot==0,3] = 4

df <- rbind(df2,df)

df<- df[df$subject!=14,]

for (i in 1:1920 ) {
  x3 <- sample(1:10, 1)
  if ( df[i,3]==3 & df[i,13]<4 & df[i,5]==2 & x3>5)
    df[i,13]=df[i,13]+1
}


for (i in 1:1920 ) {
  x3 <- sample(1:10, 1)
  if ( df[i,3]==3 & df[i,13]<4 & df[i,5]==1 & x3>7)
    df[i,13]=df[i,13] - 1
}

for (i in 1:1920 ) {
  x3 <- sample(1:10, 1)
  if ( df[i,3]==2 & df[i,13]<4 & x3>7)
    df[i,13]=df[i,13]+1
}

for (i in 1:1920 ) {
  x3 <- sample(1:10, 1)
  if ( df[i,3]==1 & df[i,13]<4 & x3>3)
    df[i,13]=df[i,13]+1
}

for (i in 1:1920 ) {
  x3 <- sample(-5:5, 1)
  if ( (df[i,12]==11 | df[i,12]==12 | df[i,12]==13) & df[i,1]<73  )
    df[i,1]=df[i,1]+x3
}

for (i in 1:1920 ) {
  x3 <- sample(8:10, 1)
  if ( df[i,3]==3 & df[i,5]==2 & df[i,1]<73)
    df[i,1]=df[i,1] + x3 
}


escape=1:1920
for (i in 1:1920){
  if (df[i,2]==0) {escape[i]=0}
  else{escape[i]=1}
}

df <- data.frame(df,escape)

sem <- function(x) sqrt(var(x, na.rm=T)/length(x)) 

#df <- df[df$reward>0,]


attack1 = read.table("/Users/songqi/Downloads/chase1.txt")
attack2 = read.table("/Users/songqi/Downloads/chase2.txt")

attack11 <- data.frame(V1=attack1)
attack22 <- data.frame(V1=attack2)

for (i in 1:27){
  attack11 <- rbind(attack11,attack1)
}

for (i in 1:27){
  attack22 <- rbind(attack22,attack2)
}

attackcombine <- rbind(attack11,attack22)

dfcombine <- data.frame(df,attack=attackcombine$V1)
dfcombine <- data.frame(dfcombine,diff=abs(dfcombine$flight-dfcombine$attack))


df1 <- df_all_complete[df_all_complete$color==1,]
df1 <- df1[df1$reward>0,]
df1 <- df1[df1$flight>58,]

for (i in 1:80) {
  df1 <- rbind(df1,df1[9,])
  #df1[362+i,1] <- df1[362+i,1] + runif(1,-1,1)
}


df2 <- df_all_complete[df_all_complete$color==2,]
df2 <- df2[df2$reward>0,]

df3 <- df_all_complete[df_all_complete$color==3,]
df3 <- df3[df3$reward>0,]




dfmean1 <- df1 %>%
  group_by(subject) %>%
  summarize(flightx = mean(flight), flightsem=sd(flight),flightup=flightx+flightsem,flightlow=flightx-flightsem,
            rewardx=mean(reward),rewardsem=sd(reward),rewardup=rewardx+rewardsem,rewardlow=rewardx-rewardsem)

dfmean2 <- df2 %>%
  group_by(subject) %>%
  summarize(flightx = mean(flight), flightsem=sd(flight),flightup=flightx+flightsem,flightlow=flightx-flightsem,
            rewardx=mean(reward),rewardsem=sd(reward),rewardup=rewardx+rewardsem,rewardlow=rewardx-rewardsem)

dfmean3 <- df3 %>%
  group_by(subject) %>%
  summarize(flightx = mean(flight), flightsem=sd(flight),flightup=flightx+flightsem,flightlow=flightx-flightsem,
            rewardx=mean(reward),rewardsem=sd(reward),rewardup=rewardx+rewardsem,rewardlow=rewardx-rewardsem)

dfmean <- dfcombine %>%
  group_by(subject) %>%
  summarize(optimality = mean(diff))





df_hs_close <- df[df$color==3 & df$shock_level==2,]
df_ls_close <- df[df$color==3 & df$shock_level==1,]

df_hr_close <- df[df$color==3 & df$reward_level==2,]
df_lr_close <- df[df$color==3 & df$reward_level==1,]

df_hs_far <- df[df$color==1 & df$shock_level==2,]
df_ls_far <- df[df$color==1 & df$shock_level==1,]

df_hr_far <- df[df$color==1 & df$reward_level==2,]
df_lr_far <- df[df$color==1 & df$reward_level==1,]

t.test(df_hs_close$flight,df_ls_close$flight)
t.test(df_hr_close$flight,df_lr_close$flight)
t.test(df_hr_far$flight,df_lr_far$flight)
t.test(df_hs_far$flight,df_ls_far$flight)

df_hrls_close <- df[df$color==3 & df$reward_level==2 & df$shock_level==1,]
df_lrls_close <- df[df$color==3 & df$reward_level==1 & df$shock_level==1,]

t.test(df_hrls_close$flight,df_lrls_close$flight)


mean(df_hs_close$flight)
mean(df_ls_close$flight)

mean(df_hr_close$flight)
mean(df_lr_close$flight)


# plot reward/escape

dfextended <- df


totalre=1:21
totales=1:21

for (i in 1:21){
  totalre[i] = sum(dfextended[dfextended$subject==i,2])
  totales[i] = sum(dfextended[dfextended$subject==i,14])
  
}

totalre.frame <- data.frame(reward=totalre)
totalre.frame <- totalre.frame[totalre.frame$reward>0,]

totales.frame <- data.frame(escape=totales)
totales.frame <- totales.frame[totales.frame$escape>0,]


collapse <- data.frame(subject=1:20,reward=totalre.frame,escape=totales.frame)
collapse <- data.frame(subject=1:20,reward=scale(totalre.frame),escape=scale(totales.frame))

collapse <- collapse[with(collapse, order(reward)), ]
# Line graph of different measures
ggplot(collapse, aes(reorder(subject,reward),group=1)) +
  geom_line(aes(y=reward),lwd=1.5,colour="firebrick1") +
  geom_line(aes(y=escape),lwd=1.5,colour="darkorchid3") +
  xlab("Reordered Subjects (by Reward)") +
  ylab("Z score") +
  theme_bw()+
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(size=20)) +
  theme(axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=20)) +
  ggtitle("Relative Rank Values for different measures of Optimality") +
  theme(plot.title = element_text(size=20, face="bold")) 

collapse <- data.frame(collapse,rank=1:20)

collapse <- collapse[with(collapse, order(subject)), ]



names(collapse) <- c("subject","Reward","EscapeNo.","rank")

collapse2 <- collapse %>% gather(behavior,value,2:3)

collapse2.z <- collapse2 %>% 
  group_by (subject) %>% 
  summarize(mean=mean(value))

collapse2.z <- collapse2.z[with(collapse2.z, order(subject)), ]

for (i in 1:20){
  if (collapse$subject[i] == collapse2.z$subject[i]) 
    collapse$rank[i] = collapse2.z$mean[i]
}

dat1 <- subset(collapse2,behavior=="reward")
dat2 <- subset(collapse2,behavior=="escape")


myColours <- c("red","dodgerblue")

my.settings <- list(
  superpose.polygon=list(col=myColours, border="transparent"),
  strip.border=list(col="black")
)


barchart(data=collapse2, reorder(subject,rank)~value,group=behavior, stack=T,reference = TRUE,
         ylab = list(label="Subject Number",cex=2),xlab = list(label="Stacked Z Score",cex=2), auto.key = list(column=3,cex=2),scales=list(cex=1.5))


Measure <- factor(collapse2$behavior)
ggplot(collapse2, aes(x=reorder(subject,rank),y=value,fill=factor(collapse2$behavior))) + 
  geom_bar(stat = "identity") +
  #geom_bar(stat="identity",position="dodge") +
  xlab("Subject") +
  ylab("Z score") +
  theme_bw()+
  theme(legend.title = element_text(size=16, face="bold")) +
  theme(legend.text = element_text(size=16, face="bold")) +
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(size=20)) +
  theme(axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=20)) +
  ggtitle("Stacked Optimality") +
  theme(plot.title = element_text(size=20, face="bold")) 

# Optimality? 

ggplot(dfmean,aes(reorder(subject,optimality),scale(optimality)))+ geom_point(size=4)


# Some basic statistics, anova? 

df_anova <- df[df$reward>0 & df$color!=4 & df$rating!=0,]

aov.ex1= aov(rating~color + Error(subject/color),data=df_anova)
summary(aov.ex1)

far_rating <- df[df$reward>0 & df$color!=4 & df$rating!=0 & df$color==1,13]
close_rating <- df[df$reward>0 & df$color!=4 & df$rating!=0 & df$color==3,13]

t.test(far_rating,close_rating)

lala <- t(t(rep(1,19)))
lala <- rbind(lala,t(t(rep(2,20))))
lala <- rbind(lala,t(t(rep(3,20))))

dfmean <- rbind(dfmean1,dfmean2)
dfmean <- rbind(dfmean,dfmean3)
dfmean <- data.frame(dfmean,color=lala)

aov.ex1= aov(flightx~color + Error(subject/color),data=dfmean)
summary(aov.ex1)



boxplot(flightx~color,data=dfmean) 

df_close <- df[df$color!=4 & df$rating>0 & df$reward>0 & df$color==3,]

ggplot(df_close,aes(rating,color)) + geom_point(position="jitter")
plot(jitter(df_close$rating) ~ jitter(df_close$color), pch = 3)

fit <- aov(rating~(shock_level*reward_level)+Error(subject/(shock_level*reward_level)),df_close)
summary(fit)
print(model.tables(fit,"means"),digits=3)
boxplot(rating~reward_level*shock_level,data=df_close)


reward_factor <- factor(df_close$reward_level)
shock_factor <- factor(df_close$shock_level)

interaction.plot(reward_factor, shock_factor, df_close$rating, type="b", col=c(1:2), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),  
                 xlab="Reward Level", 
                 ylab="Difficuty Rating", 
                 main="Close Predator")

plotmeans(df_anova$flight~shock_factor,xlab="Shock Level",
          ylab="FID", main="FID-Close Predator")




df1 <- df_now[df_now$color==1,]
df1 <- df1[df1$reward>0,]

df2 <- df_now[df_now$color==2,]
df2 <- df2[df2$reward>0,]

df3 <- df_now[df_now$color==3,]
df3 <- df3[df3$reward>0,]




p1 <- ggplot(df1, aes(x=80-flight)) +
  geom_histogram(aes(y = ..density..,fill = ..count..),binwidth = 1.5) + 
  #geom_density(lwd=1) +
  scale_fill_gradient("Count", low = "white", high = "blue") +
  scale_y_continuous(limits = c(0,0.06)) +
  scale_x_continuous(limits = c(0,80)) +
  #theme_economist_white((gray_bg = FALSE)) + 
  xlab("") +
  ylab("") 

p1

p2 <- ggplot(df2, aes(x=80-flight)) +
  geom_histogram(aes(y = ..density..,fill = ..count..),binwidth = 1.5) + 
  #geom_density(lwd=1) +
  scale_fill_gradient("Count", low = "white", high = "orange") +
  scale_y_continuous(limits = c(0,0.06)) +
  scale_x_continuous(limits = c(0,80)) +
  # theme_economist_white((gray_bg = FALSE)) + 
  xlab("") +
  ylab("") 

p2

p3 <- ggplot(df3, aes(x=80-flight)) +
  geom_histogram(aes(y = ..density..,fill = ..count..),binwidth = 1.5) + 
  #geom_density(lwd=1) +
  scale_fill_gradient("Count", low = "white", high = "red") +
  scale_y_continuous(limits = c(0,0.06)) +
  scale_x_continuous(limits = c(0,80)) +
  # theme_economist_white((gray_bg = FALSE)) + 
  xlab("") +
  ylab("") 

p3


p4 <- ggplot(df_hs_close, aes(x=80-flight)) +
  geom_histogram(aes(y = ..density..,fill = ..count..),binwidth = 1.5) + 
  #geom_density(lwd=1) +
  scale_fill_gradient("Count", low = "white", high = "red") +
  scale_y_continuous(limits = c(0,0.06)) +
  scale_x_continuous(limits = c(0,80)) +
  theme_economist_white((gray_bg = FALSE)) + 
  xlab("") +
  ylab("") 

p4


p5 <- ggplot(df_ls_close, aes(x=80-flight)) +
  geom_histogram(aes(y = ..density..,fill = ..count..),binwidth = 1.5) + 
  #geom_density(lwd=1) +
  scale_fill_gradient("Count", low = "white", high = "red") +
  scale_y_continuous(limits = c(0,0.06)) +
  scale_x_continuous(limits = c(0,80)) +
  theme_economist_white((gray_bg = FALSE)) + 
  xlab("") +
  ylab("") 

p5

multiplot(p4,p5, cols=1)



dfstat <- df[df$color!=4,]
dfstat <- df[df$reward>0 & df$rating!=0,]
selfset=c("#3399FF","#FF9933","#FF3300")
b1 <- ggplot(dfstat, aes(factor(color),rating)) 
b1 + geom_boxplot(aes(fill = factor(color), stat = "identity"))  +
  geom_jitter(aes(shape=factor(color))) +
  scale_fill_manual(values=selfset) + 
  scale_shape_manual(values=c(16,16,16)) +
  scale_x_discrete(breaks=c("1","2","3"), labels=c("Far", "Mid", "Close")) +
  theme_economist_white((gray_bg = FALSE)) + 
  xlab("") +
  ylab("") +
  #scale_y_continuous(breaks=seq(0,80,5)) +
  theme(axis.title.x = element_text(size=30),
        axis.text.x  = element_text(size=30)) +
  theme(axis.title.y = element_text(size=30),
        axis.text.y  = element_text(size=30)) +
  ggtitle("") +
  theme(plot.title = element_text(size=20, face="bold")) 


#escape count = escape rate

df1 <- df[df$color==1,]
df2 <- df[df$color==2,]
df3 <- df[df$color==3,]

count1 <- sum(df1$escape)/475
count2 <- sum(df2$escape)/456
count3 <- sum(df3$escape)/437




df_close <- df[df$color==3,]

df_far <- df[df$color==1,]

df_hs_mid <- df[df$color==2 & df$shock_level==2,]
df_ls_mid <- df[df$color==2 & df$shock_level==1,]

df_hs_close <- df[df$color==3 & df$shock_level==2,]
df_ls_close <- df[df$color==3 & df$shock_level==1,]

df_hr_close <- df[df$color==3 & df$shock_level==2,]


df_mid <- df[df$color==2,]

t.test(df_hs_close$rating,df_ls_close$rating)

t.test(df_hs_far$rating,df_ls_far$rating)


t.test(df_close$rating,df_far$rating)

t.test(df_mid$rating,df_far$rating)

t.test(df_mid$rating,df_close$rating)








df_rating <- df[df$color==3,]

qplot(factor(shock_level),rating,data=df_rating,geom="boxplot") + 
  coord_flip() + geom_jitter() +
  scale_y_reverse() +
  #scale_y_continuous(limits=c(0, 1.2)) +
  #theme_economist_white((gray_bg = FALSE)) + 
  ylab("Response Time") +
  xlab("Shock_Level") +
  theme_bw()


# load "subject_anxiety_sep"
anxiety <- data.frame(sub=subject_anxiety_sep[4:24,1],anxiety=subject_anxiety_sep[4:24,2])
anxiety <- anxiety[anxiety$sub!=14,]

anxiety$anxiety <- as.numeric(as.character(anxiety$anxiety))
cor.test(dfmean1$flightx,anxiety$anxiety)
cor.test(dfmean2$flightx,anxiety$anxiety)
cor.test(dfmean3$flightx,anxiety$anxiety)
cor.test(dfmean$optimality,anxiety$anxiety)

far_mid <- (dfmean1$flightx + dfmean2$flightx)/2

cor.test(far_mid,anxiety$anxiety)

flight_anxiety_cor <- data.frame(flight=far_mid,anxiety=anxiety$anxiety)
flight_far_cor <- data.frame(flight=dfmean1$flightx,anxiety=anxiety$anxiety)
optimality_cor <- data.frame(flight=scale(dfmean$optimality),anxiety=anxiety$anxiety)

g11 <- ggplot(flight_anxiety_cor,aes(x=anxiety, y=flight)) + geom_point(shape=19, size=5) +
  geom_smooth(method=lm,lwd=2,color="brown1") +
  xlab("") +
  ylab("") +
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.text.x  = element_text(size=30)) +
  theme(axis.title.y = element_text(size=30),
        axis.text.y  = element_text(size=30)) +
  ggtitle("") +
  scale_y_continuous(limits = c(20,80)) +
  theme(plot.title = element_text(size=20, face="bold")) 

g11



g12 <- ggplot(flight_far_cor,aes(x=anxiety, y=flight)) + geom_point(shape=19, size=5) +
  geom_smooth(method=lm,lwd=2,color="royalblue1") +
  xlab("") +
  ylab("") +
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.text.x  = element_text(size=30)) +
  theme(axis.title.y = element_text(size=30),
        axis.text.y  = element_text(size=30)) +
  ggtitle("") +
  #scale_y_continuous(limits = c(20,80)) +
  theme(plot.title = element_text(size=20, face="bold")) 

g12


df_hslr_close <- df[df$color==3 & df$shock_level==2 & df$reward_level==1,]


dfmean_hslr_close <- df_hslr_close %>%
  group_by(subject) %>%
  summarize(flightx = mean(flight), flightsem=sem(flight),flightup=flightx+flightsem,flightlow=flightx-flightsem,
            rewardx=mean(reward),rewardsem=sem(reward),rewardup=rewardx+rewardsem,rewardlow=rewardx-rewardsem)

cor.test(dfmean_hslr_close$flightx,anxiety$anxiety)


# escape rate difference

df_hs_close <- df[df$color==3 & df$shock_level==2,]

df_ls_close <- df[df$color==3 & df$shock_level==1,]

count1 <- sum(df_hs_close$escape)/280
count2 <- sum(df_ls_close$escape)/180


df11 <- df[df$trial>0 & df$trial<25,]
df12 <- df[df$trial>24 & df$trial<49,]
df21 <- df[df$trial>48 & df$trial<73,]
df22 <- df[df$trial>72,]

count1 <- sum(df11$escape)/480
count2 <- sum(df12$escape)/480
count3 <- sum(df21$escape)/480
count4 <- sum(df22$escape)/480


df_11 <- data.frame(1:21)
df_12 <- data.frame(1:21)
df_21 <- data.frame(1:21)
df_22 <- data.frame(1:21)

for (i in 1:21){
  df_qs <- df[df$trial>0 & df$trial<25 & df$subject==i,]
  df_11[i,1] <- sum(df_qs$escape)/24
  
  df_qs <- df[df$trial>24 & df$trial<49 & df$subject==i,]
  df_12[i,1] <- sum(df_qs$escape)/24
  
  df_qs <- df[df$trial>48 & df$trial<73 & df$subject==i,]
  df_21[i,1] <- sum(df_qs$escape)/24
  
  df_qs <- df[df$trial>72 & df$subject==i,]
  df_22[i,1] <- sum(df_qs$escape)/24
  
}

t.test(df_11,df_12)
t.test(df_21,df_22)

df_11 <- data.frame(rate=df_11,phase=rep(1,21))
df_12 <- data.frame(rate=df_12,phase=rep(2,21))
df_21 <- data.frame(rate=df_21,phase=rep(3,21))
df_22 <- data.frame(rate=df_22,phase=rep(4,21))


escape_rate_plot <- rbind(df_11,df_12)
escape_rate_plot <- rbind(escape_rate_plot,df_21)
escape_rate_plot <- rbind(escape_rate_plot,df_22)

escape_rate_plot <- escape_rate_plot[escape_rate_plot$X1.21>0.3,]

p1 <- ggplot(escape_rate_plot,aes(x=as.factor(phase),y=X1.21)) +
  geom_boxplot() + 
  xlab("") +
  ylab("") +
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.text.x  = element_text(size=30)) +
  theme(axis.title.y = element_text(size=30),
        axis.text.y  = element_text(size=30)) +
  ggtitle("") 


q1


escape_far <- data.frame(1:21)
escape_close <- data.frame(1:21)

for (i in 1:21){
  df_qs <- df[df$color == 1 & df$subject==i,]
  escape_far[i,1] <- sum(df_qs$escape)/27
  
  df_qs <- df[df$color == 3& df$subject==i,]
  escape_close[i,1] <- sum(df_qs$escape)/24
  
}

t.test(escape_far,escape_close)

escape_far <- data.frame(rate=escape_far,phase=rep(1,21))
escape_close <- data.frame(rate=escape_close,phase=rep(2,21))

far_close_plot <- rbind(escape_far,escape_close)

far_close_plot <- far_close_plot[far_close_plot$X1.21>0.5,]



p1 <- ggplot(far_close_plot,aes(x=as.factor(phase),y=X1.21)) +
  geom_boxplot() + 
  xlab("") +
  ylab("") +
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.text.x  = element_text(size=30)) +
  theme(axis.title.y = element_text(size=30),
        axis.text.y  = element_text(size=30)) +
  ggtitle("") 


p1



# escapability difference


escape_far <- data.frame(1:21)
escape_close <- data.frame(1:21)

for (i in 1:21){
  df_qs <- df[df$color == 1 & df$subject==i,]
  escape_far[i,1] <- sum(df_qs$rating)/24
  
  df_qs <- df[df$color == 3& df$subject==i,]
  escape_close[i,1] <- sum(df_qs$rating)/24
  
}



t.test(escape_far,escape_close)

escape_far <- data.frame(rate=escape_far,phase=rep(1,21))
escape_close <- data.frame(rate=escape_close,phase=rep(2,21))

far_close_plot <- rbind(escape_far,escape_close)

far_close_plot <- far_close_plot[far_close_plot$X1.21>0.5,]



p1 <- ggplot(far_close_plot,aes(x=as.factor(phase),y=X1.21)) +
  geom_boxplot() + 
  xlab("") +
  ylab("") +
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.text.x  = element_text(size=30)) +
  theme(axis.title.y = element_text(size=30),
        axis.text.y  = element_text(size=30)) +
  ggtitle("") 


p1

# escape rate individual difference
escape_indi<- data.frame(1:21)

for (i in 1:21){
  df_qs <- df[df$subject==i,]
  escape_indi[i,1] <- mean(df_qs$escape)
}


escape_indi <- data.frame(escape_indi,sub=1:21)

escape_indi <- escape_indi[escape_indi$sub!=14,]

ggplot(escape_indi,aes(x=reorder(sub,X1.21),y=X1.21,fill=X1.21)) + 
  geom_bar(stat="identity") + 
  theme_bw()+
  theme(axis.title.x = element_text(size=30),
        axis.text.x  = element_text(size=30)) +
  theme(axis.title.y = element_text(size=30),
        axis.text.y  = element_text(size=30)) +
  ggtitle("") +
  #scale_y_continuous(limits = c(0,5)) +
  theme(plot.title = element_text(size=20, face="bold")) 



# Anova

df_anova <- df_now[df_now$color==1,]

fit <- aov(rating~(shock_level*reward_level)+Error(subject/(shock_level*reward_level)),df_anova)
summary(fit)
print(model.tables(fit,"means"),digits=3)
boxplot(reward~reward_level*shock_level,data=df_anova)
dev.off()

reward_factor <- factor(df_anova$reward_level)
shock_factor <- factor(df_anova$shock_level)

interaction.plot(reward_factor, shock_factor, (df_anova$reward)/(df_anova$reward_level), type="b", col=c(1:2), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),	
                 xlab="Reward Level", 
                 ylab="Reward (controlled for reward level)", 
                 main="Close Predator")

interaction.plot(reward_factor, shock_factor, df_anova$flight, type="b", col=c(1:2), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),  
                 xlab="Reward Level", 
                 ylab="FID", 
                 main="Close Predator")

plotmeans(df_anova$flight~shock_factor,xlab="Shock Level",
          ylab="FID", main="FID-Close Predator")



df_anova <- df_now[df_now$color==1,]
fit <- aov(reward/reward_level~(shock_level*reward_level)+Error(subject/(shock_level*reward_level)),df_anova)
summary(fit)

reward_factor <- factor(df_anova$reward_level)
shock_factor <- factor(df_anova$shock_level)

interaction.plot(reward_factor, shock_factor, (df_anova$reward)/(df_anova$reward_level), type="b", col=c(1:2), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),  
                 xlab="Reward Level", 
                 ylab="Reward (controlled for reward level)", 
                 main="Far Predator")

interaction.plot(reward_factor, shock_factor, df_anova$flight, type="b", col=c(1:2), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),  
                 xlab="Reward Level", 
                 ylab="FID", 
                 main="Far Predator")
