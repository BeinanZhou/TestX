Raw_5 <- read.delim("~/Desktop/Analysis/Exp5Compiled2SD.txt")
Raw_6 <- read.delim("~/Desktop/Analysis/Exp6Compiled2SD.txt")
data <- rbind(Raw_5,Raw_6)
data <- as.data.frame(data)

# Data formatting
data$accResp <- NULL
data$RespBox <- NULL
data$Resp <- NULL
data$SubID <- as.factor(data$SubID)
data$ItemID <- as.factor(data$ItemID)
data$ItemType <- as.factor(data$ItemType)
data$Acc <- as.factor(data$Acc)
data$Condition <- as.factor(data$Condition)
data$RtOk <- as.factor(data$RtOk)
data$PrimeTone <- as.factor(data$PrimeTone)
data$RT<- as.numeric(data$RT)
data$PrimeTone <- revalue(data$PrimeTone, c("2"="Tone2", "3"="Tone3","4"="Tone4","1"="Tone1"))
data$Condition <- revalue(data$Condition, c("0"="NonLex", "1"="Lex","2"="NonTarget"))
data$PrimeTone = factor(data$PrimeTone,levels(data$PrimeTone)[c(4,3,2,1)])

# Check Data
str(data)
head(data)

subdata <- subset(data,data$RT > 200)
# subdata <- subset(data,data$RtOk == 1)
subdata.filler <- subset(subdata,subdata$ItemType != 1 )
Projector <- cbind(c(1:52),c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2))
colnames(Projector) <- c('SubID','Proj')
temp <- merge(subdata.filler,Projector,by.x = ('SubID'),by.y = ('SubID'))
temp$Proj <- as.factor(temp$Proj)

head(temp)
anova.Proj <- aov(temp$RT ~ temp$Proj)
summary(anova.Proj)
TukeyHSD(anova.Proj)
Nontargets <- ddply(temp,~Proj,summarise,mean=mean(RT),sd=sd(RT))
# Exp6_filler_1 <- ddply(temp,~Ver,summarise,mean=mean(RT),sd=sd(RT))

plot <- ggplot(temp, aes(RT, fill = Proj)) + geom_density(alpha = 0.3)
plot

#   Proj     mean       sd
#    1 625.6787 139.8454
#    2 721.1717 126.9593
# Estimated delay = 721.17-625.67 = 95.5 ms ~96ms