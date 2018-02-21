library(ggplot2)
library(UsingR)
library(stringr)
library(sampling)

#setting up the working directory
getwd()
setwd("E:/boston/Summer'16/Foundations of Analytics/Project/Game Of Thrones")
getwd()

#####################################################################
#Reading the dataset and storing it into variables
battles <-read.csv("battles.csv")
character_deaths <-read.csv("character-deaths.csv")
character_predictions<-read.csv("character-predictions.csv")
#####################################################################
#Preprocessing after import start
View((battles))
View((character_deaths))
View(character_predictions)

nrow(battles)
#removing the rows which donot have attacker king
battles <- battles[!(is.na(battles$attacker_king) | battles$attacker_king==""),]
battles$attacker_king <- factor(battles$attacker_king)
table(battles$attacker_king)
#levels(battles$attacker_king)

#appending the names in a row whose column title is blank as "none"
head(levels(character_predictions[,"title"]))
levels(character_predictions[,"title"])[1] <- "None"
character_predictions<-character_predictions[!(character_predictions$title=="[1]"),]
character_predictions$title<-factor(character_predictions$title)
head(levels(character_predictions$title))

#same as above for the house
head(levels(character_predictions[,"house"]))
levels(character_predictions[,"house"])[1] <- "None"
character_predictions<-character_predictions[!(character_predictions$house=="[1]"),]
character_predictions$house<-factor(character_predictions$house)
head(levels(character_predictions[,"house"]))

# Replacing NA with 0 in the book of death column for characters 
#whose name is not there in any of the book of deaths
character_deaths$Book.of.Death[is.na(character_deaths$Book.of.Death)]<-0
character_deaths$Book.of.Death
table(character_deaths$Book.of.Death)
View(character_deaths)

# Replacing NA with 0 in the death chapter column for characters 
#whose name is not there in any of the death chapter
character_deaths$Death.Chapter[is.na(character_deaths$Death.Chapter)]<-0
table(character_deaths$Death.Chapter)
#Preprocessing after import end

##########################################################################
##categorical data
table(battles[,"attacker_king"])

#table(character_predictions[,"title"])

#table(character_deaths[,"Allegiances"])
table(character_deaths$Allegiances)

#table(character_predictions[,"house"])

# pie chart for attaker kings
data<-table(battles$attacker_king)
data
slice.labels<-names(data)
slice.percent<-round(data/sum(data)*100)
slice.labels<-paste(slice.labels,slice.percent)
slice.labels<-paste(slice.labels,"%",sep = " ")

pie(data,labels = slice.labels,col=rainbow(4),
    main = "Percentage of attacks by the kings")

# pie chart for Allegiances
#pie(table(character_deaths$Allegiances),main = "A kingdom and its allegiances "
 #   ,col = rainbow(10),radius = 1.1)

barplot(table(character_deaths$Allegiances),col = "blue",ylim = c(0,255),
        main = "A kingdom and its allegiances ",xlab = "King",
        ylab = "Number of all allegiances")
#barplot for title
#barplot(table(character_predictions[,"title"]),col = "blue",
 #       ylim = c(0,200),xlab ="Titles",ylab = "Number of kings holding title")

#barplot for house
#barplot(table(character_predictions$house),col = "Green")
# to draw barplot

#########################################################################
##numerical data

table(character_predictions$alive)

#barplot(table(character_predictions$alive),main ="abc",
       # xlab = "Probability of alive characters",ylab = "Number of characters")
hist(character_predictions$alive,breaks = 15,xlab = "Probability of alive characters",
     ylab = "Number of characters",main = "Prediction for alive charaters")

x<- as.numeric(table(character_deaths$Death.Year))

dotchart(x,main = "Character deaths in year",labels = 297:300
         ,xlab = "No. of character Deaths",ylab = "year")



#dotchart(table(character_predictions$popularity) ,cex=.3,
 #        main = "xyz",
  #       xlab ="Probability of alive characters",
   #      ylab = "Number of characters" )

#x<-t(table(character_predictions$popularity))
#dotplot(x,groups = TRUE)


##########################################################################

#distribution
summary(character_predictions$alive)
var(character_predictions$alive)
sd(character_predictions$alive)
mean(character_predictions$alive)
nrow(character_predictions)
#x<-character_predictions$alive
#x
y<-seq(0.000,1.25,by=(1/1000))
pdf<-dnorm(y,mean = 0.6344697,sd = 0.3126374)
par(mfrow=c(1,1))
plot(y,pdf,type = "l",col="red",
     main = "Character alive predictions")
abline(h=0)

#nr<- nrow(character_predictions)
#z<-rnorm(20,mean = 0.6344697,sd = 0.3126374)
#z<- round(z)
#table(z)
#plot(table(z),type = 'h')

#cdf<-pnorm(y,mean = 0.6344,sd = 0.3127009)
#plot(x,cdf,type = "l",col="red",
 #    main = "Character alive predictions")

#Popularity plot

#summary(character_predictions$popularity)
#0.08957   0.160609

#sd(character_predictions$popularity)

#y<-seq(0.000000,1,by=(1/1000000))
#pdf<-dnorm(y,mean = 0.08957,sd = 0.160609)

#plot(y,pdf,type = "l",col="red",
 #    main = "Character popularity predictions")

#cumulative probability
#z<- seq(0,0.5)
mu<-0.6344697;sigma=0.3126374
cdf<- pnorm(y,mean = mu,sd=sigma)
#cdf
plot(y,cdf,type = "l",col="red",
     main = "Cumulative probability of Character alive predictions")

pnorm(mu+3*sigma,mean = mu,sd=sigma)- pnorm(mu-3*sigma,mean = mu, sd=sigma)

#normal distribution
#plot(table(character_predictions$alive),type = "h",main="Alive character prediction",
 #    xlab = "Probability",ylab = "Number of characters")
#plot(table(character_predictions$popularity),type = "h",main="Popularity character prediction",
 #        xlab = "Probability",ylab = "Number of characters")

###########################################################################
#central limit theorem
nrow(character_predictions)
mean(character_predictions$alive)
sd(character_predictions$alive)
x<-rnorm(nrow(character_predictions),mean=0.6344697,sd=0.3126374)
hist(x,prob=TRUE,breaks = 15)

# for sample size in 10,20,30,40
samples<- nrow(character_predictions)
par(mfrow=c(2,2))
for (sample.size in c(10,20,30,40)) {
xbar<-numeric(samples)
for(i in 1:samples)
{
  xbar[i]<-mean(rnorm(sample.size,mean=0.6344697,sd=0.3126374))
}
hist(xbar,prob=TRUE,breaks=15,ylim = c(0,8.2),
     main = paste("histogram for sample size ",sample.size),col = "Green")
cat("size of sample=",sample.size,"Mean=",mean(xbar),
    "Standard Deviation=",sd(xbar),"\n")
#mean(xbar)
#sd(xbar)
}

#various samplings

#simple random sampling

nrow(character_deaths)
table(character_deaths$Book.of.Death)
#taking 80 samples
s<- srswor(80,nrow(character_deaths))
#s
sample.1<- character_deaths[s!=0,]
table(sample.1$Book.of.Death)

# systematic sampling
N<-nrow(character_deaths)
n<-80
k<-ceiling(N/n)
k

r<-sample(k,1)

s<-seq(r,by=k,length=n)

sample.2<-character_deaths[s!=0,]
#sample.2
table(sample.2$Book.of.Death)

# unequal probabilities
pik<- inclusionprobabilities(character_deaths$Book.of.Death,80)
length(pik)
sum(pik)
s<-UPsystematic(pik)
sample.3<-character_deaths[s!=0,]
table(sample.3$Book.of.Death)

#stratified sampling
battle.attack<-battles$attacker_king
#battle.attack
battle.size<- battles$attacker_size
data<-data.frame(attacker = battle.attack,size= battle.size)
data
datum<- data[complete.cases(data),]
datum
table(datum$attacker)
names(table(datum$attacker))
b.attacker<-rep(names(table(datum$attacker)),c(3,8,8,5))
#b.defender
b.size<-datum$size
b.size
d<- data.frame(attack=b.attacker,si=b.size)
View(d)

freq<- table(d$attack)
freq

st.sizes<-20*freq/sum(freq)
st.sizes
st.2<-strata(d,stratanames = c("attack"),size = st.sizes,
             method = "srswor",description = TRUE)

st.sample2<-getdata(d,st.2)
st.sample2

#cluster sampling
set.seed(100)
cl<- cluster(d,c("attack"),size = 2,method = "srswor")
cl.sample<-getdata(data,cl)
table(cl.sample$attack)

#confidence level of 80 and 90 for the death chapters
nrow(character_deaths)
mean(character_deaths$Death.Chapter)
sd(character_deaths$Death.Chapter)
#for sample size 30 and 20 samples
set.seed(100)
mean_dc<-mean(character_deaths$Death.Chapter)
mean_dc
sd_dc<-sd(character_deaths$Death.Chapter)
sd_dc

x<-rnorm(nrow(character_deaths),mean = mean_dc,sd=sd_dc)
x<-as.integer(x)
sample.size<-30
sd.sample<-sd_dc/sqrt(sample.size)
sd.sample

samples<-20

conf<-c(80,90)
conf
alpha<-1-conf/100
alpha

for (i in 1:samples) 
  {
  str1<- sprintf("for sample %d",i)
  cat(str1,"\n")
  for (j in alpha) 
    {
    sample.data.1 <- sample(x, size=sample.size)
    xbar[i] <- mean(sample.data.1)
    
    str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                   100*(1-j), j, 
                   xbar[i] - qnorm(1-j/2) * sd.sample,
                   xbar[i] + qnorm(1-j/2) * sd.sample)
    cat(str,"\n")
  }
}

xbar
sum(abs(xbar[i]-mean_dc) > 2*sd.sample)
