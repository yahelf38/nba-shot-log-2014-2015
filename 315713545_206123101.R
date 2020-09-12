# librarys ----------------------------------------------------------------
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse") 
library(tidyverse)
install.packages("readr")
library(readr)
install.packages("lubridate")
library(lubridate)
install.packages("ggplot2")
library(ggplot2)
install.packages("xts")
library(xts)
install.packages("corrplot")
library(corrplot)
install.packages("sm")
library(sm)
install.packages("effsize")
library(effsize)
install.packages("car")
library(car)
install.packages("hexbin")
library(hexbin)
install.packages("MVN")
library(MVN)

# load data ----------------------------------------------------------------
shot_logs<-read.csv("315713545_206123101.csv")
# question 1 ----------------------------------------------------------------
## -anova test for distance between defender and shooter for each month 
##makes a new column for each month 
shot_logs<-shot_logs %>%
  mutate(Month = ifelse(str_detect(shot_logs$MATCHUP,"JAN"),"JAN"
                 ,ifelse(str_detect(shot_logs$MATCHUP,"FEB"),"FEB"
                  ,ifelse(str_detect(shot_logs$MATCHUP,"MAR"),"MAR"
                    ,ifelse(str_detect(shot_logs$MATCHUP,"APR"),"APR"
                     ,ifelse(str_detect(shot_logs$MATCHUP,"MAY"),"MAY"
                      ,ifelse(str_detect(shot_logs$MATCHUP,"JUN"),"JUN"
                       ,ifelse(str_detect(shot_logs$MATCHUP,"JUL"),"JUL"
                        ,ifelse(str_detect(shot_logs$MATCHUP,"AUG"),"AUG"
                         ,ifelse(str_detect(shot_logs$MATCHUP,"SEP"),"SEP"
                          ,ifelse(str_detect(shot_logs$MATCHUP,"OCT"),"OCT"
                           ,ifelse(str_detect(shot_logs$MATCHUP,"NOV"),"NOV"
                            ,ifelse(str_detect(shot_logs$MATCHUP,"DEC"),"DEC","")))))))))))))
  
## anova test for months 
## make month column as a factor 

MonthVector<-unique(shot_logs$Month)
shot_logs$Month <- as.factor(shot_logs$Month)
shot_logs$Month = factor(shot_logs$Month , labels = MonthVector)

## assumption 

jan <- subset(shot_logs,Month == "JAN")
feb <- subset(shot_logs,Month == "FEB")
mar <- subset(shot_logs,Month == "MAR")
oct <- subset(shot_logs,Month == "OCT")
nov <- subset(shot_logs,Month == "NOV")
dec <- subset(shot_logs,Month == "DEC")

## check that each group is normally distributed for CLOSE_DEF_DIST
par(mfrow=c(3,3))

qqnorm(jan$CLOSE_DEF_DIST,main = "jan")
qqline(jan$CLOSE_DEF_DIST)

qqnorm(feb$CLOSE_DEF_DIST,main = "feb")
qqline(feb$CLOSE_DEF_DIST)

qqnorm(mar$CLOSE_DEF_DIST,main = "mar")
qqline(mar$CLOSE_DEF_DIST)

qqnorm(oct$CLOSE_DEF_DIST,main = "oct")
qqline(oct$CLOSE_DEF_DIST)

qqnorm(nov$CLOSE_DEF_DIST,main  = "nov")
qqline(nov$CLOSE_DEF_DIST )

qqnorm(dec$CLOSE_DEF_DIST,main = "dec")
qqline(dec$CLOSE_DEF_DIST)

## Kolmogorov-Smirnov test for normality

ks.test(shot_logs$CLOSE_DEF_DIST,"pnorm", mean(shot_logs$CLOSE_DEF_DIST),sd(shot_logs$CLOSE_DEF_DIST))

## next assumption - variances hummogancy shot clock

varOfMonthsClosestDef<-c(var(oct$CLOSE_DEF_DIST, na.rm =  TRUE),
  var(nov$CLOSE_DEF_DIST, na.rm = TRUE),
  var(dec$CLOSE_DEF_DIST, na.rm = TRUE),
  var(jan$CLOSE_DEF_DIST, na.rm = TRUE),
  var(feb$CLOSE_DEF_DIST, na.rm = TRUE),
  var(mar$CLOSE_DEF_DIST, na.rm = TRUE))


pVar<-ggplot(shot_logs, aes(x=CLOSE_DEF_DIST, y = Month, xlab("month"),ylab("distance from def"))) 

pVar + 
  geom_boxplot(notch = TRUE)+
    coord_flip()


bartlett.test(CLOSE_DEF_DIST~Month, data = shot_logs)

## plot ## plot distribution of means of means 
NamesOfmonths<-unique(shot_logs$Month)
meanOfMonthsClosestDef<-c(mean(oct$CLOSE_DEF_DIST, na.rm =  TRUE),
                         mean(nov$CLOSE_DEF_DIST, na.rm = TRUE),
                         mean(dec$CLOSE_DEF_DIST, na.rm = TRUE),
                         mean(jan$CLOSE_DEF_DIST, na.rm = TRUE),
                         mean(feb$CLOSE_DEF_DIST, na.rm = TRUE),
                         mean(mar$CLOSE_DEF_DIST, na.rm = TRUE))

meanOfMonthsClosestDefDataFrame<-data.frame(MONTH = NamesOfmonths, CLOSE_DEF_DIST_mean = meanOfMonthsClosestDef)
meanOfMonthsClosestDefDataFrame$MONTH<-as.factor(meanOfMonthsClosestDefDataFrame$MONTH)

ggplot(meanOfMonthsClosestDefDataFrame, aes(x= MONTH,y=CLOSE_DEF_DIST_mean)) +
  geom_point(color = "black",size = 3, shape = 24 ) + 
  geom_smooth()

max(meanOfMonthsClosestDef)-min(meanOfMonthsClosestDef)

## start anova test 

model2 <- lm(CLOSE_DEF_DIST~Month, data = shot_logs)

P<-anova(model2)
P
## extract f value from anova

criticalQ<-qf(.95, df1 = length(NamesOfmonths)-1, df2 = length(shot_logs$GAME_ID)-length(NamesOfmonths))

if(P[4]$`F value`[1]>criticalQ){
  print("need to reject null hypothesis")
}
## visualise f curve
## blue line is alpha value
## red line is f value

curve(df(x, df1=length(NamesOfmonths)-1, 
         length(shot_logs$GAME_ID)-length(NamesOfmonths)), 
          from=0, to=5,
           xlab = "DEF distance from shooter", 
            ylab = "f distrabution") 
              abline(v=c(criticalQ,P[4]$`F value`[1]),col=c("blue", "red"), lty=c(1,2), lwd=c(1, 3))

##for G power
    print(P[4]$`F value`[1]/2)

## extra statistics 
    
var((shot_logs$CLOSE_DEF_DIST))
max(shot_logs$CLOSE_DEF_DIST)-min(shot_logs$CLOSE_DEF_DIST)
sd(shot_logs$CLOSE_DEF_DIST)
          
# question 2 ----------------------------------------------------------------
## make a new data frame for all the player 
## games played, shots taken, shots made, 
PlayersList <- unique(shot_logs$player_name)
GameList<-unique(shot_logs$MATCHUP)
## player statistics data frame -  columns for the games they played, shots taken, and shots made 
playerStats <- data.frame(player_name = PlayersList, games_played = rep(0,length(PlayersList)),
                          shots_taken=rep(0,length(PlayersList)),
                            shots_made = rep(0,length(PlayersList)))
## loop how many games they have played 
## each game filter shot_logs for that game and save it as a variable 
## make a new vector for only the names of the players for that specific game 
for(i in 1:length(GameList)){
gameVariable <- filter(shot_logs,MATCHUP == GameList[i])
playerInGameVariable <- unique(gameVariable$player_name)
## run a new loop on the vector of the names of the players 
## if the name of the player shows on that vector add 1 to the player statistics data frame 
  for(j in 1:length(playerInGameVariable)){
    for (k in 1:length(playerStats$player_name)) {
        if(playerInGameVariable[j]==playerStats$player_name[k]){
          playerStats$games_played[k]=playerStats$games_played[k]+1
        }
      }
    }
}

## count shots for each player 
  for(k in 1:length(shot_logs$player_name)){
    for(i in 1:length(playerStats$player_name)){
    if(shot_logs$player_name[k]==playerStats$player_name[i] && shot_logs$SHOT_RESULT[k] == "made"){
      playerStats$shots_made[i]=playerStats$shots_made[i]+1
    }
     if(shot_logs$player_name[k]==playerStats$player_name[i]) {
      playerStats$shots_taken[i] = playerStats$shots_taken[i] + 1
      }   
    }
  }
## calculate percentage of accuracy 
playerStats <- mutate(playerStats, shot_percentage  = shots_made/shots_taken)
## calculate percentage of shots per game 
playerStats <- mutate(playerStats, shot_per_game  = shots_taken/games_played)
sort(playerStats$shot_percentage)
## round the shots per game 
## make histogram for shots per game 
dt2<-function(x){
  dt(x - mean(playerStats$shot_per_game), length(playerStats$shot_per_game))
}

## mean shots per game 
mean(playerStats$shot_per_game)

##plot distrabution
GraphShotsPerGame<-ggplot(data = playerStats, aes(shot_per_game)) + 
  geom_histogram(aes(y = ..density..), 
                 fill = "light blue", 
                  color = "black",
                    bins = 15)+
                    stat_function(
                      fun = dt2,
                          color = "dark blue", 
                             size = 0.7)

## add confidence interval lines to graph
 lowerTailCi<-mean(playerStats$shot_per_game, na.rm =TRUE)-
   qt(0.975,length(playerStats$shot_per_game))*
   sd(playerStats$shot_per_game)/sqrt(length(playerStats$shot_per_game))
 UpperTailCi<-mean(playerStats$shot_per_game, na.rm =TRUE)+
   (qt(0.975,length(playerStats$shot_per_game))*
   sd(playerStats$shot_per_game)/sqrt(length(playerStats$shot_per_game)))
 
ciShotspergame <-c(lowerTailCi,UpperTailCi) 
## plot histogram and confidence interval

GraphShotsPerGame +
  geom_vline(xintercept = ciShotspergame, color = "red", size = 1) +
    annotate(geom = 'text', x = ciShotspergame, y = 0.025, color = 'blue', label = c("0.025" , "0.975"), hjust = c(1.3,-0.3)) +
      geom_vline(xintercept = playerStats$shot_per_game[playerStats$player_name == "omri casspi"], color = "purple", size = 1) +
        annotate(geom = 'text', x =playerStats$shot_per_game[playerStats$player_name == "omri casspi"] , y = 0, color = 'purple', label = "omri casspi mean", hjust =1, vjust =-1) 

## check normality assumption 
qqnorm(playerStats$shot_per_game)
shapiro.test(playerStats$shot_per_game)

## make a t test 

t.test(playerStats$shot_per_game,mu=playerStats$shot_per_game[playerStats$player_name == "omri casspi"] ,alter = "two.sided", conf = 0.95)

## another plot
ggplot(playerStats,  aes(x =shot_per_game, y= shot_percentage)) +
  geom_point() +
  geom_smooth(method = "lm" ,se =TRUE) +
  scale_x_continuous(name = "mean shot per game per player") +
  scale_y_continuous(name = "shot percentage per player") +
  ggtitle("scatter plot for players \n shot per game & shot percentage")
  cor(playerStats$shot_per_game,playerStats$shot_percentage)
# question 3 ----------------------------------------------------------------
## add an extra variable - over_all_game_clock_time
## over_all_game_clock_time it is the time that passed (seconds) from the start of the match 
## extract game time into minutes and seconds 
## each period is 12 minuets 
## change game_clock into numeric
as.character(shot_logs$GAME_CLOCK)
class(shot_logs$GAME_CLOCK)
over_all_game_clock_time_vector <- ms(shot_logs$GAME_CLOCK) 
minutes_over_all_game_clock_time_vector <- minute(over_all_game_clock_time_vector)
seconds_over_all_game_clock_time_vector <- second(over_all_game_clock_time_vector)
passed_time_in_seconds <- minutes_over_all_game_clock_time_vector *
  60+seconds_over_all_game_clock_time_vector 
  

## check if veritacl is long as data base 
if(length(passed_time_in_seconds)==length(shot_logs$GAME_CLOCK))
{
  print("you can continue")
}else{
  print("ERROR: not the same length, check whats wrong")
}

## add an extra variable in shot_logs
shot_logs<-shot_logs %>% 
    mutate(over_all_game_clock_time = passed_time_in_seconds+(shot_logs$PERIOD-1)*(12*60))

## multivariate assumption
shot_logs_for_multivaraite<-shot_logs[c(which(colnames(shot_logs)=="DRIBBLES"),which(colnames(shot_logs)=="over_all_game_clock_time"))]
mvn(shot_logs_for_multivaraite, mvnTest = "hz")
## do a pearson cor test between time passed and dribbles 
## manuel cor test 
r<-cor(shot_logs$over_all_game_clock_time, shot_logs$DRIBBLES)
N<-length(shot_logs$GAME_ID)
Tvalue<-r*sqrt((N-2)/(1-r**2))
qriticalT<-qt(0.95,N-2)
if(Tvalue>qriticalT==TRUE)
{
  print("reject NULL")
}
## effect size 
r/2

## cor test function 
cor.test(shot_logs$DRIBBLES,shot_logs$over_all_game_clock_time,conf.level = 0.95)

## visualise pearson cor 

ggplot(shot_logs, aes(x= over_all_game_clock_time,y=DRIBBLES)) +
  geom_point(color = "black",size = 3, shape = 1 ) + 
    geom_smooth() +
      stat_binhex()

## find extra statistics 
mean(shot_logs$DRIBBLES)
median(shot_logs$DRIBBLES)
max(shot_logs$DRIBBLES)


# question 4 ----------------------------------------------------------------
## t test for james hardn touch time mean in the condition of shot result
## two independent t test
## turn touch time to a numeric class
as.numeric(shot_logs$TOUCH_TIME)
class(shot_logs$TOUCH_TIME)

## visualise touch time of all the players 

Players_Touch_Time<-group_by(shot_logs, player_name)
Players_Touch_Time_Overall <-summarize(Players_Touch_Time, touch_time = sum(TOUCH_TIME))
Player_Touch_Time_Means<-summarize(Players_Touch_Time, mean_touch_time = mean(TOUCH_TIME))

## plot overall touch time before taking a shot
## with quantilles, mean and median 

QuantilesTouchtime <-quantile(Players_Touch_Time_Overall$touch_time,c(0.25,0.75))
mean((Players_Touch_Time_Overall$touch_time))
median((Players_Touch_Time_Overall$touch_time))

ggplot(Players_Touch_Time_Overall,aes(x = touch_time)) + 
  geom_density(bins = 30, binwidth = 0.2, fill = "light grey", colour = "black") +
    geom_vline(xintercept = QuantilesTouchtime, color = "purple", size = 1) +
      geom_vline(xintercept = mean((Players_Touch_Time_Overall$touch_time)), color = "blue", size = 1) +
        geom_vline(xintercept = median((Players_Touch_Time_Overall$touch_time)), color = "dark green", size = 1) 
          
        
## check t test assumptions
## both distrabutions are normal 
qqnorm(shot_logs$TOUCH_TIME[shot_logs$player_name=="james harden" & shot_logs$SHOT_RESULT == "made"])
qqline(shot_logs$TOUCH_TIME[shot_logs$player_name=="james harden" & shot_logs$SHOT_RESULT == "made"])

qqnorm(shot_logs$TOUCH_TIME[shot_logs$player_name=="james harden" & shot_logs$SHOT_RESULT == "missed"])
qqline(shot_logs$TOUCH_TIME[shot_logs$player_name=="james harden" & shot_logs$SHOT_RESULT == "missed"])

shapiro.test(shot_logs$TOUCH_TIME[shot_logs$player_name=="james harden" & shot_logs$SHOT_RESULT == "made"])
shapiro.test(shot_logs$TOUCH_TIME[shot_logs$player_name=="james harden" & shot_logs$SHOT_RESULT == "missed"])

## ditrabutions variances are normal 
Ftest<-var.test(shot_logs$TOUCH_TIME[shot_logs$player_name=="james harden" & shot_logs$SHOT_RESULT == "missed"],
         shot_logs$TOUCH_TIME[shot_logs$player_name=="james harden" & shot_logs$SHOT_RESULT == "made"],
          alternative = "two.sided")
Ftest

## start t test 

tValue<-t.test(shot_logs$TOUCH_TIME[shot_logs$player_name=="james harden"] ~shot_logs$SHOT_RESULT[shot_logs$player_name=="james harden"])
tValue

boxplot(shot_logs$TOUCH_TIME[shot_logs$player_name=="james harden"] ~shot_logs$SHOT_RESULT[shot_logs$player_name=="james harden"])

## plot t test for james harden 
CiLow <- -qt(0.975, length(shot_logs$TOUCH_TIME[shot_logs$player_name=="james harden"])-2)
CiHigh <- qt(0.975, length(shot_logs$TOUCH_TIME[shot_logs$player_name=="james harden"])-2)

t.values <- seq(-4,4,.1)
plot(x = t.values,y = dt(t.values,length(shot_logs$TOUCH_TIME[shot_logs$player_name=="james harden"])-2), type = "l", lty = "dotted", ylim = c(0,.4), xlab = "t", ylab = "f(t)") +
  abline(v = CiLow, col="blue") +
  abline(v = CiHigh, col="blue") + 
  abline(v = tValue$statistic, col="red")

## cohen D
VectorMade <- shot_logs$TOUCH_TIME[shot_logs$player_name=="james harden" & shot_logs$SHOT_RESULT == "made"]
vectorMissed  <- shot_logs$TOUCH_TIME[shot_logs$player_name=="james harden" & shot_logs$SHOT_RESULT == "missed"]
## collect means
meanMade <- mean(VectorMade)
meanMissed <-mean(vectorMissed)

## collect var
varMade <- var(VectorMade)
varMissed <- var(vectorMissed)

sdMade<- sd(VectorMade)
sdMissed <-sd(vectorMissed)

## collect length

lengthMade<- length(VectorMade)
LengthMissed <- length(vectorMissed)

##make ditrabution variance 
CohenDvar<-sqrt((varMade*lengthMade+varMissed*LengthMissed)/(lengthMade+LengthMissed-2))


(0-(meanMissed-meanMade)/CohenDvar)
(0-(meanMissed-meanMade)/CohenDvar)/2


# question 5 ----------------------------------------------------------------
## chi squared for 2/3 pointers and periods 
## filter only for four periods of play (regular time)
FourPeriod<-filter(shot_logs, PERIOD==1|PERIOD == 2|PERIOD == 3|PERIOD == 4)
## add it to a table format
tab<-table(FourPeriod$PERIOD,FourPeriod$PTS_TYPE)
## do chisq test
chisq.test(tab)
chisq<-chisq.test(tab)
## plot cor plot

corrplot(chisq$residuals, is.cor = FALSE)

## visualise proportion

p <-  ggplot(FourPeriod,aes(x=factor(PERIOD),fill=factor(PTS_TYPE)))+geom_bar(position = "fill") +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count',position=position_fill(vjust=0.5))

p




