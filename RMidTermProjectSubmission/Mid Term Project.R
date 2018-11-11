#import datasets RegularSeasonDetailedResults and TourneyCompactResults
dat <- RegularSeasonDetailedResults
dat2 <- subset(TourneyCompactResults, TourneyCompactResults$Season >= 2003)
dim(dat)

############## SPLITTING DATASETS ##############
str2 <- names(dat)
#creating a single dataset with team names and season
w_team <- dat 
l_team <- dat
for (i in str2){
  if(startsWith(i, "L")){
    w_team[i] <- NULL
  }
  if(startsWith(i, "W") & i != "Wloc" ){
    l_team[i] <- NULL
  }
}
head(l_team)
head(w_team)
names(w_team) <- c("Season", "Daynum", "Team", "score", "Wloc", "Numot", "fgm", "fga", "fgm3", "fga3", "ftm","fta", "or", "dr", "ast",
             "to", "stl", "blk", "pf")
names(l_team) <- c("Season", "Daynum", "Team", "score", "Wloc", "Numot", "fgm", "fga", "fgm3", "fga3", "ftm","fta", "or", "dr", "ast",
                   "to", "stl", "blk", "pf")
names(l_team)
teams <- rbind(w_team, l_team)
teamsreg <- teams
#teams
teams$Daynum <- NULL
teams$score <- NULL
teams$Wloc <- NULL
teams$Numot <- NULL
names(teams)

############## AVERAGE VALUES ##################
#finding average statistic values
teams <- teams[order(teams$Team, teams$Season),]
#teams
ans <- data.frame()
#ans
i = 1101
for(i in Teams$Team_Id){
  team_ind <- subset(teams, teams$Team == i)
  season = unique(team_ind$Season)
  for(j in 2003:2017){
    if(any(j %in% season)){
    team_ind <- subset(teams, teams$Team == i & teams$Season == j)
    s_avg <- (apply(team_ind, 2, mean))
    ans <- rbind(ans,s_avg)
    }
  }
}

names(ans) <- c("Season",  "Team",  "fgm", "fga", "fgm3", "fga3", "ftm","fta", "or", "dr", "ast",
                   "to", "stl", "blk", "pf")
head(ans)

############## CLUSTERING #############
############## K MEANS ################

summary(ans)
#ncol(ans)
team_clus <- kmeans(ans[,3:13], 3)
summary(team_clus)
team_clus$totss
team_clus$withinss
team_clus$tot.withinss
team_clus$betweenss

# Elbow Method
teams.scaled <- scale(ans[, -c(1,2)])
summary(teams.scaled)
set.seed(123)

# Compute and plot wss for k = 2 to k = 15
k.max <- 15
wss <- sapply(1:k.max,
              function(k){kmeans(teams.scaled, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

############## HIERARCHICAL CLUSTERING ################

t.complete = hclust(dist(teams.scaled), method="complete")
t.average = hclust(dist(teams.scaled), method="average")
t.single = hclust(dist(teams.scaled), method="single")

plot(t.complete,main="Complete Linkage", xlab="", sub="", cex=.9,labels = FALSE)
plot(t.average, main="Average Linkage", xlab="", sub="", cex=.9,labels = FALSE)
plot(t.single, main="Single Linkage", xlab="", sub="", cex=.9,labels = FALSE)

############## REGRESSION ##############
teamsreg
teamsreg$Daynum <- NULL
teamsreg$Wloc <- NULL
teamsreg$Numot <- NULL
names(teamsreg)
teamsreg <- teamsreg[order(teamsreg$Team, teamsreg$Season),]
#teamsreg
reg <- data.frame()
#reg
i = 1101
for(i in Teams$Team_Id){
  team_indreg <- subset(teamsreg, teamsreg$Team == i)
  seasonreg = unique(team_indreg$Season)
  for(j in 2003:2017){
    if(any(j %in% seasonreg)){
      team_indreg <- subset(teamsreg, teamsreg$Team == i & teamsreg$Season == j)
      s_avgreg <- (apply(team_indreg, 2, mean))
      reg <- rbind(reg,s_avgreg)
    }
  }
}

names(reg) <- c("Season",  "Team", "score", "fgm", "fga", "fgm3", "fga3", "ftm","fta", "or", "dr", "ast",
                "to", "stl", "blk", "pf")
head(reg)

#splitting dataset into training and testing datasets
#attach package caTools
sampledData <- sample.split(reg$score, SplitRatio = 0.80)
training_set <- subset(reg, sampledData == TRUE)
testing_set <- subset(reg, sampledData == FALSE)

#fitting the model and reviewing
mlr.model <- lm(formula = score ~ ., data = training_set)
summary(mlr.model)

#making prediction on the testing dataset
testing_preds <- predict(mlr.model, newdata = testing_set)
#ncol(testing_set)

is.matrix(testing_preds)
test<-t(testing_preds)
#nrow(test)
#ncol(test)
test1<-t(test)
#nrow(test1)
#test1
res=testing_set[,c("Season","Team")]
output=cbind(res,test1)
#output
names(output)=c("Season","Team","Score")
#output
#output.csv contains the score predictions
write.csv(output,file="output.csv")

#Evaluating the model/predictions
#without cross validation
rmse <- function(y, yhat) sqrt(mean((y-yhat)^2)) #to compute RMSE
rmse(testing_set$score, test1)

#with cross validation
glm.fit = glm(score~., data=reg)
summary(glm.fit)
plot(glm.fit$residuals~glm.fit$fitted.values)
cv.out=cv.glm(reg,glm.fit,K=10)
cv.out$delta[1]
cv.out$delta[2]
