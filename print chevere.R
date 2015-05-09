#Load packages

library(devtools)
library(httr)
library(twitteR)

#------------------------------------------------------------------#
#Accessing twitter
api_key <- "CY9yGxL0lv84ZrHI6rOQCbNOI"

api_secret <- "iRkslgkkHavoeSwBeQDpugTA5gUgJWaXL66rSAU7cJ3ODicoWN"

access_token <- "45983979-IrSB5XkJAITuYaEoHKEtVb1MoxzihrJ63zwSp2KHl"

access_token_secret <- "Rx8mE0EjNvdph9k0caWVYUi9iRN9ZG4KV96M9wd4DCW8G"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#---------------------------------------------------------------#
#Get user
user <- getUser("moracarlos") #Set the username
userFriends <- user$getFriends()
userFollowers <- user$getFollowers()
userNeighbors <- union(userFriends, userFollowers)

userNeighbors.df = twListToDF(userNeighbors)
userNeighbors.df[userNeighbors.df=="0"]<-1
userNeighbors.df$logFollowersCount <-log(userNeighbors.df$followersCount)
userNeighbors.df$logFriendsCount <-log(userNeighbors.df$friendsCount)
userNeighbors.df$logStatusesCount <-log(userNeighbors.df$statusesCount)

userNeighbors.df$logFavoritesCount <-log(userNeighbors.df$favoritesCount, )

kObject.log <- data.frame(userNeighbors.df$logFriendsCount,userNeighbors.df$logFollowersCount, userNeighbors.df$logFavoritesCount) 
mydata <- kObject.log

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
#Run the K Means algorithm, specifying 4 centers
user2Means.log <- kmeans(kObject.log, centers=4, iter.max=10, nstart=100)

#Add the vector of specified clusters back to the original vector as a factor

userNeighbors.df$cluster <- factor(user2Means.log$cluster)
library(RCurl)
library(rCharts)


p2 <- nPlot(logFollowersCount ~ logFriendsCount, group = 'cluster', data = userNeighbors.df, type = 'scatterChart')

p2$xAxis(axisLabel = 'Followers Count')

p2$yAxis(axisLabel = 'Friends Count')

p2$chart(tooltipContent = "#! function(key, x, y, e){
return e.point.screenName + ' Followers: ' + e.point.followersCount +' Friends: ' + e.point.friendsCount
} !#")

p2
