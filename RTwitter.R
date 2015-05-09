#install packages
install.packages(c("devtools", "rjson", "bit64"))
#RESTART R session!
install_github("hadley/httr") #Version 0.6.1
install_github("geoffjentry/twitteR") #Lastest version

#------------------------------------------------------------------#
#Load packages

library(devtools)
library(httr)
library(twitteR)
library(plyr)

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

df1 = twListToDF(userFriends)
relationship = rep(1, times=length(userFriends))
df1 = cbind(df1, relationship)

df2 = twListToDF(userFollowers)
relationship = rep(2, times=length(userFollowers))
df2 = cbind(df2, relationship)

userNeighbors.df = merge(x = df1, y= df2, all=TRUE)

#---------------------------------------------------------------#
#Clean user
userNeighbors.subset <- subset(userNeighbors.df, select = c('id','statusesCount', 'followersCount', 'favoritesCount', 'friendsCount', 'created', 'verified', 'relationship'))

usersLog <- subset(userNeighbors.df, select = c('id','statusesCount', 'followersCount', 'favoritesCount', 'friendsCount', 'created', 'verified', 'relationship'))

#---------------------------------------------------------------#
#Save values

write.csv2(userNeighbors.subset, "23595856_23696989_22671778_twitter_moracarlos.csv")


#---------------------------------------------------------------#
#Time
usersLog$created <- sapply (usersLog$created, function(x) difftime(Sys.time(), x))
#Verified
usersLog$verified <- sapply (usersLog$verified, function(x) if (x==FALSE) {x=1} else{ x=2})
#Replace zero
usersLog$statusesCount <- sapply (usersLog$statusesCount, function(x) if (x==0) {x=1}else{x=x})
usersLog$followersCount <- sapply (usersLog$followersCount, function(x) if (x==0) {x=1}else{x=x})
usersLog$favoritesCount <- sapply (usersLog$favoritesCount, function(x) if (x==0) {x=1}else{x=x})
usersLog$friendsCount <- sapply (usersLog$friendsCount, function(x) if (x==0) {x=1}else{x=x})

#---------------------------------------------------------------#
#log(e)
usersLog$statusesCount <- sapply (usersLog$statusesCount, function(x) log(x, base=exp(1)))
usersLog$followersCount <- sapply (usersLog$followersCount, function(x) log(x, base=exp(1)))
usersLog$favoritesCount <- sapply (usersLog$favoritesCount, function(x) log(x, base=exp(1)))
usersLog$friendsCount <- sapply (usersLog$friendsCount, function(x) log(x, base=exp(1)))
usersLog$created <- sapply(usersLog$created, function(x) log(x, base=exp(1)))
usersLog$verified <- sapply (usersLog$verified, function(x) log(x, base=exp(1)))
usersLog$relationship <- sapply (usersLog$relationship, function(x) log(x, base=exp(1)))

#---------------------------------------------------------------#
#Write

write.csv2(usersLog, "23595856_23696989_22671778_twitter_moracarlos_logaritmo.csv")

#---------------------------------------------------------------#
#Codo de Jambu
mydata <- subset(usersLog, select = 2:8)

InerciaIC = rep(0, 30)
for (k in 1:30) {
  grupos = kmeans(mydata, k)
  InerciaIC[k] = grupos$tot.withinss
}
plot(InerciaIC, col = "blue", type = "b")

means <- kmeans(mydata, centers=4, iter.max=10, nstart=100)
plot(means$centers)
userNeighbors.subset$cluster <- factor(means$cluster)
usersLog$cluster <- factor(means$cluster)

write.csv(userNeighbors.subset, "23595856_23696989_22671778_twitter_moracarlos_grupos_stats.csv")
write.csv(usersLog, "23595856_23696989_22671778_twitter_moracarlos_grupos_logaritmo_stats.csv")

usersByGroup = count(usersLog$cluster)
porc = lapply (usersByGroup[,2], function(x) (x*100)/length(usersLog$cluster))
usersByGroup
porc
