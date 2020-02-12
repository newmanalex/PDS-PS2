#Alex Newman Problem Set 2

#Problem 1.

#Iterate from 1 to 7 and print the cube of each number
for (i in 1:7) {
  print(i^3)
}

#problem 2

#set the seed
set.seed(14)
#initiate casts, the vector that will contain each game's number of dice casts
casts<-NULL
#simulate two dice being rolled in game 1000 times

for(iterator in 1:1000){
  roll<-sample(x=1:6, size = 2, replace=TRUE)
  rollnum<-1
  #end game immediately if dice sum totals to certain values
  if(sum(roll)%in% c(8,9,10,11,12)){
    casts<-c(casts, rollnum)
    next
  }
  #else roll dice until either a 2 or 6 is rolled
  repeat{
    roll<-sample(x=1:6, size = 2, replace=TRUE)
    rollnum<-rollnum+1
    if(any(roll==2|roll==6)){
      casts<-c(casts, rollnum)
      break
    }
  }
}

#find average number of dice casts per game
mean(casts)

#problem 3

#read the dataset in
df<-read.csv("http://politicaldatascience.com/PDS/Problem%20Sets/Problem%20Set%202/GSS-data.csv")
#create function vote.choice
vote.choice<- function(candidate){
  #return trump vote count
   if(candidate=="Trump"){
    return(length(which(df$pres16=="Trump")))
  }
  #return clinton vote count
  if(candidate=="Clinton"){
    return(length(which(df$pres16=="Clinton")))
  }
  #return other vote count
  if(candidate=="Other"){
    return(length(which(df$pres16=="Other candidate (specify)")))
  }
  #handle invalid inputs
  return("Please input either Trump, Clinton, or Other")
}
