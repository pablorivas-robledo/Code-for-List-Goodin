
library(RcppAlgos)
library(data.table)

# function plurality_rule
# input: a non negative integer number of voters and
#        a vector with probability mass function for options
# output: probability that first option wins under plurality rule

plurality_rule <- function (total.voters, options) { 
  
  num.options = length(options)
  
  # Calculating how many votes are neede to win
  ## Standard. Comment for two options
  winning = ceiling(total.voters/num.options) + 1
  
  ## Un-commment for two options
  # winning = ceiling((total.voters + 1)/num.options)
  
  difference = total.voters-winning
  
  df = compositionsGeneral(0:difference, length(options), repetition = TRUE, weak = TRUE)
  
  df[,1] = df[,1]+winning
  
  #Filtering cases in which the first option is the winning options with a general algorithm 
  
  ## Standard. Comment and choose one of the above to improve speed
  preferred = c()
  for (option in 2:length(options)) {
    most_a = which(df[,1] > df[,option])
    if (option == 2) {
      preferred = c(preferred, most_a)
    } else {
      preferred = intersect(preferred, most_a)
    }
  }
  
  ########################################################################
  #Filtering cases in which the first option is the winning options with an specific algorithm 
  
  #Un-comment for two options
  #preferred = which(df[,1] > df[,2])
  
  #Un-comment for three options
  #preferred = intersect(which(df[,1] > df[,2]), which(df[,1] > df[,3]))
  
  #Un-comment for four options
  # preferred_b = intersect(which(df[,1] > df[,2]), which(df[,1] > df[,3]))
  # preferred = intersect(preferred_b, which(df[,1] > df[,4]))
    
  #Un-comment for five options
  # preferred_b = intersect(which(df[,1] > df[,2]), which(df[,1] > df[,3]))
  # preffered_c = intersect(which(df[,1] > df[,4]), which(df[,1] > df[,5]))
  # preferred = intersect(preferred_b, preffered_c)
  
  ########################################################################
  
  
  # Choosing only the cases where the first option wins
  df = df[preferred,]
  
  df=as.data.frame(df)
  rows = nrow(df)
  
  #convert the data.frame to a data.table
  table = setDT(df)
  
  #put the data in long format
  table = data.table::melt(df, measure.vars = names(df))
  
  table[, group := rep(1:rows, num.options)]
  
  #apply function to each group
  table[, probability := dmultinom(value, prob = options), by = "group"]
  
  whole.group = head(table, rows)
  prob = sum(whole.group$probability)
  
  return(prob)
  
}

#Example to run the function

##voters
total.voters = 11
##options
options = c(0.5, 0.3, 0.2)
## one call of the function 
plurality_rule(total.voters, options)

#reproducing L&G

cases = c(11, 31, 51, 76, 101, 201, 301, 451, 601)
options = c(0.5, 0.3, 0.1, 0.1)
num.options = length(options)
repro = c()
for (i in cases){
  repro = c(repro, plurality_rule(i, options))
  print(i)
}
repro


model = lm(repro ~ log(cases))
summary(model)

# using logarithmic regression to calculate the function

model = lm(p ~ log(values))
summary(model)

#poltnomial regression



# repeated calls of the functions changing the number of voters

options = c(0.5, 0.3, 0.2)
num.options = length(options)
min_total.voters <- 5
max_total_voters <- 13
values = seq(min_total.voters, max_total_voters)

# arriving at 0.999...
max = 0.999
calc = 0
options =  c(0.35, 0.2, 0.15, 0.15, 0.15)

start = 210

while (max - calc > 0.00001) {
  calc = plurality_rule(start , options)
  print(start)
  print(calc)
  start = start + 1
}


# computing the winning probabilities
p = c()
for (i in values){  
  p<- c(p, plurality_rule(i , options) )
}
  

# plotting the vector of winning probabilities against number of voters

plot( values, p , 
      type = 'l', xlab="Number of voters", ylab = "Collective Competence",
      ylim = c(0.3, 1) 
)
title( "probability that first option wins" )
abline( v=min_total.voters )


#export data
save.image(file = "R_Objects.RData")

