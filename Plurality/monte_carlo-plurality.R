library(gtools)

# function simulation_plurality_rule
# input: a non negative integer number of voters,
#        a vector with probability mass function for options
#        a numeric object for the mean competence of the group of voters
# output: probability that first option wins under plurality rule


simulation_plurality_rule = function(number_of_voters, number_of_options, mean_competence){
  n_sim = 100

  results = c()
  
  for (sim in 1:n_sim) {
    print(sim)
    set.seed(sim)
    voters = rnorm(number_of_voters, mean = mean_competence, sd = 0.1)
    options = number_of_options
    
    #Un-comment for three or more options
    winning = ceiling(number_of_voters/number_of_options) + 1
    
    # Un-commment for two options
    # winning = ceiling((number_of_voters + 1)/number_of_options)
    
    difference = number_of_voters-winning
    
    df = compositionsGeneral(0:difference, options, repetition = TRUE, weak = TRUE)
    
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
    
    # Choosing only the cases where the first option wins
    df = df[preferred,]
    ########################################################################
    
    
    prob = 0
    
    for (i in winning:number_of_voters) {
      combs = combinations(n = number_of_voters, r = i, v = voters)
      
      products = 0
      
      for (row in 1:nrow(combs)) {
        not_in = voters[!voters %in% combs[row,]]
        
        mistake = rep(1, length(not_in))
        
        mistake = mistake - not_in
        
        products = c(products, prod(combs[row,])*prod(mistake))
      }
      case = sum(products)
      
      prob = prob + case
    }
    
    prob
    
    results = c(results, prob)
    
    gc()
  
  }
  
  final = mean(results)
  return(final)
}

voters = 11
options = 3
mean_competence = 0.5
simulation_plurality_rule(voters, options, mean_competence)

# repeated calls of the functions changing the number of voters

options = c(0.5, 0.3, 0.2)
num.options = length(options)
min_total.voters <- 5
max_total_voters <- 13
values = seq(min_total.voters, max_total_voters)

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
title( "Probability that the first option wins" )
abline( v=min_total.voters )


library(gridExtra)

data_for_table = data.frame(values, p)
colnames(data_for_table) = c("Voters", "P(Majn)")

png("data.png", height = 50*nrow(data_for_table), width = 200*ncol(data_for_table))
grid.table(data_for_table)
dev.off()

