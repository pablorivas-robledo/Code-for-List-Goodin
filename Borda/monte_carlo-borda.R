library(gtools)

# function simulation_plurality_rule
# input: a non negative integer number of voters,
#        a vector with probability mass function for options
#        a numeric object for the mean competence of the group of voters
# output: probability that first option wins under plurality rule


simulation_borda = function(number_of_voters, number_of_options, mean_competence){
  n_sim = 100
  
  results = c()
  
  for (sim in 1:n_sim) {
    print(sim)
    set.seed(sim)
    voters = rnorm(number_of_voters, mean = mean_competence, sd = 0.1)
    options = number_of_options
    
  
    
    
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

