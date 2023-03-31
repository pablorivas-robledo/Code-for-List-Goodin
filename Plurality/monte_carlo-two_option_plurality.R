library(gtools)
library(RcppAlgos)

simulation_plurality_rule = function(number_of_voters, number_of_options, mean_competence){
  n_sim = 100
  
  results = c()
  
  for (sim in 1:n_sim) {
    print(sim)
    set.seed(sim)
    
    voters = rnorm(number_of_voters, mean = mean_competence, sd = 0.1)
    options = number_of_options
    
    winning = ceiling((number_of_voters + 1)/number_of_options)
    
    difference = number_of_voters-winning
    
    df = compositionsGeneral(0:difference, options, repetition = TRUE, weak = TRUE)
    
    df[,1] = df[,1]+winning
    
    prob = 0
    
    for (i in 1:nrow(df)) {
      combs = combinations(n = number_of_voters, r = df[i,1], v = voters)
      
      products = c()
      
      for (row in 1:nrow(combs)) {
        not_in = voters[!voters %in% combs[row,]]
        
        mistake = rep(1, length(not_in))
        
        mistake = mistake - not_in
        
        products = c(products, prod(combs[row,])*prod(mistake))
      }
      case = sum(products)
      
      prob = prob + case
    }
    results = c(results, prob)
  }
  
  final = mean(results)
  return(final)
}

number_of_voters = 11
number_of_options = 2
mean_competence = 0.6
simulation_plurality_rule(number_of_voters, number_of_options, mean_competence)


