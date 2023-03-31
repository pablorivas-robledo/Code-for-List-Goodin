library(gtools)
library(RcppAlgos)


simulation_plurality_rule_plus_2 = function(number_of_voters, number_of_options, mean_competence){
  n_sim = 100
  
  results = c()
  
  for (sim in 1:n_sim) {
    print(sim)
    set.seed(1)
    
    voters = rnorm(number_of_voters, mean = mean_competence, sd = 0.1)
    options = number_of_options
    
    winning = ceiling((number_of_voters)/number_of_options) + 1
    
    difference = number_of_voters-winning
    
    df = compositionsGeneral(0:difference, options, repetition = TRUE, weak = TRUE)
    
    df[,1] = df[,1]+winning
    
    ########################################################################
    #Filtering cases in which the first option is the winning options with an specific algorithm 
    
    #Un-comment for three options
    preferred = intersect(which(df[,1] > df[,2]), which(df[,1] > df[,3]))
    
    #Un-comment for four options
    # preferred_b = intersect(which(df[,1] > df[,2]), which(df[,1] > df[,3]))
    # preferred = intersect(preferred_b, which(df[,1] > df[,4]))
    
    #Un-comment for five options
    # preferred_b = intersect(which(df[,1] > df[,2]), which(df[,1] > df[,3]))
    # preffered_c = intersect(which(df[,1] > df[,4]), which(df[,1] > df[,5]))
    # preferred = intersect(preferred_b, preffered_c)
    
    ########################################################################
    
    df = df[preferred,]
    
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
number_of_options = 3
mean_competence = 0.5
simulation_plurality_rule_plus_2(number_of_voters, number_of_options, mean_competence)
