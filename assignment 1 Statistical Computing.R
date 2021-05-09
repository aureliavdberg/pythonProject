# Question 3.1
set.seed(8052021)
x = rnorm(5,mean=0, sd=sqrt(10))

Maximum_likelihood = var(x)
unbiased = var(x) * (length(x) /(length(x)-1))
minMSE = var(x) * (length(x) /(length(x)+1))

pop_value = 10

diff_unbiased = abs(pop_value - unbiased)
diff_minMSE = abs(pop_value - minMSE)
diff_max_likelihood = abs(pop_value - Maximum_likelihood)

table = data.frame(estimates=c(Maximum_likelihood, unbiased, minMSE), difference=c(diff_max_likelihood, diff_unbiased, diff_minMSE))
row.names(table) = c('Maximum likelihood', 'unbiased', 'minMSE')


# Question 3.2
S = 10000

# n =5
out = list(mode='vector',length=S)
S_max_likelihood = vector(length=S)
S_unbiased = vector(length=S)
S_minMSE = vector(length=S)

squared_diff_max = vector(length=S)
squared_diff_unbiased = vector(length=S)
squared_diff_minMSE = vector(length=S)

for (i in 1:S)
{
  out[[i]] = rnorm(5, mean=0, sd=sqrt(10))
  S_max_likelihood[i] = var(out[[i]])
  S_unbiased[i] = var(out[[i]]) * (length(out[[i]]) /(length(out[[i]])-1))
  S_minMSE[i] = var(out[[i]]) * (length(out[[i]]) /(length(out[[i]])+1))

  squared_diff_max[i] = (pop_value - S_max_likelihood[i])^2
  squared_diff_unbiased[i] = (pop_value - S_unbiased[i])^2
  squared_diff_minMSE[i]= (pop_value - S_minMSE[i])^2
}

# n = 10
S_max_likelihood10 = vector(length=S)
S_unbiased10 = vector(length=S)
S_minMSE10 = vector(length=S)
out10 = list(mode='vector',length=S)

squared_diff_max10 = vector(length=S)
squared_diff_unbiased10 = vector(length=S)
squared_diff_minMSE10 = vector(length=S)
for (i in 1:S)
{
  out10[[i]] = rnorm(10, mean=0, sd=sqrt(10))
  S_max_likelihood10[i] = var(out10[[i]])
  S_unbiased10[i] = var(out10[[i]]) * (length(out10[[i]]) /(length(out10[[i]])-1))
  S_minMSE10[i] = var(out10[[i]]) * (length(out10[[i]]) /(length(out10[[i]])+1))

  squared_diff_max10[i] = (pop_value - S_max_likelihood10[i])^2
  squared_diff_unbiased10[i] = (pop_value - S_unbiased10[i])^2
  squared_diff_minMSE10[i]= (pop_value - S_minMSE10[i])^2
}

# n = 100
S_max_likelihood100 = vector(length=S)
S_unbiased100 = vector(length=S)
S_minMSE100 = vector(length=S)
out100 = list(mode='vector',length=S)


squared_diff_max100 = vector(length=S)
squared_diff_unbiased100 = vector(length=S)
squared_diff_minMSE100 = vector(length=S)

for (i in 1:S)
{
  out100[[i]] = rnorm(100, mean=0, sd=sqrt(10))
  S_max_likelihood100[i] = var(out100[[i]])
  S_unbiased100[i] = var(out100[[i]]) * (length(out100[[i]]) /(length(out100[[i]])-1))
  S_minMSE100[i] = var(out100[[i]]) * (length(out100[[i]]) /(length(out100[[i]])+1))
  
  squared_diff_max100[i] = (pop_value - S_max_likelihood100[i])^2
  squared_diff_unbiased100[i] = (pop_value - S_unbiased100[i])^2
  squared_diff_minMSE100[i]= (pop_value - S_minMSE100[i])^2
}

table1 = data.frame(max_likelihood_n_5=c(mean(S_max_likelihood), abs(pop_value - mean(S_max_likelihood)), var(S_max_likelihood), (mean(squared_diff_max))))
table1$unbiased_n_5 = c(mean(S_unbiased), abs(pop_value - mean(S_unbiased)), var(S_unbiased), (mean(squared_diff_unbiased)))
table1$minMSE_n_5 = c(mean(S_minMSE), abs(pop_value - mean(S_minMSE)), var(S_minMSE), (mean(squared_diff_minMSE))) 

table1$max_likelihood_n_10 = c(mean(S_max_likelihood10), abs(pop_value - mean(S_max_likelihood10)), var(S_max_likelihood10), (mean(squared_diff_max10)))
table1$unbiased_n_10 = c(mean(S_unbiased10), abs(pop_value - mean(S_unbiased10)), var(S_unbiased10), (mean(squared_diff_unbiased10)))
table1$minMSE_n_10 = c(mean(S_minMSE10), abs(pop_value - mean(S_minMSE10)), var(S_minMSE10), (mean(squared_diff_minMSE10)))                                    
                                  
table1$max_likelihood_n_100 = c(mean(S_max_likelihood100), abs((pop_value - mean(S_max_likelihood100))), var(S_max_likelihood100), (mean(squared_diff_max100)))
table1$unbiased_n_100 = c(mean(S_unbiased100), abs(pop_value - mean(S_unbiased100)), var(S_unbiased100), (mean(squared_diff_unbiased100)))
table1$minMSE_n_100 = c(mean(S_minMSE100), abs(pop_value - mean(S_minMSE100)), var(S_minMSE100), (mean(squared_diff_minMSE100)))                                    

row.names(table1) = c('average value', 'bias', 'variance', 'squared diff')