# K-EN 
Elastic Net-based high dimensional data selection for regression
# Synopsis 
hybrid feature selection approach that integrates feature screening based on Kendall's tau and Elastic Net regularized regression (K-EN). The K-EN approach offers insightful solutions to high-dimensional regression problems and enhances elastic net performance.  
# What is K-EN

This is a package implementing the K-EN approach, an efficient hybrid  method aiming to select relevant features to the regression task in the high dimensional data frame. The K-EN  is presented in the paper "Elastic Net-based high dimensional data selection for regression". It has several advantages:

        . It is model-free.

        . It has the ability to process both continuous and categorical features.

        . It has the sure screening property.

        . Production of a sparse model with less number of  non-zero coefficients in the   elastic net model.

        . It is robust against heavy-tailed distributions.
 
The K-EN package implements two methods, namely K-EN.Filter and KEN.Embedded. The K-EN.Filter method takes the explanatory variables as input and returns the [n/(log⁡(n))]  (n=sample size) most relevant features based on their K-EN-Filter score as output, while the K-EN.Embedded method takes the KEN-Filter output dataset as input and returns the most relevant features that maximize the performance of the Elastic Net model as output.

# Installation

		

		## Loading required package : mvtnorm

				
            ## Loading required package : mvtnorm

      	## Loading required package : Kendall
		## Loading required package : SimDesign
		## Loading required package : glmnet
		## Loading required package : dplyr
            ## Loading required package : FactoMineR
# Quick start example
## Simulated Scenario 
a 70×300 predictors matrix X is generated, along with a continuous response variable
Y of length n = 70 defined as:

library(mvtnorm)

for (r in 1:100){

  r = 0.2#0.4#0.6

  Sig = diag(300)

  Sig = r^abs(row(Sig)-col(Sig))

  x <- rmvnorm(n = 70, mean = rep(0,300), Sig)

  for (i in 1:70) {

    eps[i]= rnorm(1, mean=0, sd=0.05)

    y[i] <- .20*x[i,1]+.20*x[i,2]+.20*x[i,3]+.20*x[i,1]*x[i,2]+.20*x[i,2]*x[i,3]+eps[i]

  }
  
In this case, a threshold K equal to [n/log(n)] is chosen as the number of selected features in the K-EN filter phase.

library(FactoMineR)

library(Kendall)

library(SimDesign)

library(glmnet)

library(dplyr)

 ##This function returns the Kendall's tau for two given vectors u,v

kk <- function(u,v){

  kk= Kendall(u,v)$tau[1]

  return(kk)
}

 ##This function returns the indexes of relevant features

my_funv <- function(x, thresholdm){

  #thresholdm <- ceiling(n/log(n))

  MaXcore <- rep(0,thresholdm)

  for(i in 1:thresholdm){

    MaXcore[i]= as.vector(which(x==max(x), arr.ind = T))

    x[MaXcore[i]] = 0
  }

  ##return the selected feature indexes
  MaXcore
}


 ##100 replications


cv_errors_2=c()

Kcor_21= c()

sel_21a= c()

L_fits21= list()

data= list()

ma_li21= list()
 
 ##K-EN.Filter
 
  for (j in 1:300) {
 
    Kcor_21[j] = kk(as.vector(x[,j]), as.vector(y))
 
  }
 
  sel_21a= my_funv(Kcor_21,ceiling(70/log(70)))
 
  x_nw21 <- dplyr::select(as.data.frame(x),all_of(sel_21a))
 
  data$x= as.matrix(x_nw21)
 
  data$y= y
 
  data=list(data)
 
  #PARAMETER SETTINGS
 
  for (i in 1:length(alpha_values)) {
 
    alpha <- alpha_values[i]
 
    model <- cv.glmnet(data[[1]]$x, data[[1]]$y, alpha = alpha)
 
    cv_errors_2[i] <- min(model$cvm)
 
  }
 
  data[[1]]$alpha_min= alpha_values[which.min(cv_errors_2)]
 
  ma_li21[[(length(ma_li21) + 1)]]<- data[[1]]#each element of ma_li21 corresponds to a sample (X,Y)

  data= list()# clean the data object, to generate new data in the next iteration
}

 #Then K-EN.Embedded is run on the K-EN.Filter output data 

 #step 2: K-EN.Embedded

L_fits21 = ma_li21 %>% Map(f = function(d) glmnet(x = d$x, y = d$y, 
                                                  alpha = d$alpha_min))

L_coefs = L_fits21 %>% Map(f = function(model) coef(model, s = .1))

liste_nonzero2 =L_coefs %>%
  Map(f = function(matr) matr %>% as.matrix() %>%
        as.data.frame() %>%
        filter(s1 != 0) %>% 
        rownames()
  )

 #transform the list to a vector of characters

my_vector2 <- unlist(liste_nonzero2)

 #count the occurrences of each element

my_table2 <- table(my_vector2)

The K-EN returns the frequency of active features namely V1, V2 and V3

V1:100%, V2:99% and V3:100%


# Authors 
Hansna Chamlal, Asmaa Bezmane and Tayeb Ouaderhman




