library(mvtnorm)
library(FactoMineR)
library(Kendall)
library(SimDesign)
library(glmnet)
library(dplyr)
############################# Functions ############################## 
#This function returns the Kendall's tau for two given vectors u,v
kk <- function(u,v){
  kk= Kendall(u,v)$tau[1]
  return(kk)
}

#This function returns the indexes of relevant features
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

########################## Simulated data ############################

#100 replications


cv_errors_2=c()
Kcor_21= c()
sel_21a= c()
L_fits21= list()
data= list()
ma_li21= list()
y=rep(0,70)
eps=rep(0,70)
alpha_values <- seq(0.01, 0.99, by = 0.01)
cv_errors <- numeric(length(alpha_values))
for (r in 1:100){
  r = 0.2#0.4#0.6
  Sig = diag(300)
  Sig = r^abs(row(Sig)-col(Sig))
  x <- rmvnorm(n = 70, mean = rep(0,300), Sig)
  for (i in 1:70) {
    eps[i]= rnorm(1, mean=0, sd=0.05)
    #marginal and interaction effects
    y[i] <- .20*x[i,1]+.20*x[i,2]+.20*x[i,3]+.20*x[i,1]*x[i,2]+.20*x[i,2]*x[i,3]+eps[i]
  }
  #Step 1
  ###########K-EN.Filter##############
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

#the selection rate over the 100 replications
my_table2["V1"]
my_table2["V2"]
my_table2["V3"]