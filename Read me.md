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
      	## Loading required package : Kendall
		## Loading required package : SimDesign
		## Loading required package : glmnet
		## Loading required package : dplyr
            ## Loading required package : FactoMineR
# Authors 
Hansna Chamlal, Asmaa Bezmane and Tayeb Ouaderhman

