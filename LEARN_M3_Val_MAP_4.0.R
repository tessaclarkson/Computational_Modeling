
library(readr)
library(dplyr)
library(knitr)
library(stats)
library(plyr)
library(tidyverse)
library(ggplot2)
library(foreach)

##################################################################################
#1 Learning rate RL model
###BASE (NO VALENCE OR PREDICTABILITY OR ASSOCIATIVE VALUE)
##Instrumental RW model (no individuation)
###response RW model with different learning rates for prediction error 
#prediction error is based on prediction and feedback valence
##################################################################################

##################################################################################
##Reads in and formats data
##################################################################################
datadir = "/Users/tuk38887/Desktop/BehavData/CombinedData/"
#reads in combined data
data <- read_csv(paste(datadir,"LEARN_BehavData2.csv", sep = ""))
data <-  na.omit(data)


#makes nested data table
data= data %>% group_by(s) %>% nest()
#makes empty dataframe to fill in with all parameters
par_data = as.data.frame(matrix(ncol = 0, nrow=0))

#loops through subjects
for(d in 1:length(data$s)){
  #repeats optimization 20 times per subject to reduce chance of local minimum for param estimimates
  for (i in 1:20){
    #print(i)
    print(d)
    #d=4
    #pulls subject ID
    par_subdata= as.data.frame(data$s[d])
    
    ##############################
    ##initializes parameters and data
    ##############################
    #actual predictions
    pred_data= data$data[[d]][3]
    pred_data = t(pred_data)
    #actual feedback
    fdk_data = data$data[[d]][4]
    fdk_data= t(fdk_data)
    #reputation data
    rep_data= data$data[[d]][2]
    rep_data = t(rep_data)
    #makes a trial list
    x= as.data.frame(data$data[[d]][7])
    
    #makes parameter list
    params = list()
    params$LR_m = runif(1, min=0.01, max=.99)
    params$LR_n = runif(1, min=0.01, max=.99)
    params$tau = runif(1, min=0.01, max=.99)
    
    
    #makes empty matrices to fill in with choice probabilitiies
    pred_prob = matrix(ncol = length(x[,1]), nrow=1)
    chosen_prob = numeric(length(fdk_data))
    
    
    ##############################
    ##softmax function Prediction
    ##############################
    softmax_pred =  function(ExPred, params){
      #numerator exp of value modeled
      num = 1
      #denominator exp of all choices
      dem = 1+exp(-1*((ExPred-(1-ExPred))*params))
      #probability
      pred_prob = num/dem
      return (pred_prob)
    }
    
    ##############################
    ##loop for choice prob for each trial
    ##############################
    
    #computes log likelihood
    LEARN <- function(params) {
      #initializes Expected Prediction value
      ExPred_m6 = 0.5
      ExPred_m8 = 0.5
      ExPred_n6 = 0.5
      ExPred_n8 = 0.5
      
      LL<-  for(t in 1:length(fdk_data)) {
        # t=1
        print(t)
        
        # compute the change in strength
        if ( rep_data[t] == "Nice80") {
          #####softmax function for choice 1 & 2 probabilities######
          #calculates choice probability for selecting mean (1) for trial t
          pred_prob[t] = softmax_pred(ExPred_n8, params['tau'][[1]])
          ####update function for expected value#####
          # compute the prediction error aka "delta"
          delta <-  fdk_data[t] - ExPred_n8
          update <- params['LR_n'][[1]] * delta
          ExPred_n8 <- ExPred_n8 + update
          
        } 
        else if ( rep_data[t] == "Mean80") {
          #####softmax function for choice 1 & 2 probabilities######
          #calculates choice probability for selecting mean (1) for trial t
          pred_prob[t] = softmax_pred(ExPred_m8,params['tau'][[1]])

          ####update function for expected value#####
          # compute the prediction error aka "delta"
          delta <-  fdk_data[t] - ExPred_m8
          update <- params['LR_m'][[1]] * delta
          ExPred_m8 <- ExPred_m8 + update
        } 
        else if ( rep_data[t] == "Nice60") {
          #####softmax function for choice 1 & 2 probabilities######
          #calculates choice probability for selecting mean (1) for trial t
          pred_prob[t] = softmax_pred(ExPred_n6, params['tau'][[1]])

          ####update function for expected value#####
          # compute the prediction error aka "delta"
          delta <-  fdk_data[t] - ExPred_n6
          update <- params['LR_n'][[1]] * delta
          ExPred_n6 <- ExPred_n6 + update
          
        } 
        else {
          #####softmax function for choice 1 & 2 probabilities######
          #calculates choice probability for selecting mean (1) for trial t
          pred_prob[t] = softmax_pred(ExPred_m6, params['tau'][[1]])

          ####update function for expected value#####
          # compute the prediction error aka "delta"
          delta <-  fdk_data[t] - ExPred_m6
          update <- params['LR_m'][[1]] * delta
          ExPred_m6 <- ExPred_m6 + update
        } 
        
        #pulls only SELECTED choice probabilities
        chosen_prob[t]=pred_data[t]*pred_prob[t]+(1-pred_data[t])*(1-pred_prob[t])
        
        # Likelihood of current LR according to prior distribution (same prior used for all beta learning rates)
        prior_LR_m <-dbeta(params['LR_m'][[1]], 1.1, 1.1, ncp = 0, log = FALSE)
        prior_LR_n <-dbeta(params['LR_n'][[1]], 1.1, 1.1, ncp = 0, log = FALSE)
        prior_tau <-dgamma(params['tau'][[1]], 1.2, scale = 1.5, log = FALSE)
        
      }
      
      
      Llik =-sum(log(chosen_prob), log(prior_LR_m), log(prior_LR_n),log(prior_tau), .Machine$double.eps)
      return(Llik)
      plot(Llik)
      
    }
    
    # Use optim to minimize the (minus) log-likelihood function
    RW_1 <- optim(par      = unlist(params),             # Initial guess for beta
                  fn       = LEARN,            # Function we are minimizing
                  method   = "L-BFGS-B",       # Specific algorithm used
                  lower    = 0.01,            # Lower bound for beta 
                  upper    = .999,             # Upper bound for beta
                  hessian =TRUE)               # gives hessian matrix for laplace approx.
    
    #computes laplacian approximation of model evidence
    #RW_M3_Val_Pred$value is the stored likelihood value
    #length(RW_M3_Val_Pred$par is the number of parameters in the model
    #RW_M3_Val_Pred$hessian is the hessian matrix produced from the optim function
    laplace = -1*RW_1$value + ((length(RW_1$par))/2)*log(2*pi) - (.5*log(abs(det(RW_1$hessian))))
    
    #renames estimated paramters after model 
    par_subdata1 = as.data.frame(t(RW_1$par))
    names(par_subdata1)= names(RW_1$par)
    
    #combines estimated model parameters into single output file
    par_subdata = cbind(par_subdata, par_subdata1, laplace)
    par_data = rbind.fill(par_data, par_subdata)
    
  }
}
#export each subject's learning rate parameters for single sub validation
write.csv(par_data, "/Users/tuk38887/Desktop/BehavData/Analyses/ModelFitting/M3.2LR_Val_MAP_5.0.csv")

