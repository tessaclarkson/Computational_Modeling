
library(readr)
library(dplyr)
library(knitr)
library(stats)
library(plyr)
library(tidyverse)
library(ggplot2)
library(foreach)
library(readxl)
library(data.table)

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
##Reads in and formats data
datadir = "/Users/tuk38887/Desktop/BehavData/CombinedData/"
#reads in combined data
data <- read_csv(paste(datadir,"LEARN_BehavData2.csv", sep = ""))
data <-  na.omit(data)

##remove subjects excluded for behavioral response issues
for(i in 1:length(data$s)){
  if (data$s[i] == "sub-1144"){
    data$exclude[i] = 1
  } else if (data$s[i] == "sub-1292"){
    data$exclude[i] = 1
  } else if (data$s[i] == "sub-1304"){
    data$exclude[i] = 1
  } else if (data$s[i] == "sub-1405"){
    data$exclude[i] = 1
  } else if (data$s[i] == "sub-1482"){
    data$exclude[i] = 1
  } else{
    data$exclude[i] = 0
  }
}
data <- data %>%
  filter(exclude== 0)

pars = read_excel("Desktop/BehavData/Analyses/ModelFitting/M9_Lfdk_SigSubParms_1.0.xlsx")
#makes nested data table
data= data %>% group_by(s) %>% nest()
#makes empty dataframe to fill in with all parameters
par_data = as.data.frame(matrix(ncol = 0, nrow=0))

all_sim_data = as.data.frame(matrix(ncol = 0, nrow=0))
#
#loops through subjects
for(d in 1:length(data$s)){
  #repeats optimization 20 times per subject to reduce chance of local minimum for param estimimates
  for (i in 1:100){
    print(i)
    print(d)
     #d=1
    
    #pulls subject ID
    par_subdata= as.data.frame(data$s[d])
    sub= data$s[d]
    
    ##############################
    ##initializes parameters and data
    ##############################
    #actual predictions
    pred_data= na.omit(data$data[[d]][3])
    pred_data = t(pred_data)
    #actual feedback
    fdk_data = data$data[[d]][4]
    fdk_data= t(fdk_data)
    #reputation data
    rep_data= data$data[[d]][2]
    rep_data = t(rep_data)
    
    
    #makes a trial list
    x= as.data.frame(data$data[[d]][7])
    
    # Simulation paramters
    n_tr  <- length(x[,1])      # Number of trials 
    
    
    #makes parameter list
    params = list()
    
    params$tau = pars$tau[which( pars$sub==sub)]
    params$w_m8 = pars$w_m8[which( pars$sub==sub)]
    params$w_n8 = pars$w_n8[which( pars$sub==sub)]
    params$w_m6 = pars$w_m6[which( pars$sub==sub)]
    params$w_n6 = pars$w_n6[which( pars$sub==sub)]
    params$k = pars$k[which( pars$sub==sub)]
    params$l_p = pars$l_p[which( pars$sub==sub)]
    params$l_n = pars$l_n[which( pars$sub==sub)]
    
    
    #resp_prob empty matrices
    pred_prob = matrix(ncol = length(x[,1]), nrow=1)
    K_m8= matrix(ncol = length(x[,1]), nrow=1)
    K_n8= matrix(ncol = length(x[,1]), nrow=1)
    K_m6= matrix(ncol = length(x[,1]), nrow=1)
    K_n6= matrix(ncol = length(x[,1]), nrow=1)
    
    
    #makes empty dataframe for learning rate values for each trial
    alpha_m8= matrix(ncol = length(x[,1]), nrow=1)
    alpha_n8= matrix(ncol = length(x[,1]), nrow=1)
    alpha_m6= matrix(ncol = length(x[,1]), nrow=1)
    alpha_n6= matrix(ncol = length(x[,1]), nrow=1)
    
    #makes empty dataframe for learning rate values for each trial
    A= matrix(ncol = length(x[,1]), nrow=1)
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
    #initializes Expected Prediction value
    ExPred_m6 = 0.5
    ExPred_m8 = 0.5
    ExPred_n6 = 0.5
    ExPred_n8 = 0.5
    A_m6 =0.5
    A_m8 =0.5
    A_n6 =0.5
    A_n8 =0.5
    LR=0.5
    EV=0.5
    AV=0.5
    EV_acc=0.5

    
    sim_dat <- foreach(t=1:n_tr, .combine = "rbind") %do% {
      #t=2
    #for(t in 1:n_tr){ 
      print(t)
      # compute the change in strength
      if ( rep_data[t] == "Nice80") {
        #step one of calculating learning rate as constant or dynamic
        K_n8[1,t]= params['w_n8'][[1]]*A_n8+(1-params['w_n8'][[1]])
        alpha_n8[1,t] = params['k'][[1]]*K_n8[1,t]
        #####softmax function for choice 1 & 2 probabilities######
        #calculates choice probability for selecting mean (1) for trial t
        pred_prob[t] = softmax_pred(ExPred_n8, params['tau'][[1]])
        
        
        prob =  c(1-pred_prob[t], pred_prob[t])
        choice <- sample(c(0,1), size = 1, prob =  prob)

   
        ####update function for expected value#####
        ####update function for expected value#####
        #compute the prediction error aka "delta"
        delta <-  fdk_data[t] - ExPred_n8
        update <- alpha_n8[1,t]  * delta
        ExPred_n8 <- ExPred_n8 + update
        
        #update associability
        if (t==length(fdk_data)) {
          #print("done")
          
        } else {
          #update associability
          if ( fdk_data[t] == 1 ) {
            A_n8 = params['l_p'][[1]]*A_n8
            A_n8 = A_n8 + (1-params['l_p'][[1]])*(delta)^2} 
          else {
            A_n8 = params['l_n'][[1]]*A_n8
            A_n8 = A_n8 + (1-params['l_n'][[1]])*(delta)^2} 
        }
        LR= alpha_n8[1,t] 
        EV= ExPred_n8
        AV= A_n8
        EV_acc = prob[choice+1]
        

      } else if ( rep_data[t] == "Mean80") {
        #step one of calculating learning rate as constant or dynamic
        K_m8[1,t]= params['w_m8'][[1]]*A_m8+(1-params['w_m8'][[1]])
        alpha_m8[1,t] = params['k'][[1]]*K_m8[1,t]
        #####softmax function for choice 1 & 2 probabilities######
        #calculates choice probability for selecting mean (1) for trial t
        pred_prob[t] = softmax_pred(ExPred_m8, params['tau'][[1]])
        
        
        prob =  c(1-pred_prob[t], pred_prob[t])
        choice <- sample(c(0,1), size = 1, prob =  prob)

        ####update function for expected value#####
        ####update function for expected value#####
        #compute the prediction error aka "delta"
        delta <-  fdk_data[t] - ExPred_m8
        update <- alpha_m8[1,t]  * delta
        ExPred_m8 <- ExPred_m8 + update

        #update associability
        if (t==length(fdk_data)) {
          #print("done")
          
        } else {
          if ( fdk_data[t] == 1 ) {
            A_m8 = params['l_p'][[1]]*A_m8
            A_m8 = A_m8 + (1-params['l_p'][[1]])*(delta)^2} 
          else {
            A_m8 = params['l_n'][[1]]*A_m8
            A_m8 = A_m8 + (1-params['l_n'][[1]])*(delta)^2} 
        }
        LR= alpha_m8[1,t] 
        EV= ExPred_m8
        AV= A_m8
        EV_acc = prob[choice+1]
        
      } else if ( rep_data[t] == "Mean60") {
        #step one of calculating learning rate as constant or dynamic
        K_m6[1,t]= params['w_m6'][[1]]*A_m6+(1-params['w_m6'][[1]])
        alpha_m6[1,t] = params['k'][[1]]*K_m6[1,t]
        #####softmax function for choice 1 & 2 probabilities######
        #calculates choice probability for selecting mean (1) for trial t
        pred_prob[t] = softmax_pred(ExPred_m6, params['tau'][[1]])
        
        
        prob =  c(1-pred_prob[t], pred_prob[t])
        choice <- sample(c(0,1), size = 1, prob =  prob)

        ####update function for expected value#####
        ####update function for expected value#####
        #compute the prediction error aka "delta"
        delta <-  fdk_data[t] - ExPred_m6
        update <- alpha_m6[1,t]  * delta
        ExPred_m6<- ExPred_m6 + update
        EV_acc = prob[choice+1]
        
        #update associability
        if (t==length(fdk_data)) {
          #print("done")
          
        } else {
          #update associability
          if ( fdk_data[t] == 1 ) {
            A_m6 = params['l_p'][[1]]*A_m6
            A_m6 = A_m6 + (1-params['l_p'][[1]])*(delta)^2} 
          else {
            A_m6 = params['l_n'][[1]]*A_m6
            A_m6 = A_m6 + (1-params['l_n'][[1]])*(delta)^2} 
        }
        LR= alpha_m6[1,t] 
        EV= ExPred_m6
        AV= A_m6
        EV_acc = prob[choice+1]
        
      } else  {
        #step one of calculating learning rate as constant or dynamic
        K_n6[1,t]= params['w_n6'][[1]]*A_n6 +(1-params['w_n6'][[1]])
        alpha_n6[1,t] = params['k'][[1]]*K_n6[1,t]
        #####softmax function for choice 1 & 2 probabilities######
        #calculates choice probability for selecting mean (1) for trial t
        pred_prob[t] = softmax_pred(ExPred_n6, params['tau'][[1]])
        
        
        prob =  c(1-pred_prob[t], pred_prob[t])
        choice <- sample(c(0,1), size = 1, prob =  prob)

        ####update function for expected value#####
        ####update function for expected value#####
        #compute the prediction error aka "delta"
        delta <-  fdk_data[t] - ExPred_n6
        update <- alpha_n6[1,t]  * delta
        ExPred_n6 <- ExPred_n6 + update
        
        #update associability
        if (t==length(fdk_data)) {
          #print("done")
          
        } else {
          #update associability
          if ( fdk_data[t] == 1 ) {
            A_n6 = params['l_p'][[1]]*A_n6
            A_n6 = A_n6 + (1-params['l_p'][[1]])*(delta)^2} 
          else {
            A_n6 = params['l_n'][[1]]*A_n6
            A_n6 = A_n6 + (1-params['l_n'][[1]])*(delta)^2} 
        }
        LR= alpha_n6[1,t] 
        EV= ExPred_n6
        AV= A_n6
        EV_acc = prob[choice+1]
        
      }

      #chosen_prob[t]=pred_data[t]*pred_prob[t]+(1-pred_data[t])*(1-pred_prob[t])
      chosen_prob[t] = prob[choice+1]

      # Likelihood of current LR according to prior distribution (same prior used for all beta learning rates)
      prior_tau <-dgamma(params['tau'][[1]], 1.2, scale = 1.5, log = FALSE)
      prior_w_n8 = dbeta(params['w_n8'][[1]], 1.1, 1.1, ncp = 0, log = FALSE)
      prior_w_n6 = dbeta(params['w_n6'][[1]], 1.1, 1.1, ncp = 0, log = FALSE)
      prior_w_m8 = dbeta(params['w_m8'][[1]], 1.1, 1.1, ncp = 0, log = FALSE)
      prior_w_m6 = dbeta(params['w_m6'][[1]], 1.1, 1.1, ncp = 0, log = FALSE)
      prior_k = dbeta(params['k'][[1]], 1.1, 1.1, ncp = 0, log = FALSE)
      prior_l_p = dbeta(params['l_p'][[1]], 1.1, 1.1, ncp = 0, log = FALSE)
      prior_l_n = dbeta(params['l_n'][[1]], 1.1, 1.1, ncp = 0, log = FALSE)
      
      
      # Save data
      data.frame(sub=data$s[d],
                 trial   = t,
                 Reputation     = rep_data[t],
                 Pr_nice      = round(pred_prob[t],digits= 3),
                 Pr_choice = round(chosen_prob[t], digits=3),
                 Choice  = as.numeric(choice),
                 Outcome = as.numeric(fdk_data[t]),
                 PE_Val = round((fdk_data[t] - EV), digits=3),
                 PE_Acc = round((fdk_data[t] - EV_acc), digits=3),
                 LR= round(LR, digits=3),
                 EV= round(EV, digits=3),
                 AV= round(AV, digits=3),
                 EV_acc = round(EV_acc, digits=3))
     
     
    }
    all_sim_data = rbind.fill(all_sim_data, sim_dat)
    
  }
  }

write.csv(all_sim_data,file=(paste("/Users/tuk38887/Desktop/BehavData/Analyses/Model_Validation/M9/M9_Lfdk_sim_data_1.0.csv",sep="")),row.names=FALSE)
