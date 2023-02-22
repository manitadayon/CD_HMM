### Continous time HMM with R using Depmixs4. 
library(depmixS4)
library(pracma)
Data_prepare<-function(Data,Label,Train_Ratio,N,T)
{
  Data_Train<-as.matrix(Data[1:(Train_Ratio*N),])
  
  Len_Train<-as.numeric(pracma::repmat(T,1,nrow(Data_Train)))
  
  Data_Test<-as.matrix(Data[(Train_Ratio*N+1):N,])
  
  Len_Test<-as.numeric(pracma::repmat(T,1,nrow(Data_Test)))
  
  Data_Train<-pracma::Reshape(t(Data_Train),nrow(Data_Train)*ncol(Data_Train),1)
  
  Data_Test<-pracma::Reshape(t(Data_Test),nrow(Data_Test)*ncol(Data_Test),1)
  
  Label_Test<-as.matrix(Label[(Train_Ratio*N+1):N,])
  
  Info<-list(Data_Train,Data_Test,Len_Train,Len_Test,Label_Test)
  
  
  return (Info)
}

HMM_fit<-function(Data_Train,N_state,Len_Train)
{
  mod <- depmix(Data_Train~1,  nstates = N_state,ntimes=Len_Train,  family = gaussian()) # use gaussian() for normally distributed data
  fit.mod <- fit(mod)
  return(fit.mod)
}


HMM_testing<-function(Data_Test,N_state,Len_Test,fit.mod,mapping)
{
  mod <- depmix(Data_Test~1,  nstates = N_state,ntimes=Len_Test,  family = gaussian()) # use gaussian() for normally distributed data
  modNew <- setpars(mod,getpars(fit.mod))
  states<-viterbi(modNew)[,1]
  state_label<-seq(length(mapping),1,-1)
  states<-c(state_label, states)[match(states, c(mapping, states))]
  return(states)
}

State_mapping<-function(fit.mod,N_state,flag)
{
  ## find the mean or intercept
  parameters<-getpars(fit.mod)
  Index<-seq(1,length(parameters)-(N_state^2+N_state+1),2)
  Gauss_par<-parameters[(N_state^2+N_state+1):length(parameters)]
  Mean<-Gauss_par[Index]
  
  if(flag==0) 
  {
    Mean_sorted<-sort(Mean)
  }
  
  else if(flag==1)  ## Higher states are better
  {
    Mean_sorted<-sort(Mean,decreasing = TRUE) 
  }
  mapping<-match(Mean_sorted,Mean)  
  return (mapping)
}


Acc_calc<-function(states,T,Label_Test)
{
  states_reshaped<-t(pracma::Reshape(states,T,length(states)/T))
  count_Error=rep(NA, nrow(states_reshaped))
  for (ii in 1:nrow(states_reshaped)) 
  {
    Error<-0
    for (jj in 1:T)
    {
      if(states_reshaped[ii,jj] != Label_Test[ii,jj] )
        Error<-Error+1
    }
    count_Error[ii]=Error
  }
  accuracy=1-sum(count_Error)/(nrow(states_reshaped)*T)
  acc<-list(accuracy,count_Error)
  
  return(acc)
}


Data<- read.csv(Path_Data)
Label<-read.csv(Path_Label)

Train_Ratio=0.001
N=2000
T=20
N_state<-4

flag<-1

Info<-Data_prepare(Data,Label,Train_Ratio,N,T)  # Make sure to access first element you use Info[[1]]

Model <-HMM_fit(Info[[1]],N_state,Info[[3]])
  
mapping <-State_mapping(Model,N_state,flag)

traj<-HMM_testing(Info[[2]],N_state,Info[[4]],Model,mapping)

acc_info<-Acc_calc(traj,T,Info[[5]])

print(acc_info[[1]])
