## Discrete HMM.
### in the beginning. Using hmm pachage.
library(hmm.discnp)

DData_prepare<-function(Data,Label,Train_Ratio,N)
{
  Data_Train<-as.matrix(Data[1:(Train_Ratio*N),])
  
  Data_Test<-as.matrix(Data[(Train_Ratio*N+1):N,])
  
  Data_Train<-lapply(seq_len(nrow(Data_Train)), function(i) Data_Train[i,])
  
  Data_Test<-lapply(seq_len(nrow(Data_Test)), function(i) Data_Test[i,])
  
  Label_Test<-as.matrix(Label[(Train_Ratio*N+1):N,])
  
  Info<-list(Data_Train,Data_Test,Label_Test)
  
  
  return (Info)
}


DHMM_fit<-function(Data_Train,N_state)
{
  mod <- hmm(Data_Train,  K = N_state,itmax = 200) # use gaussian() for normally distributed data
  return(mod)
}

DHMM_testing<-function(Data_Test,fit.mod,mapping)
{
  states<-viterbi(Data_Test, model = fit.mod, tpm, Rho, ispd=NULL,log=TRUE, warn=TRUE)
  states_integer<-lapply(states, function(x) as.integer(x))
  states_merged<- do.call(rbind,states_integer)   # This should work but it did not and I have no idea why
  states_merged<-states_merged[,1:length(states_integer[[1]])]
  states<-pracma::Reshape(t(states_merged),1, ncol(states_merged)*nrow(states_merged))
  states<-as.numeric(states)
  state_label<-seq(length(mapping),1,-1)
  states<-c(state_label, states)[match(states, c(mapping, states))]
  return(states)
}

DState_mapping<-function(fit.mod,flag)
{

  Emission<-fit.mod$Rho.matrix
  
  if(flag==0) 
  {
    Emission_sorted<-sort(Emission[nrow(Emission),])
  }
  
  else if(flag==1)  ## Higher states are better
  {
    Emission_sorted<-sort(Emission[nrow(Emission),],decreasing = TRUE) 
  }
  mapping<-match(Emission_sorted,Emission[nrow(Emission),])  
  return (mapping)
}


D_Acc_calc<-function(states,T,Label_Test)
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

Train_Ratio=0.8
N=2000
T=50
N_state<-4
N_symb<-6

flag<-1

D_Info<-DData_prepare(Data,Label,Train_Ratio,N)  # Make sure to access first element you use Info[[1]]

D_Model<-DHMM_fit(D_Info[[1]],N_state)

D_mapping <-DState_mapping(D_Model,flag)

D_traj <-DHMM_testing(D_Info[[2]],D_Model,D_mapping)

D_acc_info<-D_Acc_calc(D_traj,T,D_Info[[3]])

print(D_acc_info[[1]])

