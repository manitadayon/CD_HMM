### Find distribution of data in HMM per state
### This means what is the distribution of data per state 1 or 2 or 3 or 4.

Index_per_state<-function(Train_traj,N_state,T)
{

  Index=list()
  for (ii in 1:N_state)
  
    Index[[ii]]=Train_traj==ii
  
  return(Index)
  
}

HMM_Train<-function(Data_Train,N_state,Len_Train)
{
  mod <- depmix(Data_Train~1,  nstates = N_state,ntimes=Len_Train,  family = gaussian()) # use gaussian() for normally distributed data
  fit.mod <- fit(mod)
  Train_label<-viterbi(fit.mod)[,1]
  Train_param<-list(Train_label,fit.mod)
  return (Train_param)
}

observation_per_state<-function(Data,Index)
{
  ## Flatten the observation
  Data_flatten<-pracma::Reshape(t(Data),1,nrow(Data)*ncol(Data))
  obs=list()
  obs=lapply(Index, function(x){Data_flatten[x]})
  return (obs)
}

hist_fitting<-function(obs)  # This is based on automatic density estimation
{
  par(mfrow=c(length(obs),1))
  for (ii in 1:length(obs)) {
    
    hist(obs[[ii]],probability = TRUE,col = "grey",main = paste("Observation Density",ii))
    lines(density(obs[[ii]]),col="blue") 
      }
}

hist_fitting2<-function(obs)  # This is based on manual density estimation where we fit the gaussian distribution to histogram
{
  dev.new()
  par(mfrow=c(length(obs),1))
  for (ii in 1:length(obs)) {
    
    hist(obs[[ii]],probability = TRUE,col = "grey",main = paste("Observation Density manuall fitting",ii))
    lines(sort(obs[[ii]]),dnorm(sort(obs[[ii]]),mean = mean(obs[[ii]]),sd=sd(obs[[ii]])),type = "l",col="blue")
    
  }
}




Data<- read.csv(Path_Data)
Label<-read.csv(Path_Label)

Train_Ratio=0.1
N=1000
T=20
N_state<-4

flag<-1

Info<-Data_prepare(Data,Label,Train_Ratio,N,T)  # Make sure to access first element you use Info[[1]]

HMM_Label=HMM_Train(Info[[1]],N_state,Info[[3]])

Index=Index_per_state(HMM_Label[[1]],N_state,T)

obs<-observation_per_state(Info[[1]],Index)

hist_fitting(obs)

hist_fitting2(obs)




