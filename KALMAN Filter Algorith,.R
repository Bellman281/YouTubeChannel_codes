#Kalman filter https://www.youtube.com/watch?v=PZrFFg5_Sd0
EST=vector()
E_EST=vector()
MEA=vector()
#true_temp=72
ini_est=68
init_E_est=2
#MEA : measurement
init_MEA=75
E_MEA=4

i=1
EST[i]=ini_est
E_EST[i]=init_E_est
MEA[i]=init_MEA
#Round 1
#STEP 1
#Kalman GAIN KG=\frac{E_EST}{E_EST+E_MEA}
KG=E_EST[i]/(E_EST[i]+E_MEA);KG
  
#STEP2

#Estimate 
EST[i+1]=EST[i] +KG*(MEA[i]- EST[i]);EST

E_EST[i+1]=(1-KG)*(E_EST[i]);E_EST

#round 2
i=i+1
#New measurement . Normally error in measurement doesn't change
MEA[i]=71
E_MEA=4
#STEP 1
#Kalman GAIN KG=\frac{E_EST}{E_EST+E_MEA}
KG=E_EST[i]/(E_EST[i]+E_MEA);KG

#STEP2

#Estimate 
EST[i+1]=EST[i] +KG*(MEA[i]- EST[i]);EST

E_EST[i+1]=(1-KG)*(E_EST[i]);E_EST


#round3
i=i+1
#New measurement . Normally error in measurement doesn't change
MEA[i]=70
E_MEA=4
#STEP 1
#Kalman GAIN KG=\frac{E_EST}{E_EST+E_MEA}
KG=E_EST[i]/(E_EST[i]+E_MEA);KG

#STEP2

#Estimate 
EST[i+1]=EST[i] +KG*(MEA[i]- EST[i]);EST

E_EST[i+1]=(1-KG)*(E_EST[i]);E_EST



#round4
i=i+1
#New measurement . Normally error in measurement doesn't change
MEA[i]=74
E_MEA=4
#STEP 1
#Kalman GAIN KG=\frac{E_EST}{E_EST+E_MEA}
KG=E_EST[i]/(E_EST[i]+E_MEA);KG

#STEP2

#Estimate 
EST[i+1]=EST[i] +KG*(MEA[i]- EST[i]);EST

E_EST[i+1]=(1-KG)*(E_EST[i]);E_EST

plot(EST, type = "b",ylim = c(65,75))
abline(h=72,col="blue")
lines(MEA,type = "b",col="red")
