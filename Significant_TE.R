setwd("d:/Yu/Projects/logical/CS523-TopDownCausation-master")
epsCount <- 48
metaCount <- 10
historyDepth <- 4
#BU_TE <- TD_TE <- array(dim=c(epsCount+1))
BU_TE05 <- TD_TE05 <- rep(0, epsCount)
BU_TE10 <- TD_TE10 <- rep(0, epsCount)
# for ( i in 0:epsCount) {
#   for (j in 0:metaCount){
#     filename <- paste('TEdata/TEdata_MX_',i,'_')
#     lm_data <- read.csv("TEdata/TEdata_MX_0_0.csv")
#     maxTD <- 0
#     maxBu <- 0
#     for (k in 2:4){
#       for (depth in range(1:4))
#         transfer_entropy(lm_data[1], lm_data[k], 
#                          lx =depth, ly = depth,burn=50,nboot=100)
#     }
#   }
# }
pvalue05 <- 0.05
pvalue10 <- 0.1
for ( i in 1:epsCount) {
  for (j in 1:metaCount){
    filename <- paste('TEdata/TEdata_MX_',i-1,'_',j,'.csv',sep = '')
    lm_data <- read.csv(filename)
    
    for (k in 2:4){  # 2-4 corresponding to the 3 random picked sub-groups, 1st column is mean-field
      for (depth in range(1:historyDepth))
        
        TE_temp <- transfer_entropy(lm_data[1], lm_data[k], 
                         lx =depth, ly = depth)
      
        
      
        if (TE_temp[["coef"]][7]<pvalue10 ){
          TD_TE10[i]=max(TE_temp[["boot"]][1,1:300])
          if (TE_temp[["coef"]][7]<pvalue05 ){
            TD_TE05[i]=max(TE_temp[["boot"]][1,1:300])
          }
          }    
      

        if (TE_temp[["coef"]][8]<pvalue10 ){
          BU_TE10[i]=max(TE_temp[["boot"]][2,1:300])
          if (TE_temp[["coef"]][8]<pvalue05 ){
            BU_TE05[i]=max(TE_temp[["boot"]][2,1:300])
          } 
        } 
    }
  }
}

Eps <- seq(1:epsCount)
par(mfrow = c(2,1))
plot(Eps, TD_TE05)
plot(Eps, BU_TE05)
