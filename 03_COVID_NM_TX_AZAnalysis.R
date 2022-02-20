#Read Me ####
#This is an effort to understand how COVID 
#NM intervention dates
  #March 2020 -> Feb 2021 Out-of-state quarantine
  #March 2021 - schools open
  #November 2021 - vaccinations available

#covid data from 
#https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36


#libraries ####
library(tidyverse)
library(RTransferEntropy)
library(entropy)
library(VennDiagram)


#data ####
datNYT <-  read.csv("Data/Raw/NYTiems us-counties-2022.csv")
datNM_TX <- datNYT %>% 
  mutate(id = row_number()) %>% 
  mutate(date = as.Date(date)) %>% 
  filter(state == "New Mexico"| state == "Texas") %>% 
  mutate(log_cases_100K = log(cases_avg_per_100k+1)) %>% 
  select(date, state, log_cases_100K) %>% 
  pivot_wider(id_cols = date, names_from = state, values_from = log_cases_100K) %>% 
  drop_na() %>% 
  rename(Date1 = date)

#MI and TE series 1 - out-of-state quarantine in place ####
datNM_TX1 <- datNM_TX %>% 
  filter(Date1 >="2020-03-11", Date1 <= "2021-02-01") %>% 
  rename(NewMexico = "New Mexico")

 #Descritize data to bins NM_TX 1
quants1 <- datNM_TX1 %>% 
  select(!Date1) %>% 
  sapply(quantile, probs = c(0.25, 0.75))

NM1_bwidth <- (2*(quants1[2,2] - quants1[1,2]))/10^(1/3)
NM1_bins <- ceiling((max(datNM_TX1$NewMexico)-min(datNM_TX1$NewMexico))/NM1_bwidth)

TX1_bwidth <- (2*(quants1[2,1] - quants1[1,1]))/10^(1/3)
TX1_bins <- ceiling((max(datNM_TX1$Texas)-min(datNM_TX1$Texas))/TX1_bwidth)

  #mutual information calc
NM_TX1_2d <- discretize2d(datNM_TX1$NewMexico, datNM_TX1$Texas, 
                         numBins1 = NM1_bins, numBins2 = TX1_bins)
    #joint entropy
Joint_1 <- entropy(NM_TX1_2d)
    #marginal entropy
Marg_1r <- entropy(rowSums(NM_TX1_2d))
Marg_1c <- entropy(colSums(NM_TX1_2d))

    #mutual information
MI_NM_TX1 <- round((Marg_1r + Marg_1c - Joint_1), digits = 3)
MI_NM_TX1

#transfer entropy 
TE_NM_TX1 <- transfer_entropy(datNM_TX1$NewMexico, datNM_TX1$Texas, 
                              lx = 20, ly = 20)
TE_NM_TX1

#MI and TE series 2 - schools opened ####
datNM_TX2 <- datNM_TX %>% 
  filter(Date1 >="2021-03-11", Date1 < "2021-11-01")%>% 
  rename(NewMexico = "New Mexico")

#Descritize data to bins NM_TX 1
quants2 <- datNM_TX2 %>% 
  select(!Date1) %>% 
  sapply(quantile, probs = c(0.25, 0.75))

NM2_bwidth <- (2*(quants2[2,2] - quants2[1,2]))/10^(1/3)
NM2_bins <- ceiling((max(datNM_TX2$NewMexico)-min(datNM_TX2$NewMexico))/NM2_bwidth)

TX2_bwidth <- (2*(quants2[2,1] - quants2[1,1]))/10^(1/3)
TX2_bins <- ceiling((max(datNM_TX2$Texas)-min(datNM_TX2$Texas))/TX2_bwidth)

#mutual information calc
NM_TX2_2d <- discretize2d(datNM_TX2$NewMexico, datNM_TX2$Texas, 
                          numBins1 = NM2_bins, numBins2 = TX2_bins)
  #joint entropy
Joint_2 <- entropy(NM_TX2_2d)
  
  #marginal entropy
Marg_2r <- entropy(rowSums(NM_TX2_2d))
Marg_2c <- entropy(colSums(NM_TX2_2d))

  #mutual information
MI_NM_TX2 <- round((Marg_2r + Marg_2c - Joint_2), digits = 3)
MI_NM_TX2

  #transfer entropy 
TE_NM_TX2 <- transfer_entropy(datNM_TX2$NewMexico, datNM_TX2$Texas)
TE_NM_TX2

#MI and TE series 2 - vaccinations available to all in New Mexico ####

datNM_TX3 <- datNM_TX %>% 
  filter(Date1 >="2021-11-01")%>% 
  rename(NewMexico = "New Mexico")

  #Descritize data to bins NM_TX 1
quants3 <- datNM_TX3 %>% 
  select(!Date1) %>% 
  sapply(quantile, probs = c(0.25, 0.75))

NM3_bwidth <- (2*(quants3[2,2] - quants3[1,2]))/10^(1/3)
NM3_bins <- ceiling((max(datNM_TX3$NewMexico)-min(datNM_TX3$NewMexico))/NM3_bwidth)

TX3_bwidth <- (2*(quants3[2,1] - quants3[1,1]))/10^(1/3)
TX3_bins <- ceiling((max(datNM_TX3$Texas)-min(datNM_TX3$Texas))/TX3_bwidth)

  #mutual information calc
NM_TX3_2d <- discretize2d(datNM_TX3$NewMexico, datNM_TX3$Texas, 
                           numBins1 = NM3_bins, numBins2 = TX3_bins)
  #joint entropy
Joint_3 <- entropy(NM_TX3_2d)

  #marginal entropy
Marg_3r <- entropy(rowSums(NM_TX3_2d))
Marg_3c <- entropy(colSums(NM_TX3_2d))

  #mutual information
MI_NM_TX3 <- round((Marg_3r + Marg_3c - Joint_3), digits = 3)
MI_NM_TX3

  #transfer entropy 
TE_NM_TX3 <- transfer_entropy(datNM_TX3$NewMexico, datNM_TX3$Texas)
TE_NM_TX3

#plots ####
  #time series

date_range <- which(datNM_TX$Date1 %in% as.Date(
  c("2021-02-01", "2021-03-01")))

datNM_TX %>% 
  pivot_longer(cols = Texas:"New Mexico", names_to = "State", values_to = "LogCases") %>% 
  ggplot(aes(x = Date1, y = LogCases, col = State))+
  geom_line(size = 1)+
  geom_vline(xintercept = as.numeric(datNM_TX$Date1[date_range]),color = "black", 
             size = 1)+
  scale_color_manual(values = c("New Mexico" = "red", Texas ="black"))+
  annotate(geom = "text", x = as.Date("2020-04-01"), y = 0.5, 
    label = "Out-of-state quarantine", hjust = 0, vjust = 1, size = 4)+
  annotate(geom = "text", x = as.Date("2020-05-01"), y = 5.5, size = 4,
           label = "TE 0 ")+
  annotate(geom = "text", x = as.Date("2021-04-01"), y = 0.5, 
           label = "Vaccinations fully availabile", hjust = 0, vjust = 1, size = 4)+
  annotate(geom = "text", x = as.Date("2021-04-01"), y = 5.5, 
           label = "TE NM -> TX = 0\nTE TX -> NM = 11.27%", hjust = 0, vjust = 1, size = 4)+
  annotate(geom = "text", x = as.Date("2021-02-07"), y = 0.36, 
           label = "Schools opened", hjust = 0, vjust = 1, size = 4, angle = 90)+
  annotate(geom = "text", x = as.Date("2021-02-07"), y = 5.1, 
           label = "TE = 0", hjust = 0, vjust = 1, size = 4, angle = 90)+
  labs(x = "", y = "Cases (log2 + 1)")+
  theme_classic()
  

  #Venn quarantine
pl_NM_TX1 <- draw.pairwise.venn(round(Marg_1r, digits = 3), round(Marg_1c, digits = 3),
                                  cross.area = MI_NM_TX1, col = c("red", "black"),
                                  fill = c("red", "black"))
p2_NM_TX1 <- ggpubr::annotate_figure(pl_NM_TX1, fig.lab = "Out-of-state quarantine", 
                                  fig.lab.pos = c("top.left"), fig.lab.size = 12, 
                                  fig.lab.face = "bold")

  #Venn school closure
pl_NM_TX2 <- draw.pairwise.venn(round(Marg_2r, digits = 3), round(Marg_2c, digits = 3),
                                 cross.area = MI_NM_TX2, col = c("red", "black"),
                                 fill = c("red", "black"))
p2_NM_TX2 <- ggpubr::annotate_figure(pl_NM_TX2, fig.lab = "Schools open", 
                                      fig.lab.pos = c("top.left"), fig.lab.size = 12, 
                                      fig.lab.face = "bold")
  #Vaccinations available
pl_NM_TX3 <- draw.pairwise.venn(round(Marg_3r, digits = 3), round(Marg_3c, digits = 3),
                                 cross.area = MI_NM_TX3, col = c("red", "black"),
                                 fill = c("red", "black"))
p2_NM_TX3 <- ggpubr::annotate_figure(pl_NM_TX3, fig.lab = "Vaccinations fully available", 
                                     fig.lab.pos = c("top.left"), fig.lab.size = 12, 
                                     fig.lab.face = "bold")

States <- c("New Mexico", 'Texas' )
b <- c(1,2); c <- c(5,3)
ab_dat <- as.data.frame(cbind(States,b,c))
p1 <- ggplot(ab_dat, aes(x=b, y = c, color = States))+
  geom_point()+
  scale_color_manual(values = c('red', 'black'))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(nrow = 1))
mylegend <- cowplot::get_legend(p1)

gridExtra::grid.arrange(p2_NM_TX1, p2_NM_TX2, p2_NM_TX3, mylegend, ncol = 3, nrow =2, heights = c(4,0.5))

