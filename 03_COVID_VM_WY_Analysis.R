#Read Me ####
#This is an effort to understand how COVID 
#NM intervention dates
  #March 2020 -> Feb 2021 Out-of-state quarantine
  #March 2021 - schools open
  #November 2021 - vaccinations available

#covid data from 
#https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36


#libraries ####

    #uncomment if you need to install the packages
#install.packages(c("tidyverse", "RTransferEntropy", "future", "entropy", "VennDiagram"))

library(tidyverse)            #data wrangling
library(RTransferEntropy)     #calculates transfer entropy and bootstraps
library(future)               #enables parallel processing
library(entropy)              #calcuates Shannon entropy and mutual information
library(VennDiagram)          #produces Venn Diagrams
library(growthcurver)         #calculate r for logistic map

#enable parallel processing
plan(multisession)

day0 <- as.Date("2020-03-15")
day1 <- as.Date("2021-2-1")
day2 <- as.Date("2022-2-1")

#data ####
datNYT <-  read.csv("NYTiems us-counties-2022.csv")
datNM_TX <- datNYT %>% 
  mutate(id = row_number()) %>% 
  mutate(date = as.Date(date)) %>% 
  filter(state == "Vermont"| state == "Wyoming" ) %>% 
  select(date, state, cases_avg_per_100k) %>% 
  pivot_wider(id_cols = date, names_from = state, values_from = cases_avg_per_100k) %>% 
  drop_na() %>% 
  rename(Date1 = date)


#MI and TE series 1 - out-of-state quarantine in place ####
datNM_TX1 <- datNM_TX %>% 
  filter(Date1 >="2020-03-11", Date1 < day1) %>% 
  rename(Vermont = "Vermont")

 #Descritize data to bins NM_TX 1
quants1 <- datNM_TX1 %>% 
  select(!Date1) %>% 
  sapply(quantile, probs = c(0.25, 0.75))

NM1_bwidth <- (2*(quants1[2,2] - quants1[1,2]))/nrow(datNM_TX1)^(1/3)
NM1_bins <- ceiling((max(datNM_TX1$Vermont)-min(datNM_TX1$Vermont))/NM1_bwidth)

TX1_bwidth <- (2*(quants1[2,1] - quants1[1,1]))/nrow(datNM_TX1)^(1/3)
TX1_bins <- ceiling((max(datNM_TX1$Wyoming)-min(datNM_TX1$Wyoming))/TX1_bwidth)

  #mutual information calc
NM_TX1_2d <- discretize2d(datNM_TX1$Vermont, datNM_TX1$Wyoming, 
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
TE_NM_TX1 <- transfer_entropy(datNM_TX1$Vermont, datNM_TX1$Wyoming)
TE_NM_TX1

#MI and TE series 2 - quarantine eased ####
datNM_TX2 <- datNM_TX %>% 
  filter(Date1 >=day1, Date1 <day2)%>% 
  rename(Vermont = "Vermont")

#Descritize data to bins NM_TX 1
quants2 <- datNM_TX2 %>% 
  select(!Date1) %>% 
  sapply(quantile, probs = c(0.25, 0.75))

NM2_bwidth <- (2*(quants2[2,2] - quants2[1,2]))/nrow(datNM_TX2)^(1/3)
NM2_bins <- ceiling((max(datNM_TX2$Vermont)-min(datNM_TX2$Vermont))/NM2_bwidth)

TX2_bwidth <- (2*(quants2[2,1] - quants2[1,1]))/nrow(datNM_TX2)^(1/3)
TX2_bins <- ceiling((max(datNM_TX2$Wyoming)-min(datNM_TX2$Wyoming))/TX2_bwidth)

#mutual information calc
NM_TX2_2d <- discretize2d(datNM_TX2$Vermont, datNM_TX2$Wyoming, 
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
TE_NM_TX2 <- transfer_entropy(datNM_TX2$Vermont, datNM_TX2$Wyoming)
TE_NM_TX2

#MI and TE plots ####
  #time series

date_range <- which(datNM_TX$Date1 %in% as.Date(c(day0,day1, day2)))


datNM_TX %>% 
  pivot_longer(cols = Wyoming:"Vermont", names_to = "State", values_to = "LogCases") %>% 
  filter(Date1 <= day2) %>% 
  ggplot(aes(x = Date1, y = LogCases, col = State))+
  geom_line(size = 1)+
  geom_vline(xintercept = as.Date(day0),color = "black", size = 1)+
  geom_vline(xintercept = as.Date(day1),color = "black", size = 1)+
  geom_vline(xintercept = as.Date(day2),color = "black", size = 1)+
  scale_color_manual(values = c("Vermont" = "red", Wyoming ="black"))+
  annotate(geom = "text", x = as.Date("2020-04-15"), y = 300,
           label = "Before vaccine massive administration", hjust = 0, vjust = 1, size = 4)+
  
  annotate(geom = "text", x = as.Date("2020-04-15"), y = 200, size = 4,
           label = "TE: VM -> WY =0\nTE: WY -> VM = 0", hjust = 0, vjust = 1, size =4)+
  
  annotate(geom = "text", x = as.Date("2021-06-01"), y = 300, 
           label = "After vaccine massive administration", hjust = 0, vjust = 1, size = 4)+
  
  annotate(geom = "text", x = as.Date("2021-06-01"), y = 200, 
           label = "TE: VM -> WY = 0.0323\nTE: WY -> VM = 0.037", hjust = 0, vjust = 1, size = 4)+
  
  labs(x = "", y = "Cases (7-day average per 100,000 people)")+
  theme_classic()+
  theme(legend.position = "bottom")
  
  #Venn quarantine - Shannon
grid.newpage()
pl_NM_TX1_SE <- draw.pairwise.venn(round(Marg_1r, digits = 3), round(Marg_1c, digits = 3),
                                                cross.area = 0, col = c("red", "black"),
                                                fill = c("red", "black"))
p2_NM_TX1_SE <- ggpubr::annotate_figure(pl_NM_TX1_SE, fig.lab = "Before vaccine massive administration\n   Shannon Entropy", 
                                     fig.lab.pos = c("top.left"), fig.lab.size = 12, 
                                     fig.lab.face = "bold")

grid.newpage()
pl_NM_TX2_SE <- draw.pairwise.venn(round(Marg_2r, digits = 3), round(Marg_2c, digits = 3),
                                   cross.area = 0, col = c("red", "black"),
                                   fill = c("red", "black"))
p2_NM_TX2_SE <- ggpubr::annotate_figure(pl_NM_TX2_SE, fig.lab = "After vaccine massive administration\n    Shannon Entropy", 
                                        fig.lab.pos = c("top.left"), fig.lab.size = 12, 
                                        fig.lab.face = "bold")

  #Venn quarantine - Mutual Information
grid.newpage()
pl_NM_TX1 <- draw.pairwise.venn(round(Marg_1r, digits = 3), round(Marg_1c, digits = 3),
                                  cross.area = MI_NM_TX1, col = c("red", "black"),
                                  fill = c("red", "black"))
p2_NM_TX1 <- ggpubr::annotate_figure(pl_NM_TX1, fig.lab = "Mutual Information", 
                                  fig.lab.pos = c("top.left"), fig.lab.size = 12, 
                                  fig.lab.face = "bold")

  #Venn quarantine eased
grid.newpage()
pl_NM_TX2 <- draw.pairwise.venn(round(Marg_2r, digits = 3), round(Marg_2c, digits = 3),
                                 cross.area = MI_NM_TX2, col = c("red", "black"),
                                 fill = c("red", "black"))
p2_NM_TX2 <- ggpubr::annotate_figure(pl_NM_TX2, fig.lab = "Mutual Information", 
                                      fig.lab.pos = c("top.left"), fig.lab.size = 12, 
                                      fig.lab.face = "bold")

States <- c("Vermont", 'Wyoming' )
b <- c(1,2); c <- c(5,3)
ab_dat <- as.data.frame(cbind(States,b,c))
p1 <- ggplot(ab_dat, aes(x=b, y = c, color = States))+
  geom_point()+
  scale_color_manual(values = c('red', 'black'))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(nrow = 1))
mylegend <- cowplot::get_legend(p1)

gridExtra::grid.arrange(p2_NM_TX1_SE, p2_NM_TX2_SE, p2_NM_TX1, p2_NM_TX2, mylegend, nrow = 3, 
                        ncol =2, heights = c(2,2, 0.15))

#Logistic map use to calculate reproductive rate at between these periods ####
  #data wrangle
      #all data 
dat_reprod <- datNYT %>% 
  filter(state == "Vermont") %>% 
  select(date, state, cases_avg_per_100k) %>% 
  mutate(CumCases0 = cumsum(cases_avg_per_100k),
         Time0 = row_number())

      #first period out-of-state quarantine for plotting
dat_reprod1 <- datNYT %>% 
  filter(state == "Vermont") %>% 
  select(date, state, cases_avg_per_100k) %>% 
  mutate(CumCases1 = cumsum(cases_avg_per_100k),
         Time1 = row_number())%>% 
  filter(date >="2020-03-11", date < day1) 

      #second period out-of-state quarantine easing for plotting
dat_reprod2 <- datNYT %>% 
  filter(state == "Vermont") %>% 
  select(date, state, cases_avg_per_100k) %>% 
  mutate(CumCases2 = cumsum(cases_avg_per_100k),
         Time2 = row_number())%>% 
  filter(date >=day1,date <day2) 

  #plot
ggplot(dat_reprod, aes(x=Time0, y = CumCases0))+
  geom_point(size = 0.04)+
  geom_smooth(method = "glm", method.args = list(family = gaussian(link="log")), size = 1, fill = "blue", 
              color = "blue")+
  geom_line(data=dat_reprod1, aes(x=Time1, y=CumCases1), color='grey60', size = 2) + 
  geom_line(data=dat_reprod2, aes(x=Time2, y=CumCases2), color='grey80', size = 2)+
  theme_classic()+
  ylab("Cummulative cases (7-day average/100K)")+
  xlab("Time")+
  geom_text(aes(550,15000, label="Overall: R = 0.006"))+
  geom_text(aes(150,0, label="Quarantine: R = 0.030"))+
  geom_text(aes(250,9000, label="Eased quarantiene: R = 0.004"))


  #fit a logistic map to overall and different periods to obtain reproductive rate
gc_fit0 <- SummarizeGrowth(dat_reprod$Time0, dat_reprod$CumCases0)
summary(gc_fit0)

gc_fit1 <- SummarizeGrowth(dat_reprod1$Time1, dat_reprod1$CumCases1)
summary(gc_fit1)

gc_fit2 <- SummarizeGrowth(dat_reprod2$Time2, dat_reprod2$CumCases2)
summary(gc_fit2)


