setwd("C:/Users/EALESO/OneDrive - The University of Melbourne/Research Ideas/ApparentReproductionNumber")
source('functions.R')

library(RColorBrewer)

sim_init <- data.frame(time = seq(-99,0),
                       Inc = rep(1,100),
                       Rt = rep(1,100))


#Generation time (unimportant)
n <- 2.29
b <- 0.36 


# Define R value lists

R_values <-  1.03+0.5*sin(2*pi*seq(0,5,length=500))





sim_res1 <- run_sim(sim_init, R_values, n, b)



# Prevalence data
ieff1 <- estimate_Ieff(sim_res1, conv_function = "prev2", conv_param1 = 100)

conv1 <- data.frame(time=seq(0,100),
                    prob = prev_conv_2()/sum(prev_conv_2()),
                    data = "Prevalence")



# Symptom onset
mean <- 6
sd <- 3.1
conv_param1 <- mean**2/sd**2
conv_param2 <- 1/(sd**2/mean)

ieff2 <- estimate_Ieff(sim_res1, conv_function = "gamma", conv_param1 = conv_param1, conv_param2 = conv_param2)
ieff2b <- ieff2
ieff2b$Inc <- ieff2b$Ieff


conv2 <- data.frame(time=seq(0,100),
                    prob = dgamma(seq(0,100),conv_param1, conv_param2)/sum(dgamma(seq(0,100),conv_param1, conv_param2)),
                    data = "Symptom onset")

# Death
mean <- 15
sd <- 6.9
conv_param1 <- mean**2/sd**2
conv_param2 <- 1/(sd**2/mean)

ieff3 <- estimate_Ieff(ieff2b, conv_function = "gamma", conv_param1 = conv_param1, conv_param2 = conv_param2)


new_dist <- rep(0,202)
for(i in 1:100){
  new_dist[i:(i+100)] <- new_dist[i:(i+100)]+conv2$prob[2:101][i]*dgamma(seq(0,100), conv_param1, conv_param2)
}
conv3 <- data.frame(time=seq(0,100),
                    prob = new_dist[1:101]/sum(new_dist[1:101]),
                    data = "Deaths")
# Hospitalisation
mean <- 7.8
sd <- sqrt(35.7)
conv_param1 <- mean**2/sd**2
conv_param2 <- 1/(sd**2/mean)

ieff4 <- estimate_Ieff(ieff2b, conv_function = "gamma", conv_param1 = conv_param1, conv_param2 = conv_param2)

new_dist <- rep(0,202)
for(i in 1:100){
  new_dist[i:(i+100)] <- new_dist[i:(i+100)]+conv2$prob[2:101][i]*dgamma(seq(0,100), conv_param1, conv_param2)
}
conv4 <- data.frame(time=seq(0,100),
                    prob = new_dist[1:101]/sum(new_dist[1:101]),
                    data = "Hospitalisations")



################################################################################################################
# Estimate Reproduction number


rapp1 <- estimate_Rapp(ieff1, n, b)
rapp2 <- estimate_Rapp(ieff2, n, b)
rapp3 <- estimate_Rapp(ieff3, n, b)
rapp4 <- estimate_Rapp(ieff4, n, b)


rapp1$data <- "Prevalence"
rapp2$data <- "Symptom\nonset"
rapp3$data <- "Deaths"
rapp4$data <- "Hospitalisations"


df_square <- rbind(rapp1,
                   rapp2,
                   rapp3,
                   rapp4)


pal <- c("black", brewer.pal(4,"Dark2")[1:4])

plt1<-ggplot(df_square)+
  geom_line(data=df_square[df_square$data=="Prevalence",], aes(x=time, y=Inc/1000,color=' Infection\n incidence'), size=1)+
  geom_line(aes(x=time, y=Ieff/1000, color = data), size=1)+
  coord_cartesian(xlim = c(0,250))+
  theme_bw()+
  scale_color_manual(values=pal)+
  labs(color="Timeseries")+
  ylab("Infection incidence")+
  xlab("Time (days)")+
  theme(legend.position = c(0.8,0.8),
        legend.background = element_rect(color='black'))+
  labs(tag="A")



df_conv <- rbind(conv1,
                 conv2,
                 conv3,
                 conv4)



plt2<-ggplot(df_conv)+
  geom_line(aes(x=time, y=prob, color = data), size=1)+
  theme_bw()+
  ylab("Probablity density")+
  xlab("Time delay (days)")+
  scale_color_brewer(palette = "Dark2")+
  labs(color="Timeseries")+
  coord_cartesian(xlim=c(0,50))+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        legend.background = element_rect(color='black'))+
  labs(tag="B")



plt_grid <- plot_grid(plt1, plt2, nrow=1, rel_widths = c(2,1.2))


ggsave("figure/figure6.pdf", plt_grid, height = 5, width=14)


