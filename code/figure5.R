setwd('./apparent-reproduction-number')
source('code/functions.R')
library(cowplot)
library(ggplot2)

sim_init <- data.frame(time = seq(-99,0),
                       Inc = rep(1,100),
                       Rt = rep(1,100))


#Generation time (unimportant)
n <- 2.29
b <- 0.36 


# Define R value lists
R_values_square <- c(rep(1.5,50), rep(0.5,50),rep(1.5,50), rep(0.5,50),rep(1.5,50), rep(0.5,50),rep(1.5,50), rep(0.5,50),rep(1.5,50), rep(0.5,50))



R_values <- c(rep(3,100),
              3-(3-0.7)*seq(1,9)/9,
              0.7-(0.7-0.8)*seq(1,47)/47,
              0.8-(0.8-0.7)*seq(1,35)/35,
              0.7-(0.7-0.9)*seq(1,19)/19,
              0.9-(0.9-0.9)*seq(1,30)/30,
              0.9-(0.9-1.4)*seq(1,29)/29,
              1.4-(1.4-1.2)*seq(1,13)/13,
              1.2-(1.2-1.3)*seq(1,30)/30,
              1.3-(1.3-1.2)*seq(1,17)/17,
              1.2-(1.2-0.9)*seq(1,5)/5,
              rep(0.9,100))




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
rapp2$data <- "Symptom onset"
rapp3$data <- "Deaths"
rapp4$data <- "Hospitalisations"


df_square <- rbind(rapp1,
                   rapp2,
                   rapp3,
                   rapp4)


plt1<-ggplot(df_square)+
  geom_line(data=df_square[df_square$data=="Prevalence",], aes(x=time, y=Rt),color='black', size=1)+
  geom_line(aes(x=time, y=Rapp, color = data), size=1)+
  coord_cartesian(xlim = c(75,400))+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  labs(color="Timeseries")+
  ylab("Time-varying reproduction number")+
  xlab("Time (days)")+
  theme(legend.position = c(0.8,0.8),
        legend.background = element_rect(color='black'))+
  geom_hline(yintercept = 1.0, color='black', linetype='dashed')+
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
        panel.grid = element_blank())+
  labs(tag="B")


dat_mns1 <- get_mean_sd(conv1$prob)
dat_mns2 <- get_mean_sd(conv2$prob)
dat_mns3 <- get_mean_sd(conv3$prob)
dat_mns4 <- get_mean_sd(conv4$prob)


df_mns <- rbind(dat_mns1,
                dat_mns2,
                dat_mns3,
                dat_mns4)
df_mns$data <- as.character(c("Prevalence","Symptom onset","Deaths","Hospitalisations"))


plt3<-ggplot(data=df_mns)+
  geom_point(aes(x=mu,y=sd, color=data), size=2)+
  theme_bw()+
  xlab("Mean")+
  ylab("Standard deviation")+
  scale_color_brewer(palette = "Dark2")+
  theme(legend.position = "none")+
  ggtitle("Convolution function")+
  theme(plot.title = element_text(hjust=0.5))


plt23 <- plt2 +
  annotation_custom(
    ggplotGrob(plt3),
    xmin = 25, xmax = 50, ymin = 0.05, ymax = 0.15
  )


plt23


plt_grid <- plot_grid(plt1, plt23, nrow=1, rel_widths = c(2,1.2))


ggsave("figure/figure5.pdf", plt_grid, height = 5, width=14)


