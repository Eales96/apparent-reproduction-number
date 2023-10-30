setwd("C:/Users/EALESO/OneDrive - The University of Melbourne/Research Ideas/ApparentReproductionNumber")
source('functions.R')

sim_init <- data.frame(time = seq(-99,0),
                       Inc = rep(1,100),
                       Rt = rep(1,100))


#Generation time (unimportant)
n <- 2.29
b <- 0.36


# Define R value lists
R_values_square <- c(rep(1.5,50), rep(0.5,50),rep(1.5,50), rep(0.5,50),rep(1.5,50), rep(0.5,50),rep(1.5,50), rep(0.5,50),rep(1.5,50), rep(0.5,50))
R_values_sine <- 1.+0.5*sin(2*pi*seq(0,5,length=500))


sim_res1 <- run_sim(sim_init, R_values_square, n, b)
sim_res2 <- run_sim(sim_init, R_values_sine, n, b)



# First values
conv_param1 <- 37

ieff1 <- estimate_Ieff(sim_res1, conv_function = "prev1", conv_param1 = conv_param1)
ieff2 <- estimate_Ieff(sim_res2, conv_function = "prev1", conv_param1 = conv_param1)
rapp1_sq <- estimate_Rapp(ieff1, n, b)
rapp1_sin <- estimate_Rapp(ieff2, n, b)

rapp1_sq$freq <- 37
rapp1_sin$freq <- 37


conv1 <- data.frame(time=seq(0,100),
                    prob = prev_conv(conv_param1)/sum(prev_conv(conv_param1)),
                    mean = 37)

dat_mns1 <- get_mean_sd(prev_conv(conv_param1))


# Second values
conv_param1 <- 35

ieff1 <- estimate_Ieff(sim_res1, conv_function = "prev1", conv_param1 = conv_param1)
ieff2 <- estimate_Ieff(sim_res2, conv_function = "prev1", conv_param1 = conv_param1)
rapp2_sq <- estimate_Rapp(ieff1, n, b)
rapp2_sin <- estimate_Rapp(ieff2, n, b)

rapp2_sq$freq <- 35
rapp2_sin$freq <- 35


conv2 <- data.frame(time=seq(0,100),
                    prob = prev_conv(conv_param1)/sum(prev_conv(conv_param1)),
                    mean = 35)

dat_mns2 <- get_mean_sd(prev_conv(conv_param1))

# Third values
conv_param1 <- 30

ieff1 <- estimate_Ieff(sim_res1, conv_function = "prev1", conv_param1 = conv_param1)
ieff2 <- estimate_Ieff(sim_res2, conv_function = "prev1", conv_param1 = conv_param1)
rapp3_sq <- estimate_Rapp(ieff1, n, b)
rapp3_sin <- estimate_Rapp(ieff2, n, b)

rapp3_sq$freq <- 30
rapp3_sin$freq <- 30


conv3 <- data.frame(time=seq(0,100),
                    prob = prev_conv(conv_param1)/sum(prev_conv(conv_param1)),
                    mean = 30)

dat_mns3 <- get_mean_sd(prev_conv(conv_param1))

# Fourth values
conv_param1 <- 25

ieff1 <- estimate_Ieff(sim_res1, conv_function = "prev1", conv_param1 = conv_param1)
ieff2 <- estimate_Ieff(sim_res2, conv_function = "prev1", conv_param1 = conv_param1)
rapp4_sq <- estimate_Rapp(ieff1, n, b)
rapp4_sin <- estimate_Rapp(ieff2, n, b)

rapp4_sq$freq <- 25
rapp4_sin$freq <- 25


conv4 <- data.frame(time=seq(0,100),
                    prob = prev_conv(conv_param1)/sum(prev_conv(conv_param1)),
                    mean = 25)

dat_mns4 <- get_mean_sd(prev_conv(conv_param1))

# Fifth values
conv_param1 <- 20

ieff1 <- estimate_Ieff(sim_res1, conv_function = "prev1", conv_param1 = conv_param1)
ieff2 <- estimate_Ieff(sim_res2, conv_function = "prev1", conv_param1 = conv_param1)
rapp5_sq <- estimate_Rapp(ieff1, n, b)
rapp5_sin <- estimate_Rapp(ieff2, n, b)

rapp5_sq$freq <- 20
rapp5_sin$freq <- 20

conv5 <- data.frame(time=seq(0,100),
                    prob = prev_conv(conv_param1)/sum(prev_conv(conv_param1)),
                    mean = 20)

dat_mns5 <- get_mean_sd(prev_conv(conv_param1))



############################################################################################

df_square <- rbind(rapp1_sq,
                   rapp2_sq,
                   rapp3_sq,
                   rapp4_sq,
                   rapp5_sq)

df_sine <- rbind(rapp1_sin,
                 rapp2_sin,
                 rapp3_sin,
                 rapp4_sin,
                 rapp5_sin)

df_conv <- rbind(conv1,
                 conv2,
                 conv3,
                 conv4,
                 conv5)

df_mns <- rbind(dat_mns1,
                dat_mns2,
                dat_mns3,
                dat_mns4,
                dat_mns5)




df_square$freq <- factor(df_square$freq)
plt1 <- ggplot(df_square)+
  geom_line(data=df_square[df_square$freq=="37",], aes(x=time, y=Rt),color='black', size=1)+
  geom_line(aes(x=time, y=Rapp, color = freq, linetype= freq), size=0.5)+
  coord_cartesian(xlim = c(-5,70))+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  labs(color="Ct positivty threshold",linetype="Ct positivty threshold")+
  ylab("Time-varying reproduction number")+
  xlab("Time (days)")+
  theme(legend.position = "none",
        legend.background = element_rect(color='black'))+
  labs(tag="A")



plt1
df_sine$freq <- factor(df_sine$freq)
plt2 <- ggplot(df_sine)+
  geom_line(data=df_sine[df_sine$freq=="37",], aes(x=time, y=Rt),color='black', size=1)+
  geom_line(aes(x=time, y=Rapp, color = freq,  linetype = freq), size=0.5)+
  coord_cartesian(xlim = c(-5,90))+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  labs(color="test frequency (days^-1)", linetype="test frequency (days^-1)")+
  ylab("Time-varying reproduction number")+
  xlab("Time (days)")+
  theme(legend.position = "none")+
  labs(tag="B")


plt2

df_conv$mean <- factor(df_conv$mean)

plt3 <- ggplot(df_conv)+
  geom_line(aes(x=time, y=prob, color = mean, linetype = mean), size=1)+
  theme_bw()+
  ylab("Probablity density")+
  xlab("Time delay (days)")+
  scale_color_brewer(palette = "Dark2")+
  labs(color="Ct positivity threshold", linetype="Ct positivity threshold")+
  coord_cartesian(xlim=c(0,25))+
  theme(legend.position = c(0.75,0.25),
        legend.background = element_rect(color='black'),
        panel.grid = element_blank())+
  labs(tag="C")

plt3

df_mns
df_mns$freq <- c("37","35","30","25","20")


plt4<-ggplot(data=df_mns)+
  geom_point(aes(x=mu,y=sd, color=freq), size=2)+
  theme_bw()+
  xlab("Mean")+
  ylab("Standard deviation")+
  scale_color_brewer(palette = "Dark2")+
  theme(legend.position = "none")+
  ggtitle("Convolution function")+
  theme(plot.title = element_text(hjust=0.5))






###########################################

df_power <- data.frame()
for(i in 37:20){
  row_df <- data.frame(ct=i,
                       rel_prob=sum(prev_conv(i))/sum(prev_conv(37)))
  df_power <- rbind(df_power, row_df)
}

plt5<-ggplot(df_power)+
  geom_point(aes(x=ct, y= rel_prob))+
  geom_line(aes(x=ct, y=rel_prob))+
  theme_bw()+
  xlab("Ct positivity threshold")+
  ylab("Probability of detection (relative to Ct=37)")+
  labs(tag="D")



library(egg)

plt34 <- plt3 +
  annotation_custom(
    ggplotGrob(plt4),
    xmin = 10, xmax = 25, ymin = 0.4, ymax = 0.7
  )


plt34

library(cowplot)
plt_gridab <- plot_grid(plt1, plt2, nrow = 2)

plt_grid <- plot_grid(plt_gridab, plt34,plt5, nrow=1, rel_widths = c(1.5,1.5,1))


ggsave("figure/figure4.pdf", plt_grid, height = 6, width=11)

