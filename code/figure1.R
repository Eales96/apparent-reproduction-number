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
R_values_sine <- 1.+0.5*sin(2*pi*seq(0,5,length=500))


sim_res1 <- run_sim(sim_init, R_values_square, n, b)
sim_res2 <- run_sim(sim_init, R_values_sine, n, b)


# First values
mean <- 12
sd <- 3

conv_param1 <- mean**2/sd**2
conv_param2 <- 1/(sd**2/mean)

ieff1 <- estimate_Ieff(sim_res1, conv_function = "gamma", conv_param1 = conv_param1, conv_param2 = conv_param2)
ieff2 <- estimate_Ieff(sim_res2, conv_function = "gamma", conv_param1 = conv_param1, conv_param2 = conv_param2)
rapp1_sq <- estimate_Rapp(ieff1, n, b)
rapp1_sin <- estimate_Rapp(ieff2, n, b)

rapp1_sq$mean <- "12 days"
rapp1_sq$sd <- "3 days"
rapp1_sin$mean <- "12 days"
rapp1_sin$sd <- "3 days"


conv1 <- data.frame(time=seq(0,100),
                    prob = dgamma(seq(0,100),conv_param1, conv_param2),
                    mean = "12 days",
                    sd = "3 days")


# Second values
mean <- 6
sd <- 3

conv_param1 <- mean**2/sd**2
conv_param2 <- 1/(sd**2/mean)

ieff1 <- estimate_Ieff(sim_res1, conv_function = "gamma", conv_param1 = conv_param1, conv_param2 = conv_param2)
ieff2 <- estimate_Ieff(sim_res2, conv_function = "gamma", conv_param1 = conv_param1, conv_param2 = conv_param2)
rapp2_sq <- estimate_Rapp(ieff1, n, b)
rapp2_sin <- estimate_Rapp(ieff2, n, b)

rapp2_sq$mean <- "06 days"
rapp2_sq$sd <- "3 days"
rapp2_sin$mean <- "06 days"
rapp2_sin$sd <- "3 days"


conv2 <- data.frame(time=seq(0,100),
                    prob = dgamma(seq(0,100),conv_param1, conv_param2),
                    mean = "06 days",
                    sd = "3 days")
# Third values
mean <- 18
sd <- 3

conv_param1 <- mean**2/sd**2
conv_param2 <- 1/(sd**2/mean)

ieff1 <- estimate_Ieff(sim_res1, conv_function = "gamma", conv_param1 = conv_param1, conv_param2 = conv_param2)
ieff2 <- estimate_Ieff(sim_res2, conv_function = "gamma", conv_param1 = conv_param1, conv_param2 = conv_param2)
rapp3_sq <- estimate_Rapp(ieff1, n, b)
rapp3_sin <- estimate_Rapp(ieff2, n, b)

rapp3_sq$mean <- "18 days"
rapp3_sq$sd <- "3 days"
rapp3_sin$mean <- "18 days"
rapp3_sin$sd <- "3 days"

conv3 <- data.frame(time=seq(0,100),
                    prob = dgamma(seq(0,100),conv_param1, conv_param2),
                    mean = "18 days",
                    sd = "3 days")


# Fourth values
mean <- 12
sd <- 6

conv_param1 <- mean**2/sd**2
conv_param2 <- 1/(sd**2/mean)

ieff1 <- estimate_Ieff(sim_res1, conv_function = "gamma", conv_param1 = conv_param1, conv_param2 = conv_param2)
ieff2 <- estimate_Ieff(sim_res2, conv_function = "gamma", conv_param1 = conv_param1, conv_param2 = conv_param2)
rapp4_sq <- estimate_Rapp(ieff1, n, b)
rapp4_sin <- estimate_Rapp(ieff2, n, b)

rapp4_sq$mean <- "12 days"
rapp4_sq$sd <- "6 days"
rapp4_sin$mean <- "12 days"
rapp4_sin$sd <- "6 days"

conv4 <- data.frame(time=seq(0,100),
                    prob = dgamma(seq(0,100),conv_param1, conv_param2),
                    mean = "12 days",
                    sd = "6 days")


# Fifth values
mean <- 12
sd <- 9

conv_param1 <- mean**2/sd**2
conv_param2 <- 1/(sd**2/mean)

ieff1 <- estimate_Ieff(sim_res1, conv_function = "gamma", conv_param1 = conv_param1, conv_param2 = conv_param2)
ieff2 <- estimate_Ieff(sim_res2, conv_function = "gamma", conv_param1 = conv_param1, conv_param2 = conv_param2)
rapp5_sq <- estimate_Rapp(ieff1, n, b)
rapp5_sin <- estimate_Rapp(ieff2, n, b)

rapp5_sq$mean <- "12 days"
rapp5_sq$sd <- "9 days"
rapp5_sin$mean <- "12 days"
rapp5_sin$sd <- "9 days"

conv5 <- data.frame(time=seq(0,100),
                    prob = dgamma(seq(0,100),conv_param1, conv_param2),
                    mean = "12 days",
                    sd = "9 days")




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


plt1 <- ggplot(df_square)+
  geom_line(data=df_square[df_square$mean=="12 days",], aes(x=time, y=Rt),color='black', size=1)+
  geom_line(aes(x=time, y=Rapp, color = interaction(mean,sd), linetype= interaction(mean,sd)), size=1)+
  coord_cartesian(xlim = c(-5,95))+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  labs(color=paste0("Convolution (", expression(mu), ".", expression(sigma),")"), linetype=paste0("Convolution (", expression(mu), ".", expression(sigma),")"))+
  ylab("Time-varying reproduction number")+
  xlab("Time (days)")+
  theme(legend.position = c(0.2,0.24),
        legend.background = element_rect(color='black'))+
  labs(tag="A")




plt2 <- ggplot(df_sine)+
  geom_line(data=df_sine[df_sine$mean=="12 days",], aes(x=time, y=Rt),color='black', size=1)+
  geom_line(aes(x=time, y=Rapp, color = interaction(mean,sd),  linetype = interaction(mean,sd)), size=1)+
  coord_cartesian(xlim = c(-5,95))+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  labs(color=paste0("Convolution (", expression(mu), ".", expression(sigma),")"), linetype=paste0("Convolution (", expression(mu), ".", expression(sigma),")"))+
  ylab("Time-varying reproduction number")+
  xlab("Time (days)")+
  theme(legend.position = "none")+
  labs(tag="B")



plt3 <- ggplot(df_conv)+
  geom_line(aes(x=time, y=prob, color = interaction(mean,sd), linetype = interaction(mean,sd)), size=1)+
  theme_bw()+
  ylab("Probablity density")+
  xlab("Time delay (days)")+
  scale_color_brewer(palette = "Dark2")+
  labs(color=paste0("Convolution (", expression(mu), ".", expression(sigma),")"), linetype=paste0("Convolution (", expression(mu), ".", expression(sigma),")"))+
  coord_cartesian(xlim=c(0,25))+
  theme(legend.position = "none")+
  labs(tag="C")

plt_grid <- plot_grid(plt1, plt2, plt3, nrow = 3)


ggsave("figure/figure1.pdf", plt_grid, height = 12.5, width=5.5)
