setwd('./apparent-reproduction-number')
source('code/functions.R')
library(cowplot)
library(ggplot2)
library(egg)

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
conv_param1 <- 100

ieff1 <- estimate_Ieff(sim_res1, conv_function = "prev2", conv_param1 = conv_param1)
ieff2 <- estimate_Ieff(sim_res2, conv_function = "prev2", conv_param1 = conv_param1, conv_param2 = conv_param2)
rapp1_sq <- estimate_Rapp(ieff1, n, b)
rapp1_sin <- estimate_Rapp(ieff2, n, b)

rapp1_sq$freq <- "100 days"
rapp1_sin$freq <- "100 days"


conv1 <- data.frame(time=seq(0,100),
                    prob = rep_test_conv_2(conv_param1)/sum(rep_test_conv_2(conv_param1)),
                    mean = "every 100 day(s)")

dat_mns1 <- get_mean_sd(rep_test_conv_2(conv_param1))


# Second values
conv_param1 <- 14

ieff1 <- estimate_Ieff(sim_res1, conv_function = "prev2", conv_param1 = conv_param1)
ieff2 <- estimate_Ieff(sim_res2, conv_function = "prev2", conv_param1 = conv_param1, conv_param2 = conv_param2)
rapp2_sq <- estimate_Rapp(ieff1, n, b)
rapp2_sin <- estimate_Rapp(ieff2, n, b)

rapp2_sq$freq <- "14 days"
rapp2_sin$freq <- "14 days"


conv2 <- data.frame(time=seq(0,100),
                    prob = rep_test_conv_2(conv_param1)/sum(rep_test_conv_2(conv_param1)),
                    mean = "every 14 day(s)")

dat_mns2 <- get_mean_sd(rep_test_conv_2(conv_param1))


# First values
conv_param1 <- 7

ieff1 <- estimate_Ieff(sim_res1, conv_function = "prev2", conv_param1 = conv_param1)
ieff2 <- estimate_Ieff(sim_res2, conv_function = "prev2", conv_param1 = conv_param1, conv_param2 = conv_param2)
rapp3_sq <- estimate_Rapp(ieff1, n, b)
rapp3_sin <- estimate_Rapp(ieff2, n, b)

rapp3_sq$freq <- "7 days"
rapp3_sin$freq <- "7 days"


conv3 <- data.frame(time=seq(0,100),
                    prob = rep_test_conv_2(conv_param1)/sum(rep_test_conv_2(conv_param1)),
                    mean = "every 7 day(s)")

dat_mns3 <- get_mean_sd(rep_test_conv_2(conv_param1))


# First values
conv_param1 <- 3

ieff1 <- estimate_Ieff(sim_res1, conv_function = "prev2", conv_param1 = conv_param1)
ieff2 <- estimate_Ieff(sim_res2, conv_function = "prev2", conv_param1 = conv_param1, conv_param2 = conv_param2)
rapp4_sq <- estimate_Rapp(ieff1, n, b)
rapp4_sin <- estimate_Rapp(ieff2, n, b)

rapp4_sq$freq <- "3 days"
rapp4_sin$freq <- "3 days"


conv4 <- data.frame(time=seq(0,100),
                    prob = rep_test_conv_2(conv_param1)/sum(rep_test_conv_2(conv_param1)),
                    mean = "every 3 day(s)")

dat_mns4 <- get_mean_sd(rep_test_conv_2(conv_param1))


# Fifth values
conv_param1 <- 1

ieff1 <- estimate_Ieff(sim_res1, conv_function = "prev2", conv_param1 = conv_param1)
ieff2 <- estimate_Ieff(sim_res2, conv_function = "prev2", conv_param1 = conv_param1, conv_param2 = conv_param2)
rapp5_sq <- estimate_Rapp(ieff1, n, b)
rapp5_sin <- estimate_Rapp(ieff2, n, b)

rapp5_sq$freq <- "1 days"
rapp5_sin$freq <- "1 days"


conv5 <- data.frame(time=seq(0,100),
                    prob = rep_test_conv_2(conv_param1)/sum(rep_test_conv_2(conv_param1)),
                    mean = "every 1 day(s)")

dat_mns5 <- get_mean_sd(rep_test_conv_2(conv_param1))



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


df_conv$mean <- factor(df_conv$mean, levels = c("every 1 day(s)","every 3 day(s)",
                                                "every 7 day(s)","every 14 day(s)",
                                                "every 100 day(s)"))

df_square$freq <- factor(df_square$freq, levels = c("1 days","3 days",
                                                    "7 days","14 days",
                                                    "100 days"))

df_sine$freq <- factor(df_sine$freq, levels = c("1 days","3 days",
                                                    "7 days","14 days",
                                                    "100 days"))

plt1 <- ggplot(df_square)+
  geom_line(data=df_square[df_square$freq=="100 days",], aes(x=time, y=Rt),color='black', size=1)+
  geom_line(aes(x=time, y=Rapp, color = freq, linetype= freq), size=1)+
  coord_cartesian(xlim = c(-5,95))+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  labs(color="test frequency (days^-1)",linetype="test frequency (days^-1)")+
  ylab("Time-varying reproduction number")+
  xlab("Time (days)")+
  theme(legend.position = "none",
        legend.background = element_rect(color='black'))+
  labs(tag="A")



plt1
plt2 <- ggplot(df_sine)+
  geom_line(data=df_sine[df_sine$freq=="100 days",], aes(x=time, y=Rt),color='black', size=1)+
  geom_line(aes(x=time, y=Rapp, color = freq,  linetype = freq), size=1)+
  coord_cartesian(xlim = c(-5,95))+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  labs(color="test frequency (days^-1)", linetype="test frequency (days^-1)")+
  ylab("Time-varying reproduction number")+
  xlab("Time (days)")+
  theme(legend.position = "none")+
  labs(tag="B")


plt2

df_conv$mean <- factor(df_conv$mean, levels = c("every 1 day(s)","every 3 day(s)",
                                                   "every 7 day(s)","every 14 day(s)",
                                                   "every 100 day(s)"))
plt3 <- ggplot(df_conv)+
  geom_line(aes(x=time, y=prob, color = mean, linetype = mean), size=1)+
  theme_bw()+
  ylab("Probablity density")+
  xlab("Time delay (days)")+
  scale_color_brewer(palette = "Dark2")+
  labs(color="Testing frequency", linetype="Testing frequency")+
  coord_cartesian(xlim=c(0,25))+
  theme(legend.position = c(0.8,0.25),
        legend.background = element_rect(color='black'),
        panel.grid = element_blank())+
  labs(tag="C")

plt3

df_mns
df_mns$freq <- c("every 100 day(s)", "every 14 day(s)","every 7 day(s)", "every 3 day(s)", "every 1 day(s)")

df_mns$freq <- factor(df_mns$freq, levels = c("every 1 day(s)","every 3 day(s)",
                                                "every 7 day(s)","every 14 day(s)",
                                                "every 100 day(s)"))
plt4<-ggplot(data=df_mns)+
  geom_point(aes(x=mu,y=sd, color=freq), size=2)+
  theme_bw()+
  xlab("Mean")+
  ylab("Standard deviation")+
  scale_color_brewer(palette = "Dark2")+
  theme(legend.position = "none")+
  ggtitle("Convolution function")+
  theme(plot.title = element_text(hjust=0.5))






plt34 <- plt3 +
  annotation_custom(
    ggplotGrob(plt4),
    xmin = 10, xmax = 25, ymin = 0.25, ymax = 0.5
  )
ggsave(filename = "inset-plot.png")

plt34

plt_gridab <- plot_grid(plt1, plt2, nrow = 2)

plt_grid <- plot_grid(plt_gridab, plt34, nrow=1, rel_widths = c(1,1.5))


ggsave("figure/figure3.pdf", plt_grid, height = 6, width=10)

###########################################


