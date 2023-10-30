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


mean_conv <- seq(1,20, length.out = 50)
std_conv <- seq(1,5, length.out = 100)



## Square forcing function
df_diag_square <- data.frame()

for(i in seq_len(length(mean_conv))){
  for(j in seq_len(length(std_conv))){

    k <- mean_conv[i]**2 / std_conv[j]**2
    theta <- std_conv[j]**2/mean_conv[i]

    ieff1 <- estimate_Ieff(sim_res1, conv_function = "gamma", conv_param1 = k, conv_param2 = 1/theta)
    rapp1 <- estimate_Rapp(ieff1, n, b)
    diag1 <- diagnostics_square(rapp1)
    diag1$mean <- mean_conv[i]
    diag1$std <- std_conv[j]

    df_diag_square <- rbind(df_diag_square, diag1)
  }
}

df_diag_square2 <- df_diag_square[df_diag_square$mean>=df_diag_square$std,]

plt1<-ggplot(data = df_diag_square2)+
  geom_tile(aes(x=mean, y=std, fill=Tmax_T95), color='white',size=0.01)+
  theme_bw()+
  xlab("Mean (days)")+
  ylab("Standard deviation (days)")+
  scale_fill_distiller(palette = "Spectral")+
  labs(fill=expression(tau[max]-tau[0.95*max]))+
  theme(panel.grid  = element_blank(),
        legend.position = c(0.13,0.83),
        legend.background = element_rect(color='black'))+
  coord_cartesian(xlim=c(-1,20))+
  labs(tag="A")

plt1
plt2<-ggplot(data = df_diag_square2)+
  geom_tile(aes(x=mean, y=std, fill=T95_T05), color='white',size=0.01)+
  theme_bw()+
  xlab("Mean (days)")+
  ylab("Standard deviation (days)")+
  scale_fill_distiller(palette = "Spectral")+
  labs(fill=expression(tau[0.95*max]-tau[0.05*max]))+
  theme(panel.grid  = element_blank(),
        legend.position = c(0.13,0.83),
        legend.background = element_rect(color='black'))+
  coord_cartesian(xlim=c(-1,20))+
  labs(tag="B")



plt2





## Sine forcing function
df_diag_sine <- data.frame()

for(i in seq_len(length(mean_conv))){
  for(j in seq_len(length(std_conv))){

    k <- mean_conv[i]**2 / std_conv[j]**2
    theta <- std_conv[j]**2/mean_conv[i]

    ieff1 <- estimate_Ieff(sim_res2, conv_function = "gamma", conv_param1 = k, conv_param2 = 1/theta)
    rapp1 <- estimate_Rapp(ieff1, n, b)
    diag1 <- diagnostics_sine(rapp1)
    diag1$mean <- mean_conv[i]
    diag1$std <- std_conv[j]

    df_diag_sine <- rbind(df_diag_sine, diag1)
  }
}

df_diag_sine2 <- df_diag_sine[df_diag_sine$mean>=df_diag_sine$std,]





plt3<-ggplot(data = df_diag_sine2)+
  geom_tile(aes(x=mean, y=std, fill=peak_delay), color='white',size=0.01)+
  theme_bw()+
  xlab("Mean (days)")+
  ylab("Standard deviation (days)")+
  scale_fill_distiller(palette = "Spectral")+
  labs(fill=expression(tau[R(t)[max]]-tau[R[A](t)[max]]))+
  theme(panel.grid  = element_blank(),
        legend.position = c(0.13,0.83),
        legend.background = element_rect(color='black'))+
  coord_cartesian(xlim=c(-1,20))+
  labs(tag="C")


plt4<-ggplot(data = df_diag_sine2)+
  geom_tile(aes(x=mean, y=std, fill=prop_max), color='white',size=0.01)+
  theme_bw()+
  xlab("Mean (days)")+
  ylab("Standard deviation (days)")+
  scale_fill_distiller(palette = "Spectral")+
  labs(fill="Relative\ndifferences\nin peak\nR(t)")+
  theme(panel.grid  = element_blank(),
        legend.position = c(0.13,0.78),
        legend.background = element_rect(color='black'))+
  coord_cartesian(xlim=c(-1,20))+
  labs(tag="D")


plt4

library(cowplot)

plt_grid <- plot_grid(plt1, plt2, plt3, plt4, nrow=2)
ggsave("figure/figure2.pdf", plt_grid, height = 12, width=12)
