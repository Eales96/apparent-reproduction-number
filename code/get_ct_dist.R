library(rstan)
ct_fit<-readRDS("dat/ct_fit.rds")

ff<-rstan::extract(ct_fit)

dp_mu<-mean(ff$log_dp_mean)
dp_sd<-mean(ff$log_dp_sd)

wp_mu<-mean(ff$log_wp_mean)
wp_sd<-mean(ff$log_wp_sd)

wr_mu<-mean(ff$log_wr_mean)
wr_sd<-mean(ff$log_wr_sd)

dp_midpoint=20
wp_midpoint=5
wr_midpoint=12

dp <- exp(rnorm(1000, dp_mu, dp_sd))*dp_midpoint
wp <- exp(rnorm(1000, wp_mu, wp_sd))*wp_midpoint
wr <- exp(rnorm(1000, wr_mu, wr_sd))*wr_midpoint

t<-seq(0,100)
tp <- 5
ct_thresh <- 37.0


new_df <- data.frame()

ct_array<-array(NA,dim=c(101,1000))

for(i in seq_len(length(t))){
  if(t[i] <=tp){
    ct <- dp/wp * (t[i]-(tp-wp))
  } else{
    ct <- dp - dp/wr*(t[i]-tp)
  }

  ct <- 40-ct

  ct_array[i,]<- ct

}

saveRDS(ct_array, "output/ct_array.rds")


