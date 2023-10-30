##################################################### Simulation functions ###########################################################################
# Advance single timestep

sim_time_step <- function(sim_df, Rt, n, b, t){

  inc <- sum(dgamma(seq(1,100), shape=n, rate=b) * rev(sim_df$Inc)[1:100] *Rt)  /sum(dgamma(seq(1,100), shape=n, rate=b))

  new_df <- data.frame(time = t,
                       Inc = inc,
                       Rt = Rt)

  return(rbind(sim_df, new_df))
}


# Run entire simulation
run_sim <- function(sim_df, Rt, n, b){
  t_values <- seq(1, length(Rt))

  for(i in seq_len(length(Rt))){
    sim_df <- sim_time_step(sim_df, Rt[i], n, b, t_values[i])
  }

  return(sim_df)
}



##################################################### Convolution functions ###########################################################################

# Gamma convolution function
gamma_conv <- function(tau_vals,shape, rate){

  dgamma(tau_vals, shape=shape, rate=rate)

}


# Gamma convolution function
prev_conv <- function(ct_threshold){

  ct_array<-readRDS("output/ct_array.rds")

  ct_array<-ct_array<ct_threshold
  rowMeans(ct_array)
}


prev_conv_2 <- function(){

  pos_array<-read.csv("pcr_pos_dat.csv")
  c(pos_array$y, rep(0,70))
}


prev_urt_conv <- function(){

  conv<-readRDS('output/prev_urt_conv.rds')
  conv

}


rep_test_conv <- function(ct_threshold, test_freq){
  prob_ls <- c()
  tau_vals <- seq(1,100)
  for(i in seq_len(length(tau_vals))){
    test_number <- (i-1) %/% test_freq

    prob_pos <- prev_conv(ct_threshold)[i]
    neg_multi <- 1
    if(test_number>0){
      for(j in 1:test_number){
        neg_multi <- neg_multi * (1-prev_conv(ct_threshold)[i-test_freq*j])
      }
    }

    prob_ls <- c(prob_ls, prob_pos*neg_multi)

  }

  return(prob_ls)
}


rep_test_urt_conv <- function(test_freq){
  prob_ls <- c()
  tau_vals <- seq(1,100)
  for(i in seq_len(length(tau_vals))){
    test_number <- (i-1) %/% test_freq

    prob_pos <- prev_urt_conv()[i]
    neg_multi <- 1
    if(test_number>0){
      for(j in 1:test_number){
        neg_multi <- neg_multi * (1-prev_urt_conv()[i-test_freq*j])
      }
    }

    prob_ls <- c(prob_ls, prob_pos*neg_multi)

  }

  return(prob_ls)
}



rep_test_conv_2 <- function(test_freq){
  prob_ls <- c()
  tau_vals <- seq(0,100)
  for(i in seq_len(length(tau_vals))){
    test_number <- (i-1) %/% test_freq

    prob_pos <- prev_conv_2()[i]
    neg_multi <- 1
    if(test_number>0){
      for(j in 1:test_number){
        neg_multi <- neg_multi * (1-prev_conv_2()[i-test_freq*j])
      }
    }

    prob_ls <- c(prob_ls, prob_pos*neg_multi)

  }

  return(prob_ls)
}



get_mean_sd <- function(testing){
  xfx <- seq(0,100)*testing
  mu <- sum(xfx)/sum(testing)

  xmu2fx <- (seq(0,100) - mu)**2 *testing
  var <- sum(xmu2fx)/sum(testing)
  sd <- sqrt(var)

  data.frame(mu=mu,
             sd=sd)

}



##################################################### Diagnostic functions ###########################################################################

# Calculate thc convolved time-series of incidence
estimate_Ieff <- function(res_df, conv_function, conv_param1, conv_param2=0){
  res_df$Ieff <- 0


  if(conv_function=="gamma"){
    for(i in 2:(nrow(res_df))){

      if(i+99 <= nrow(res_df)){
        res_df$Ieff[i:(i+99)] <- res_df$Ieff[i:(i+99)] + res_df$Inc[i-1] *gamma_conv(seq(1,100), shape=conv_param1, rate=conv_param2)

      } else{

        index <- nrow(res_df)-i

        res_df$Ieff[i:(i+index)] <- res_df$Ieff[i:(i+index)] + res_df$Inc[i-1] *gamma_conv(seq(1,index+1), shape=conv_param1, rate=conv_param2)

      }
    }
  } else if(conv_function=="prev1"){
    for(i in 2:(nrow(res_df))){

      if(i+99 <= nrow(res_df)){
        res_df$Ieff[i:(i+99)] <- res_df$Ieff[i:(i+99)] + res_df$Inc[i-1] *prev_conv(conv_param1)

      } else{

        index <- nrow(res_df)-i

        res_df$Ieff[i:(i+index)] <- res_df$Ieff[i:(i+index)] + res_df$Inc[i-1] *prev_conv(conv_param1)

      }
    }
  } else if(conv_function=="prev2"){
    for(i in 2:(nrow(res_df))){

      if(i+99 <= nrow(res_df)){
        res_df$Ieff[i:(i+99)] <- res_df$Ieff[i:(i+99)] + res_df$Inc[i-1] *rep_test_conv_2(conv_param1)

      } else{

        index <- nrow(res_df)-i

        res_df$Ieff[i:(i+index)] <- res_df$Ieff[i:(i+index)] + res_df$Inc[i-1] *rep_test_conv_2(conv_param1)

      }
    }
  }


  res_df$Ieff[1:99] <- res_df$Ieff[100]



  return(res_df)

}


# Calculate the deconvolved timeseries
estimate_decon <- function(res_df,conv_function = "gamma", cp1, cp2){

  res_df2 <- res_df
  res_df2$Ieff <- 0
  res_df2$Ieff[1:100] <- res_df$Ieff[101]/ sum(dgamma(seq(1,100), shape=cp1, rate=cp2))

  for(i in 101:nrow(res_df)){
    res_df2$Ieff[i] <- (res_df$Ieff[i+1] - sum(dgamma(seq(100,2), shape=cp1, rate=cp2)*res_df2$Ieff[(i-99):(i-1)]))/dgamma(1, shape=cp1, rate=cp2)
  }

  res_df2
}



# Calculate the apparent reproduction number using the convolved timseries
estimate_Rapp <- function(res_df, n, b){


  res_df$Rapp <- 1

  for(i in 100:(nrow(res_df))){

    res_df$Rapp[i] <- res_df$Ieff[i] * sum(dgamma(seq(1,100), shape=n, rate=b)) / sum(dgamma(seq(1,100), shape=n, rate=b) * rev(res_df$Ieff[(i-100):(i-1)]))

  }

  res_df$Rt[1:99] <- res_df$Rt[100]
  return(res_df)

}


################################# Diagnostics #####################################################

# Extraxt time to decrease for the step change in Rt (square Rt function)
diagnostics_square <- function(rapp_df){
  rapp_df <- rapp_df[150:200,]
  max_R <- max(rapp_df$Rt)
  min_R <- min(rapp_df$Rt)
  delta_R <- max_R - min_R


  whenT_max<-which(abs(rapp_df$Rt-max_R)<0.001)[1]
  whenT_90 <-which(rapp_df$Rapp<max_R-0.05*delta_R)[1]
  whenT_10 <-which(rapp_df$Rapp<max_R-0.95*delta_R)[1]
  whenT_min<-which(abs(rapp_df$Rapp-min_R)<0.001)[1]





  T_max_90 <- whenT_90 - whenT_max
  T_90_10  <- whenT_10 - whenT_90

  data.frame(Tmax_T95 = T_max_90,
             T95_T05  = T_90_10)

}


# Extraxt time to peak and max value in Rt (sine Rt function)
diagnostics_sine <- function(rapp_df){
  rapp_df <- rapp_df[100:200,]

  max_R <- max(rapp_df$Rt)
  min_R <- min(rapp_df$Rt)
  delta_R <- max_R - min_R



  peak_delay <- which(rapp_df$Rapp==max(rapp_df$Rapp)) - which(rapp_df$Rt==max(rapp_df$Rt))
  max_Rapp <- max(rapp_df$Rapp)

  trough_delay <- which(rapp_df$Rapp==min(rapp_df$Rapp)) - which(rapp_df$Rt==min(rapp_df$Rt))
  min_Rapp <- min(rapp_df$Rapp)


  prop_max <- 1-(max_R - max_Rapp)/delta_R

  prop_min<- 1-(max_R - min_Rapp)/delta_R



  data.frame(peak_delay = peak_delay,
             prop_max = prop_max,
             trough_delay = trough_delay,
             prop_min = prop_min)

}


