
evalstats <- function(obs, pred, is_outlier, is_outrange, remove_outlier = FALSE, remove_outrange = FALSE) {
  sel_complete = !(is.na(obs) | is.na(pred))
  if(remove_outlier) {
    sel_complete = sel_complete & (!is_outlier)
  }
  if(remove_outrange) {
    sel_complete = sel_complete & (!is_outrange)
  }
  n<- sum(sel_complete)

  n_obs<- sum(!is.na(obs))
  n_pred<- sum(!is.na(pred))
  
  if(n>0) {
    obs <- obs[sel_complete]
    pred <- pred[sel_complete]
    E <- pred - obs
    Bias <- mean(E)
    Bias.rel <- 100 * Bias / abs(mean(obs))
    MAE <- mean(abs(E)) #Mean absolute error
    MAE.rel <- 100 * MAE / abs(mean(obs))
    m <- lm(obs ~ pred)
    b <- as.numeric(m$coefficients)[2]
    r2<- cor(pred, obs)^2
    NSE <- 1 - (sum((obs - pred) ^ 2) / sum((obs - mean(obs)) ^ 2)) #Nash–Sutcliffe model efficiency coefficient (NSE)
    
  } else {
    Bias <- NA
    Bias.rel <- NA
    MAE <- NA
    MAE.rel <- NA
    b <- NA
    r2 <- NA
    NSE <- NA
  }
  return(
    list(
      n_obs = n_obs,
      n_pred = n_pred,
      n = n,
      Bias = Bias,
      Bias.rel = Bias.rel,
      MAE = MAE,
      MAE.rel = MAE.rel,
      b = b,
      r2 = r2,
      NSE = NSE
    )
  )
}
