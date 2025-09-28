
evalstats <- function(obs, pred, is_outlier, remove_outlier = TRUE) {
  sel_complete = !(is.na(obs) | is.na(pred))
  if(remove_outlier) {
    sel_complete = sel_complete & (!is_outlier)
  }
  n<- sum(sel_complete)
  
  n_obs<- sum(!is.na(obs))
  n_pred<- sum(!is.na(pred))
  
  obs <- obs[sel_complete]
  pred <- pred[sel_complete]
  E <- pred - obs
  Bias <- mean(E)
  Bias.rel <- 100 * Bias / abs(mean(obs))
  MAE <- mean(abs(E)) #Mean absolute error
  RMSE <- sqrt(mean(E^2)) #Root mean squared error
  MAE.rel <- 100 * MAE / abs(mean(obs))
  r <- cor(obs, pred)
  r2<- r^2
  NSE <- 1 - (sum((obs - pred) ^ 2) / sum((obs - mean(obs)) ^ 2)) #Nash–Sutcliffe model efficiency coefficient (NSE)
  #NSE.abs <- 1 - (sum(abs(obs - pred)) / sum(abs(obs - mean(obs))))
  return(
    list(
      n_obs = n_obs,
      n_pred = n_pred,
      n = n,
      Bias = Bias,
      Bias.rel = Bias.rel,
      MAE = MAE,
      RMSE = RMSE,
      MAE.rel = MAE.rel,
      #r = r,
      r2 = r2,
      NSE = NSE
      #NSE.abs = NSE.abs
    )
  )
}
