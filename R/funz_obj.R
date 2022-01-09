
funz_obj <- function(x,data){

  tc = x[1]
  m = x[2]
  w = x[3]

  lin_par <- matrix_eq(data,tc,m,w)

  #c = (lin_par[3] ** 2 + lin_par[4] ** 2) ** 0.5


  # Residual sum of squares

  delta <- log(data$Close)-lppl_est(data,tc, m, w, lin_par[1], lin_par[2],
                                    lin_par[3], lin_par[4])



  RSS <- sum(delta^2)

  return(RSS)

}
