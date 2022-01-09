lppl_est <- function(data, tc, m, w, a, b, c1, c2){

  dif_time = abs(tc-data$t)
  #dif_time = tc-data$t
  est = a + dif_time^m*(b + ((c1 * cos(w * log(dif_time)))
                             + (c2 * sin(w * log(dif_time)))))

  return(est)
}
