
matrix_eq <- function(data, tc, m, w){

  ti <- abs(tc - data$t)
  #ti <- tc - data$t
  fi <- ti ** m #B
  gi <- ti ** m * cos(w * log(ti)) #C1
  hi <- ti ** m * sin(w * log(ti)) #C2
  yi <- log(data$Close)

  MAT <- matrix(c(length(ti),sum(fi),sum(gi),sum(hi),
                  sum(fi),sum(fi**2),sum(fi*gi),sum(fi*hi),
                  sum(gi),sum(fi*gi),sum(gi**2),sum(gi*hi),
                  sum(hi),sum(fi*hi),sum(gi*hi),sum(hi**2)),ncol=4,nrow=4)

  YY <- matrix(c(sum(yi),sum(yi*fi),sum(yi*gi),sum(yi*hi)),ncol=1)

  coef <- solve(MAT,YY)

  #reg <- coef(lm(logP ~ Xm + Xm.cos + Xm.sin, data=data))

  return(coef)
}
