#' Estimates the LPPLS parameters (single time window)
#'
#' @description Estimates the LPPLS parameters (single time window)
#' @usage lppls_fit(data)
#'
#' @param data Time series dataset: First column (class Date) in format "%Y-%m-%d" and second column is the asset price
#' @param t1 Beginning of time windows upon which estimate the parameters
#' @param t2 End of time windows upon which estimate the parameters (If t2 = "today" the last day in the time series will be used)
#' @param type Optimization methods ("L-BFGS-B", "CMAES", "contr", "isres", "mlsl", "nelder")
#' @param plot Generate a plot if TRUE
#'
#' @return List containing parameters and other data
#' @export
#'
#' @examples lppls_fit(data)
lppls_fit <- function(data,t1="2018-10-09",t2="2019-10-09",t=NULL,type="mlsl",plot=TRUE){

  #Data wrangling

  #t1="2018-10-09"
  #t2="2019-10-09"

  names(data) <- c("Date", "Close")
  data$Date <- as.Date(data$Date)
  if(t == "today"){
    t2 <- lubridate::today()
    t1 <- data$Date[1]
    ticker <- data %>% dplyr::filter(Date >= t1 & Date <= t2)

    } else {

  ticker <- data %>% dplyr::filter(Date >= t1 & Date <= t2)

  }

  names(ticker) <- c("Date", "Close")

  ticker$t <- lubridate::decimal_date(ticker$Date)
  data$t <- lubridate::decimal_date(data$Date)
  ticker <- stats::na.omit(ticker)
  data <- stats::na.omit(data)
  ticker$Close <- as.numeric(ticker$Close)
  data$Close <- as.numeric(data$Close)

  last_row <- utils::tail(ticker, 1)
  first_row <- utils::head(ticker, 1)
  dt <- last_row$t -first_row$t

  start_search <- c(runif(1,max(ticker$t)-0.2*dt,
                          max(ticker$t)+0.2*dt),
                    runif(1,0.01,1.99),
                    runif(1,1,50))

  upper <- c(max(ticker$t)+0.2*dt,2,50)
  lower <- c(max(ticker$t)-0.2*dt,0.01,1)




if(type=="L-BFGS-B"){

  test <- stats::optim(start_search,funz_obj,lower=lower,upper=upper,method="L-BFGS-B",data=ticker)

}

if(type=="CMAES"){

  nbre_generation <- 100

  vec_control <- data.frame(maxit = c(nbre_generation))

  test <- cmaes::cma_es(start_search, funz_obj, ticker,
                        lower=c(max(ticker$t)-0.2*dt, 0.01, 1), upper=c(max(ticker$t)+0.2*dt, 1, 50), control=vec_control)

}


if(type=="contr"){

  test <- nloptr::crs2lm(start_search, funz_obj, lower=lower, upper=upper, data=ticker)


}

if(type=="isres"){

  test <- nloptr::isres(start_search,funz_obj,lower=lower,upper=upper,data=ticker)

}

if(type=="mlsl"){

  test <- nloptr::mlsl(start_search,funz_obj,lower=lower,upper=upper,local.method = "LBFGS",data=ticker)

  }

  if(type=="nelder"){

    test <- nloptr::neldermead(start_search,funz_obj,lower=lower,upper=upper, data=ticker)

  }


  linear_param <- matrix_eq(ticker,test$par[1], test$par[2], test$par[3])

  fitted <- lppl_est(ticker,test$par[1], test$par[2], test$par[3],
                     linear_param[1],linear_param[2],linear_param[3],linear_param[4])



  residual <- log(ticker$Close)-fitted

  test.resid <- suppressWarnings(tseries::kpss.test(residual)[1] )#Test stazionarieta' residui

  rownames(test.resid) <- c()

  #253 trading days in a year


  results <- data.frame(first_row$Date,
                        last_row$Date,
                        last_row$Close,
                        as.integer(dt/(1/253)),
                        exp(max(fitted)),
                        test$par[1]-last_row$t,
                        as.integer((test$par[1]-last_row$t)/(1/253)),
                        test$par[2], #m
                        test$par[3], #w
                        test$par[1], #tc
                        linear_param[1], #A
                        linear_param[2], #B
                        linear_param[3], #C1
                        linear_param[4], #C2
                        (test$par[3]/(2*pi))*log(abs((test$par[1])/(test$par[1]-last_row$t))),
                        #(test$par[3]/(2))*log(abs((test$par[1]-first_row$t)/(last_row$t-first_row$t))),#number oscillation
                        (test$par[2]*abs(linear_param[2]))/(test$par[3]
                                                            *abs((linear_param[3]^2+linear_param[4]^2)^0.5)),
                        #*abs(linear_param[3]/(cos(atan(linear_param[4]/linear_param[3]))))

                        #abs((log(last_row$Close)-fitted[length(fitted)])/fitted[length(fitted)]),#relative error sbagliato
                        mean(abs(residual)/fitted, na.rm =TRUE), #relative error corretto
                        last_row$t-0.05*dt,
                        last_row$t+0.1*dt,
                        - linear_param[2] * test$par[2] - abs((linear_param[3]^2+linear_param[4]^2)^0.5)* sqrt(test$par[2]^2+test$par[3]^2),#fantazzini
                        test.resid,
                        sum(residual^2)
  )

  names(results) <- c("start_date","end_date","last_price","dt","LPPL_max","tc-end.t",
                      "day_to_tc","m","w","tc","A","B","C1","C2","oscill","damp","rel_err","dt_filter_low","dt_filter_high","hazard","test.resid","resid")


  rownames(results) <- c()


  output <- list("results" = results,
                 "fitted" = fitted,
                 "residuals" = residual)

  if(plot==TRUE){


    base_data <- data %>% dplyr::filter(t >= lubridate::decimal_date(as.Date(t1)) &
                                 t <= lubridate::decimal_date(as.Date(t2))+0.2)


    lppl_est_plot <- function(data, tc, m, w, a, b, c1, c2, base_data){

      tempo <- c(base_data$t,seq(max(base_data$t),max(base_data$t)+0.2, by=0.002))
      dif_time = abs(tc-tempo)
      est = a + dif_time^m*(b + ((c1 * cos(w * log(dif_time)))
                                 + (c2 * sin(w * log(dif_time)))))

      est <- cbind(tempo,est)
      est <- as.data.frame(est)
      names(est) <- c("time","fitted")
      return(est)
    }

    fitted_pl <- lppl_est_plot(ticker,output$results$tc, output$results$m, output$results$w,
                            output$results$A,output$results$B,output$results$C1,
                            output$results$C2, base_data)





    base_data$Close <- log(base_data$Close)



   a <- ggplot(NULL) +
      geom_rect(aes(xmin=lubridate::decimal_date(output$results$start_date), xmax=lubridate::decimal_date(output$results$end_date),
                    ymin= -Inf, ymax=+ Inf), fill="gray", alpha=0.5) +
      geom_line(data= fitted_pl, aes(y=fitted, x=time), color = "red") +
      geom_line(data=base_data, aes(y=Close, x=t)) +


      #geom_vline(aes(xintercept=res$tc), color="blue") +
      #geom_vline(aes(xintercept=decimal_date(res$start_date)), color="green") +
      #geom_vline(aes(xintercept=decimal_date(res$end_date)), color="green") +
      theme_light() +
      theme(text = element_text(size=20),
            axis.text = element_text(colour = "black"))+
      labs(x="Time",y="Price (log)")


   plot(a)
    }




  invisible(output)

}
