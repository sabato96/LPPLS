lppls_conf <- function(data,clusters=9,size=1,diff=0,timeframe="weekly"){

  ticker <- data



  # conf_ind <- data.frame(P.SS_EW=rep(0,nrow(ticker)),
  #                        P.SS_EF=rep(0,nrow(ticker)),
  #                        P.S_EW=rep(0,nrow(ticker)),
  #                        P.S_EF=rep(0,nrow(ticker)),
  #                        P.M_EW=rep(0,nrow(ticker)),
  #                        P.M_EF=rep(0,nrow(ticker)),
  #                        P.L_EW=rep(0,nrow(ticker)),
  #                        P.L_EF=rep(0,nrow(ticker)),
  #                        N.SS_EW=rep(0,nrow(ticker)),
  #                        N.SS_EF=rep(0,nrow(ticker)),
  #                        N.S_EW=rep(0,nrow(ticker)),
  #                        N.S_EF=rep(0,nrow(ticker)),
  #                        N.M_EW=rep(0,nrow(ticker)),
  #                        N.M_EF=rep(0,nrow(ticker)),
  #                        N.L_EW=rep(0,nrow(ticker)),
  #                        N.L_EF=rep(0,nrow(ticker)),#19
  #
  #                        P.SS_tc=rep(0,nrow(ticker)),#20
  #                        P.S_tc=rep(0,nrow(ticker)),
  #                        P.M_tc=rep(0,nrow(ticker)),
  #                        P.L_tc=rep(0,nrow(ticker)),
  #
  #                        N.SS_tc=rep(0,nrow(ticker)),
  #                        N.S_tc=rep(0,nrow(ticker)),
  #                        N.M_tc=rep(0,nrow(ticker)),
  #                        N.L_tc=rep(0,nrow(ticker))#27
  # )
  #
  # ticker <- cbind(ticker,conf_ind)
  #

  if(timeframe == "daily"){

  cl <- parallel::makeForkCluster(clusters)
  doParallel::registerDoParallel(cl)


  #for(j in diff:(size+diff)){

    sub_ticker <- ticker[seq(nrow(ticker)-1190,nrow(ticker)),1:2]




    df_result <- foreach::foreach (i = seq(1,1190,1), .combine = rbind) %dopar% {


      r.ticker <- sub_ticker[i:nrow(sub_ticker),]


      result <- NULL
      attempt <- 3
      while(is.null(result) && attempt <= 4){

        attempt <- attempt +1
        try(
          result <- lppls_fit(r.ticker, t= "today", plot=FALSE)$results#,
          #silent=TRUE
        )

      }


      return(result)

    }


  #}




    parallel::stopCluster(cl)


  #Prendo solo lunghezza intervallo che mi interessa dt tra 1460 e 40

  df_result <- tibble::tibble(df_result) %>%

    filter(dt >= 40 & dt <= 1460)


  P.SS_EW <- nrow(as_tibble(df_result) %>%

                    filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                           & tc <= dt_filter_high & tc >= dt_filter_low
                           & oscill >= 2.5 & damp >=0.6
                           & rel_err >=0 & rel_err <=0.05
                           & dt >= 40 & dt<=183
                           # & hazard>0))
                           & B<0 & test.resid<0.463))/nrow(as_tibble(df_result) %>%

                                                             filter(dt >= 40 & dt<=183
                                                                    & B<0
                                                                    #& hazard>0
                                                             ))


  if (is.nan(P.SS_EW)) P.SS_EW=0



  N.SS_EW <- nrow(as_tibble(df_result) %>%

                    filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                           & tc <= dt_filter_high & tc >= dt_filter_low
                           & oscill >= 2.5 & damp >=0.6
                           #& rel_err >=0 & rel_err <=0.05
                           & dt >= 40 & dt<=183
                           # & hazard<0))
                           & B>0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                              filter(dt >= 40 & dt<=183
                                                                     & B>0
                                                                     #& hazard>0
                                                              ))


  if (is.nan(N.SS_EW)) N.SS_EW=0


  SS_EW <- (P.SS_EW-N.SS_EW)



  P.SS_EF <- nrow(as_tibble(df_result) %>%

                    filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                           & tc <= dt_filter_high & tc >= dt_filter_low
                           & oscill >= 2.5 & damp >=0.8
                           #& rel_err >=0 & rel_err <=0.2
                           & dt >= 40 & dt<=183
                           # & hazard>0))
                           & B<0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                              filter(dt >= 40 & dt<=183
                                                                     & B<0
                                                                     #& hazard>0
                                                              ))

  if (is.nan(P.SS_EF)) P.SS_EF=0

  N.SS_EF <- nrow(as_tibble(df_result) %>%

                    filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                           & tc <= dt_filter_high & tc >= dt_filter_low
                           & oscill >= 2.5 & damp >=0.8
                           #& rel_err >=0 & rel_err <=0.2
                           & dt >= 40 & dt<=183 & B>0
                           # & hazard<0))
                           &  test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                         filter(dt >= 40 & dt<=183
                                                                & B>0
                                                                #& hazard>0
                                                         ))

  if (is.nan(N.SS_EF)) N.SS_EF=0

  SS_EF <- (P.SS_EF-N.SS_EF)



  P.S_EW <- nrow(as_tibble(df_result) %>%

                   filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low
                          & oscill >= 2.5 & damp >=0.6
                          & rel_err >=0 & rel_err <=0.05
                          & dt >= 40 & dt<=360
                          # & hazard>0))
                          & B<0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                             filter(dt >= 40 & dt<=360
                                                                    & B<0
                                                                    #& hazard>0
                                                             ))
  if (is.nan(P.S_EW)) P.S_EW=0

  N.S_EW <- nrow(as_tibble(df_result) %>%

                   filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low
                          & oscill >= 2.5 & damp >=0.6
                          #& rel_err >=0 & rel_err <=0.05
                          & dt >= 40 & dt<=360
                          # & hazard<0))
                          & B>0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                             filter(dt >= 40 & dt<=360
                                                                    & B>0
                                                                    #& hazard>0
                                                             ))
  if (is.nan(N.S_EW)) N.S_EW=0

  S_EW <- (P.S_EW-N.S_EW)




  P.S_EF <- nrow(as_tibble(df_result) %>%

                   filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low
                          & oscill >= 2.5 & damp >=0.8
                          #& rel_err >=0 & rel_err <=0.2
                          & dt >= 40 & dt<=360
                          # & hazard>0))
                          & B<0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                             filter(dt >= 40 & dt<=360
                                                                    & B<0
                                                                    #& hazard>0
                                                             ))
  if (is.nan(P.S_EF)) P.S_EF=0


  N.S_EF <- nrow(as_tibble(df_result) %>%

                   filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low
                          & oscill >= 2.5 & damp >=0.8
                          #& rel_err >=0 & rel_err <=0.2
                          & dt >= 40 & dt<=360 & B>0
                          # & hazard<0))
                          &  test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                        filter(dt >= 40 & dt<=360
                                                               & B>0
                                                               #& hazard>0
                                                        ))

  if (is.nan(N.S_EF)) N.S_EF=0


  S_EF <- (P.S_EF-N.S_EF)



  P.M_EW <- nrow(as_tibble(df_result) %>%

                   filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low
                          & oscill >= 2.5 & damp >=0.6
                          #& rel_err >=0 & rel_err <=0.05
                          & dt >= 365 & dt<=730
                          # & hazard>0))
                          & B<0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                             filter(dt >= 365 & dt<=730
                                                                    & B<0
                                                                    #& hazard>0
                                                             ))
  if (is.nan(P.M_EW)) P.M_EW=0

  N.M_EW <- nrow(as_tibble(df_result) %>%

                   filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low
                          & oscill >= 2.5 & damp >=0.6
                          #& rel_err >=0 & rel_err <=0.05
                          & dt >= 365 & dt<=730
                          # & hazard<0))
                          & B>0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                             filter(dt >= 365 & dt<=730
                                                                    & B>0
                                                                    #& hazard>0
                                                             ))
  if (is.nan(N.M_EW)) N.M_EW=0

  M_EW <- (P.M_EW-N.M_EW)




  P.M_EF <- nrow(as_tibble(df_result) %>%

                   filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low
                          & oscill >= 2.5 & damp >=0.8
                          #& rel_err >=0 & rel_err <=0.2
                          & dt >= 365 & dt<=730
                          # & hazard>0))
                          & B<0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                             filter(dt >= 365 & dt<=730
                                                                    & B<0
                                                                    #& hazard>0
                                                             ))

  if (is.nan(P.M_EF)) P.M_EF=0

  N.M_EF <- nrow(as_tibble(df_result) %>%

                   filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low
                          & oscill >= 2.5 & damp >=0.8
                          #& rel_err >=0 & rel_err <=0.2
                          & dt >= 365 & dt<=730 & B>0
                          # & hazard<0))
                          &  test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                        filter(dt >= 365 & dt<=730
                                                               & B>0
                                                               #& hazard>0
                                                        ))

  if (is.nan(N.M_EF)) N.M_EF=0

  M_EF <- (P.M_EF-N.M_EF)




  P.L_EW <- nrow(as_tibble(df_result) %>%

                   filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low
                          & oscill >= 2.5 & damp >=0.6
                          #& rel_err >=0 & rel_err <=0.05
                          & dt >= 730 & dt<=1460
                          # & hazard>0))
                          & B<0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                             filter(dt >= 730 & dt<=1460
                                                                    & B<0
                                                                    #& hazard>0
                                                             ))


  if (is.nan(P.L_EW)) P.L_EW=0

  N.L_EW <- nrow(as_tibble(df_result) %>%

                   filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low
                          & oscill >= 2.5 & damp >=0.6
                          #& rel_err >=0 & rel_err <=0.05
                          & dt >= 730 & dt<=1460
                          # & hazard<0))
                          & B>0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                             filter(dt >= 730 & dt<=1460
                                                                    & B>0
                                                                    #& hazard>0
                                                             ))

  if (is.nan(N.L_EW)) N.L_EW=0


  L_EW <- (P.L_EW-N.L_EW)



  P.L_EF <- nrow(as_tibble(df_result) %>%

                   filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low
                          & oscill >= 2.5 & damp >=0.8
                          #& rel_err >=0 & rel_err <=0.2
                          & dt >= 730 & dt<=1460
                          # & hazard>0))
                          & B<0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                             filter(dt >= 730 & dt<=1460
                                                                    & B<0
                                                                    #& hazard>0
                                                             ))

  if (is.nan(P.L_EF)) P.L_EF=0

  N.L_EF <- nrow(as_tibble(df_result) %>%

                   filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low
                          & oscill >= 2.5 & damp >=0.8
                          #& rel_err >=0 & rel_err <=0.2
                          & dt >= 730 & dt<=1460 & B>0
                          # & hazard<0))
                          &  test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                        filter(dt >= 730 & dt<=1460
                                                               & B>0
                                                               #& hazard>0
                                                        ))

  if (is.nan(N.L_EF)) N.L_EF=0

  L_EF <- (P.L_EF-N.L_EF)


  output <- c(SS_EW,SS_EF,S_EW,S_EF,M_EW,M_EF,L_EW,L_EF)
  output <- round(output, digits=4)
  }


  if(timeframe == "weekly"){


    cl <- parallel::makeForkCluster(clusters)
    doParallel::registerDoParallel(cl)


    #for(j in diff:(size+diff)){

    sub_ticker <- ticker[seq(nrow(ticker)-220,nrow(ticker)),1:2]




    df_result <- foreach::foreach (i = seq(1,220,1), .combine = rbind) %dopar% {


      r.ticker <- sub_ticker[i:nrow(sub_ticker),]


      result <- NULL
      attempt <- 3
      while(is.null(result) && attempt <= 4){

        attempt <- attempt +1
        try(
          result <- lppls_fit(r.ticker, t= "today", plot=FALSE)$results#,
          #silent=TRUE
        )

      }


      return(result)

    }


    #}




    parallel::stopCluster(cl)


    #Prendo solo lunghezza intervallo che mi interessa dt tra 1460 e 40

    df_result <- tibble::tibble(df_result) %>%

      filter(dt >= 80 & dt <= 1460)


    P.SS_EW <- nrow(as_tibble(df_result) %>%

                      filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                             & tc <= dt_filter_high & tc >= dt_filter_low
                             & oscill >= 2.5 & damp >=0.6
                             & rel_err >=0 & rel_err <=0.05
                             & dt >= 80 & dt<=183
                             # & hazard>0))
                             & B<0 & test.resid<0.463))/nrow(as_tibble(df_result) %>%

                                                               filter(dt >= 80 & dt<=183
                                                                      & B<0
                                                                      #& hazard>0
                                                               ))


    if (is.nan(P.SS_EW)) P.SS_EW=0



    N.SS_EW <- nrow(as_tibble(df_result) %>%

                      filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                             & tc <= dt_filter_high & tc >= dt_filter_low
                             & oscill >= 2.5 & damp >=0.6
                             #& rel_err >=0 & rel_err <=0.05
                             & dt >= 80 & dt<=183
                             # & hazard<0))
                             & B>0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                                filter(dt >= 80 & dt<=183
                                                                       & B>0
                                                                       #& hazard>0
                                                                ))


    if (is.nan(N.SS_EW)) N.SS_EW=0


    SS_EW <- (P.SS_EW-N.SS_EW)



    P.SS_EF <- nrow(as_tibble(df_result) %>%

                      filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                             & tc <= dt_filter_high & tc >= dt_filter_low
                             & oscill >= 2.5 & damp >=0.8
                             #& rel_err >=0 & rel_err <=0.2
                             & dt >= 40 & dt<=183
                             # & hazard>0))
                             & B<0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                                filter(dt >= 40 & dt<=183
                                                                       & B<0
                                                                       #& hazard>0
                                                                ))

    if (is.nan(P.SS_EF)) P.SS_EF=0

    N.SS_EF <- nrow(as_tibble(df_result) %>%

                      filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                             & tc <= dt_filter_high & tc >= dt_filter_low
                             & oscill >= 2.5 & damp >=0.8
                             #& rel_err >=0 & rel_err <=0.2
                             & dt >= 40 & dt<=183 & B>0
                             # & hazard<0))
                             &  test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                           filter(dt >= 40 & dt<=183
                                                                  & B>0
                                                                  #& hazard>0
                                                           ))

    if (is.nan(N.SS_EF)) N.SS_EF=0

    SS_EF <- (P.SS_EF-N.SS_EF)



    P.S_EW <- nrow(as_tibble(df_result) %>%

                     filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                            & tc <= dt_filter_high & tc >= dt_filter_low
                            & oscill >= 2.5 & damp >=0.6
                            & rel_err >=0 & rel_err <=0.05
                            & dt >= 40 & dt<=360
                            # & hazard>0))
                            & B<0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                               filter(dt >= 40 & dt<=360
                                                                      & B<0
                                                                      #& hazard>0
                                                               ))
    if (is.nan(P.S_EW)) P.S_EW=0

    N.S_EW <- nrow(as_tibble(df_result) %>%

                     filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                            & tc <= dt_filter_high & tc >= dt_filter_low
                            & oscill >= 2.5 & damp >=0.6
                            #& rel_err >=0 & rel_err <=0.05
                            & dt >= 40 & dt<=360
                            # & hazard<0))
                            & B>0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                               filter(dt >= 40 & dt<=360
                                                                      & B>0
                                                                      #& hazard>0
                                                               ))
    if (is.nan(N.S_EW)) N.S_EW=0

    S_EW <- (P.S_EW-N.S_EW)




    P.S_EF <- nrow(as_tibble(df_result) %>%

                     filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                            & tc <= dt_filter_high & tc >= dt_filter_low
                            & oscill >= 2.5 & damp >=0.8
                            #& rel_err >=0 & rel_err <=0.2
                            & dt >= 40 & dt<=360
                            # & hazard>0))
                            & B<0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                               filter(dt >= 40 & dt<=360
                                                                      & B<0
                                                                      #& hazard>0
                                                               ))
    if (is.nan(P.S_EF)) P.S_EF=0


    N.S_EF <- nrow(as_tibble(df_result) %>%

                     filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                            & tc <= dt_filter_high & tc >= dt_filter_low
                            & oscill >= 2.5 & damp >=0.8
                            #& rel_err >=0 & rel_err <=0.2
                            & dt >= 40 & dt<=360 & B>0
                            # & hazard<0))
                            &  test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                          filter(dt >= 40 & dt<=360
                                                                 & B>0
                                                                 #& hazard>0
                                                          ))

    if (is.nan(N.S_EF)) N.S_EF=0


    S_EF <- (P.S_EF-N.S_EF)



    P.M_EW <- nrow(as_tibble(df_result) %>%

                     filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                            & tc <= dt_filter_high & tc >= dt_filter_low
                            & oscill >= 2.5 & damp >=0.6
                            #& rel_err >=0 & rel_err <=0.05
                            & dt >= 365 & dt<=730
                            # & hazard>0))
                            & B<0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                               filter(dt >= 365 & dt<=730
                                                                      & B<0
                                                                      #& hazard>0
                                                               ))
    if (is.nan(P.M_EW)) P.M_EW=0

    N.M_EW <- nrow(as_tibble(df_result) %>%

                     filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                            & tc <= dt_filter_high & tc >= dt_filter_low
                            & oscill >= 2.5 & damp >=0.6
                            #& rel_err >=0 & rel_err <=0.05
                            & dt >= 365 & dt<=730
                            # & hazard<0))
                            & B>0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                               filter(dt >= 365 & dt<=730
                                                                      & B>0
                                                                      #& hazard>0
                                                               ))
    if (is.nan(N.M_EW)) N.M_EW=0

    M_EW <- (P.M_EW-N.M_EW)




    P.M_EF <- nrow(as_tibble(df_result) %>%

                     filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                            & tc <= dt_filter_high & tc >= dt_filter_low
                            & oscill >= 2.5 & damp >=0.8
                            #& rel_err >=0 & rel_err <=0.2
                            & dt >= 365 & dt<=730
                            # & hazard>0))
                            & B<0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                               filter(dt >= 365 & dt<=730
                                                                      & B<0
                                                                      #& hazard>0
                                                               ))

    if (is.nan(P.M_EF)) P.M_EF=0

    N.M_EF <- nrow(as_tibble(df_result) %>%

                     filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                            & tc <= dt_filter_high & tc >= dt_filter_low
                            & oscill >= 2.5 & damp >=0.8
                            #& rel_err >=0 & rel_err <=0.2
                            & dt >= 365 & dt<=730 & B>0
                            # & hazard<0))
                            &  test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                          filter(dt >= 365 & dt<=730
                                                                 & B>0
                                                                 #& hazard>0
                                                          ))

    if (is.nan(N.M_EF)) N.M_EF=0

    M_EF <- (P.M_EF-N.M_EF)




    P.L_EW <- nrow(as_tibble(df_result) %>%

                     filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                            & tc <= dt_filter_high & tc >= dt_filter_low
                            & oscill >= 2.5 & damp >=0.6
                            #& rel_err >=0 & rel_err <=0.05
                            & dt >= 730 & dt<=1460
                            # & hazard>0))
                            & B<0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                               filter(dt >= 730 & dt<=1460
                                                                      & B<0
                                                                      #& hazard>0
                                                               ))


    if (is.nan(P.L_EW)) P.L_EW=0

    N.L_EW <- nrow(as_tibble(df_result) %>%

                     filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                            & tc <= dt_filter_high & tc >= dt_filter_low
                            & oscill >= 2.5 & damp >=0.6
                            #& rel_err >=0 & rel_err <=0.05
                            & dt >= 730 & dt<=1460
                            # & hazard<0))
                            & B>0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                               filter(dt >= 730 & dt<=1460
                                                                      & B>0
                                                                      #& hazard>0
                                                               ))

    if (is.nan(N.L_EW)) N.L_EW=0


    L_EW <- (P.L_EW-N.L_EW)



    P.L_EF <- nrow(as_tibble(df_result) %>%

                     filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                            & tc <= dt_filter_high & tc >= dt_filter_low
                            & oscill >= 2.5 & damp >=0.8
                            #& rel_err >=0 & rel_err <=0.2
                            & dt >= 730 & dt<=1460
                            # & hazard>0))
                            & B<0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                               filter(dt >= 730 & dt<=1460
                                                                      & B<0
                                                                      #& hazard>0
                                                               ))

    if (is.nan(P.L_EF)) P.L_EF=0

    N.L_EF <- nrow(as_tibble(df_result) %>%

                     filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                            & tc <= dt_filter_high & tc >= dt_filter_low
                            & oscill >= 2.5 & damp >=0.8
                            #& rel_err >=0 & rel_err <=0.2
                            & dt >= 730 & dt<=1460 & B>0
                            # & hazard<0))
                            &  test.resid<0.463 ))/nrow(as_tibble(df_result) %>%

                                                          filter(dt >= 730 & dt<=1460
                                                                 & B>0
                                                                 #& hazard>0
                                                          ))

    if (is.nan(N.L_EF)) N.L_EF=0

    L_EF <- (P.L_EF-N.L_EF)


    output <- c(SS_EW,SS_EF,S_EW,S_EF,M_EW,M_EF,L_EW,L_EF)

    output <- round(output, digits=4)
  }
  return(output)
}


