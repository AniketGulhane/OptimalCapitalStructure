
OCSWRC <- function(...){
  US <- data.frame(...)


  US$SP <- 0
  US$SPV <- 0

  for(i in 1 :nrow(US))
  {for(j in 1 :nrow(Bond))
  { if(US$ICR[i] >= Bond$ICR[j] & US$ICR[i] < Bond$max[j])
  { US$SP[i] <- j}}}

  for(i in 1 :nrow(US))
    for(j in 1 :nrow(Bond))
      if(US$ICR[i] >= Bond$ICR[j] && US$ICR[i] < Bond$max[j])
      {US$SPV[i] <- Bond$ICR[j]}


  US$dtt <- as.numeric(US$LTD)/(as.numeric(US$LTD)+as.numeric(US$MarketCap))
  US$Rintr <-  as.numeric(Bond$Spread[US$SP])+USRF
  US$AftertaxCOD <-  as.numeric(US$Rintr)*(1-US$TaxR)
  US$COE <- as.numeric(USRF)+(as.numeric(US$Beta)*as.numeric(USRP))
  US$WACC <- ((as.numeric(US$COE)*(as.numeric(1)-as.numeric(US$dtt)))+(as.numeric(US$COD)*as.numeric(US$dtt)))
  US$EV <-(as.numeric(US$MarketCap)+as.numeric(US$LTD))-(as.numeric(US$Cash))
  US$PretaxCOD <- as.numeric(US$COD)/(1-as.numeric(US$TaxR))
  #EU$COE <- as.numeric(EURF)+as.numeric(EU$Beta)*as.numeric(EURP)

  # Debt 0%
  US$DbyTZero <- 0
  US$DbyEZero <- 0
  US$DebtZero <- 0


  US$PretaxCODZero <- (as.numeric(Bond$Spread[US$SP]) + USRF)
  US$InterestZero <- US$DebtZero*US$PretaxCODZero
  US$PreTaxIntCovZero <- US$EBIT/US$InterestZero
  US$TaxableIncomeZero <- as.numeric(US$EBIT)-US$InterestZero
  US$PretaxCODZero <- as.numeric(Bond$Spread[US$SP]) + USRF
  US$CODZero <- as.numeric(US$PretaxCODZero)*(1-as.numeric(US$TaxR))
  US$BetaZero <- as.numeric(US$Beta)/(1+((1-as.numeric(US$TaxR))*(as.numeric(US$LTD)/as.numeric(US$MarketCap))))*((1+(1-as.numeric(US$TaxR))*as.numeric(US$DbyEZero)))
  US$COEZero <- (as.numeric(US$BetaZero)*as.numeric(USRP))+as.numeric(USRF)
  US$WACCZero <- (as.numeric(US$CODZero)*as.numeric(US$DbyTZero))+(as.numeric(US$COEZero)*(1-as.numeric(US$DbyTZero)))
  US$InterestZero <- as.numeric(as.numeric(Bond$Spread[US$SP])+USRF)/(1-US$TaxR)*US$DebtZero
  US$TaxZero <- US$TaxableIncomeZero*US$TaxR
  US$PreTaxIntCovZero <- US$EBIT/US$InterestZero
  US$TaxRABZero <- ifelse(US$InterestZero<US$EBIT, US$TaxR,as.numeric(US$TaxR)*as.numeric(US$EBIT)/as.numeric(US$InterestZero))
  US$NetIncomeZero <-US$TaxableIncomeZero-US$TaxZero
  US$NetOpIncomeZero <-as.numeric(US$NetIncomeZero)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovZero <- US$EBIT/US$InterestZero
  US$ValueZero <- US$EV*(1+((US$WACC-US$WACCZero)/(US$WACCZero-USRF)))

  as.numeric(US$BetaZero)-as.numeric(US$Beta)

  # Debt 10%
  US$DbyTTen <- 0.10
  US$DbyETen <- 0.1111
  US$DebtTen <- US$DbyTTen*(as.numeric(US$MarketCap)+as.numeric(US$LTD))



  US$InterestTen <- as.numeric(as.numeric(Bond$Spread[US$SP])+USRF)/(1-US$TaxR)*US$DebtTen
  US$TaxableIncomeTen <- as.numeric(US$EBIT)-US$InterestTen
  US$PretaxCODTen <- as.numeric(Bond$Spread[US$SP]) + USRF
  US$CODTen <- as.numeric(US$PretaxCODTen)*(1-as.numeric(US$TaxR))
  US$TRTen <- ifelse(US$InterestTen<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestTen )
  US$BetaTen <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRTen))*US$DbyETen)))
  US$COETen <- (as.numeric(US$BetaTen)*as.numeric(USRP))+as.numeric(USRF)
  US$WACCTen <- (as.numeric(US$CODTen)*as.numeric(US$DbyTTen))+(as.numeric(US$COETen)*(1-as.numeric(US$DbyTTen)))

  US$TaxTen <- US$TaxableIncomeTen*US$TaxR
  US$PreTaxIntCovTen <- US$EBIT/US$InterestTen
  US$TaxRABTen <- ifelse(US$InterestTen<US$EBIT, US$TaxR,as.numeric(US$TaxR)*as.numeric(US$EBIT)/as.numeric(US$InterestTen))
  US$NetIncomeTen <-US$TaxableIncomeTen-US$TaxTen
  US$NetOpIncomeTen <-as.numeric(US$NetIncomeTen)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovTen <- US$EBIT/US$InterestTen
  US$ValueTen <- US$EV*(1+((US$WACC-US$WACCTen)/(US$WACCTen-USRF)))

  # Debt 20%

  US$DbyTTwty <- 0.20
  US$DbyETwty <- 0.25
  US$DebtTwty <- US$DbyTTwty*(as.numeric(US$MarketCap)+as.numeric(US$LTD))


  US$InterestTwty <- as.numeric(Bond$Spread[US$SP]+USRF)/(1-US$TaxR)*US$DebtTwty
  US$TaxableIncomeTwty <- as.numeric(US$EBIT)-US$InterestTwty
  US$TaxTwty <- US$TaxableIncomeTwty*US$TaxR
  US$NetIncomeTwty <-US$TaxableIncomeTwty-US$TaxTwty
  US$NetOpIncomeTwty <-as.numeric(US$NetIncomeTwty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovTwty <- US$EBIT/US$InterestTwty
  US$TRTwty <- ifelse(US$InterestTwty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestTwty )
  US$BetaTwty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRTwty))*US$DbyETwty)))
  US$COETwty <- (as.numeric(US$BetaTwty)*as.numeric(USRP))+as.numeric(USRF)




  US$TaxableIncomeTwty <- as.numeric(US$EBIT)-US$InterestTwty
  US$PretaxCODTwty <- as.numeric(Bond$Spread[US$SP]) + USRF
  US$CODTwty <- as.numeric(US$PretaxCODTwty)*(1-as.numeric(US$TaxR))
  US$TRTwty <- ifelse(US$InterestTwty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestTwty )
  US$BetaTwty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRTwty))*US$DbyETwty)))
  US$COETwty <- (as.numeric(US$BetaTwty)*as.numeric(USRP))+as.numeric(USRF)
  US$WACCTwty <- (as.numeric(US$CODTwty)*as.numeric(US$DbyTTwty))+(as.numeric(US$COETwty)*(1-as.numeric(US$DbyTTwty)))
  US$InterestTwty <- as.numeric(as.numeric(Bond$Spread[US$SP])+USRF)/(1-US$TaxR)*US$DebtTwty
  US$TaxTwty <- US$TaxableIncomeTwty*US$TaxR
  US$PreTaxIntCovTwty <- US$EBIT/US$InterestTwty
  US$TaxRABTwty <- ifelse(US$InterestTwty<US$EBIT, US$TaxR,as.numeric(US$TaxR)*as.numeric(US$EBIT)/as.numeric(US$InterestTwty))
  US$NetIncomeTwty <-US$TaxableIncomeTwty-US$TaxTwty
  US$NetOpIncomeTwty <-as.numeric(US$NetIncomeTwty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovTwty <- US$EBIT/US$InterestTwty
  US$ValueTwty <- US$EV*(1+((US$WACC-US$WACCTwty)/(US$WACCTwty-USRF)))


  # Debt 30%

  US$DbyTTrty <- 0.30
  US$DbyETrty <- 0.4286
  US$DebtTrty <- US$DbyTTrty*(as.numeric(US$MarketCap)+as.numeric(US$LTD))


  US$InterestTrty <- as.numeric(Bond$Spread[US$SP]+USRF)/(1-US$TaxR)*US$DebtTrty
  US$TaxableIncomeTrty <- as.numeric(US$EBIT)-US$InterestTrty
  US$TaxTrty <- US$TaxableIncomeTrty*US$TaxR
  US$NetIncomeTrty <-US$TaxableIncomeTrty-US$TaxTrty
  US$NetOpIncomeTrty <-as.numeric(US$NetIncomeTrty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovTrty <- US$EBIT/US$InterestTrty
  US$TRTrty <- ifelse(US$InterestTrty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestTrty )
  US$BetaTrty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRTrty))*US$DbyETrty)))
  US$COETrty <- (as.numeric(US$BetaTrty)*as.numeric(USRP))+as.numeric(USRF)



  US$TaxableIncomeTrty <- as.numeric(US$EBIT)-US$InterestTrty
  US$PretaxCODTrty <- as.numeric(Bond$Spread[US$SP]) + USRF
  US$CODTrty <- as.numeric(US$PretaxCODTrty)*(1-as.numeric(US$TaxR))
  US$TRTrty <- ifelse(US$InterestTrty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestTrty )
  US$BetaTrty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRTrty))*US$DbyETrty)))
  US$COETrty <- (as.numeric(US$BetaTrty)*as.numeric(USRP))+as.numeric(USRF)
  US$WACCTrty <- (as.numeric(US$CODTrty)*as.numeric(US$DbyTTrty))+(as.numeric(US$COETrty)*(1-as.numeric(US$DbyTTrty)))
  US$InterestTrty <- as.numeric(as.numeric(Bond$Spread[US$SP])+USRF)/(1-US$TaxR)*US$DebtTrty
  US$TaxTrty <- US$TaxableIncomeTrty*US$TaxR
  US$PreTaxIntCovTrty <- US$EBIT/US$InterestTrty
  US$TaxRABTrty <- ifelse(US$InterestTrty<US$EBIT, US$TaxR,as.numeric(US$TaxR)*as.numeric(US$EBIT)/as.numeric(US$InterestTrty))
  US$NetIncomeTrty <-US$TaxableIncomeTrty-US$TaxTrty
  US$NetOpIncomeTrty <-as.numeric(US$NetIncomeTrty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovTrty <- US$EBIT/US$InterestTrty
  US$ValueTrty <- US$EV*(1+((US$WACC-US$WACCTrty)/(US$WACCTrty-USRF)))


  # Debt 40%

  US$DbyTFrty <- 0.40
  US$DbyEFrty <- 0.6667
  US$DebtFrty <- US$DbyTFrty*(as.numeric(US$MarketCap)+as.numeric(US$LTD))
  US$InterestFrty <- as.numeric(Bond$Spread[US$SP]+USRF)/(1-US$TaxR)*US$DebtFrty
  US$TaxableIncomeFrty <- as.numeric(US$EBIT)-US$InterestFrty
  US$TaxFrty <- US$TaxableIncomeFrty*US$TaxR
  US$NetIncomeFrty <-US$TaxableIncomeFrty-US$TaxFrty
  US$NetOpIncomeFrty <-as.numeric(US$NetIncomeFrty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovFrty <- US$EBIT/US$InterestFrty
  US$TRFrty <- ifelse(US$InterestFrty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestFrty )
  US$BetaFrty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRFrty))*US$DbyEFrty)))
  US$COEFrty <- (as.numeric(US$BetaFrty)*as.numeric(USRP))+as.numeric(USRF)




  US$TaxableIncomeFrty <- as.numeric(US$EBIT)-US$InterestFrty
  US$PretaxCODFrty <- as.numeric(Bond$Spread[US$SP]) + USRF
  US$CODFrty <- as.numeric(US$PretaxCODFrty)*(1-as.numeric(US$TaxR))
  US$TRFrty <- ifelse(US$InterestFrty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestFrty )
  US$BetaFrty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRFrty))*US$DbyEFrty)))
  US$COEFrty <- (as.numeric(US$BetaFrty)*as.numeric(USRP))+as.numeric(USRF)
  US$WACCFrty <- (as.numeric(US$CODFrty)*as.numeric(US$DbyTFrty))+(as.numeric(US$COEFrty)*(1-as.numeric(US$DbyTFrty)))
  US$InterestFrty <- as.numeric(as.numeric(Bond$Spread[US$SP])+USRF)/(1-US$TaxR)*US$DebtFrty
  US$TaxFrty <- US$TaxableIncomeFrty*US$TaxR
  US$PreTaxIntCovFrty <- US$EBIT/US$InterestFrty
  US$TaxRABFrty <- ifelse(US$InterestFrty<US$EBIT, US$TaxR,as.numeric(US$TaxR)*as.numeric(US$EBIT)/as.numeric(US$InterestFrty))
  US$NetIncomeFrty <-US$TaxableIncomeFrty-US$TaxFrty
  US$NetOpIncomeFrty <-as.numeric(US$NetIncomeFrty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovFrty <- US$EBIT/US$InterestFrty
  US$ValueFrty <- US$EV*(1+((US$WACC-US$WACCFrty)/(US$WACCFrty-USRF)))



  # Debt 50%

  US$DbyTFifty <- 0.50
  US$DbyEFifty <- 1


  US$DebtFifty <- US$DbyTFifty*(as.numeric(US$MarketCap)+as.numeric(US$LTD))
  US$InterestFifty <- as.numeric(Bond$Spread[US$SP]+USRF)/(1-US$TaxR)*US$DebtFifty
  US$TaxableIncomeFifty <- as.numeric(US$EBIT)-US$InterestFifty
  US$TaxFifty <- US$TaxableIncomeFifty*US$TaxR
  US$NetIncomeFifty <-US$TaxableIncomeFifty-US$TaxFifty
  US$NetOpIncomeFifty <-as.numeric(US$NetIncomeFifty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovFifty <- US$EBIT/US$InterestFifty
  US$TRFifty <- ifelse(US$InterestFifty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestFifty )
  US$BetaFifty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRFifty))*US$DbyEFifty)))
  US$COEFifty <- (as.numeric(US$BetaFifty)*as.numeric(USRP))+as.numeric(USRF)



  US$TaxableIncomeFifty <- as.numeric(US$EBIT)-US$InterestFifty
  US$PretaxCODFifty <- as.numeric(Bond$Spread[US$SP]) + USRF
  US$CODFifty <- as.numeric(US$PretaxCODFifty)*(1-as.numeric(US$TaxR))
  US$TRFifty <- ifelse(US$InterestFifty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestFifty )
  US$BetaFifty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRFifty))*US$DbyEFifty)))
  US$COEFifty <- (as.numeric(US$BetaFifty)*as.numeric(USRP))+as.numeric(USRF)
  US$WACCFifty <- (as.numeric(US$CODFifty)*as.numeric(US$DbyTFifty))+(as.numeric(US$COEFifty)*(1-as.numeric(US$DbyTFifty)))
  US$InterestFifty <- as.numeric(as.numeric(Bond$Spread[US$SP])+USRF)/(1-US$TaxR)*US$DebtFifty
  US$TaxFifty <- US$TaxableIncomeFifty*US$TaxR
  US$PreTaxIntCovFifty <- US$EBIT/US$InterestFifty
  US$TaxRABFifty <- ifelse(US$InterestFifty<US$EBIT, US$TaxR,as.numeric(US$TaxR)*as.numeric(US$EBIT)/as.numeric(US$InterestFifty))
  US$NetIncomeFifty <-US$TaxableIncomeFifty-US$TaxFifty
  US$NetOpIncomeFifty <-as.numeric(US$NetIncomeFifty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovFifty <- US$EBIT/US$InterestFifty
  US$ValueFifty <- US$EV*(1+((US$WACC-US$WACCFifty)/(US$WACCFifty-USRF)))


  # Debt 60%

  US$DbyTSxty <- 0.60
  US$DbyESxty <- 1.5
  US$DebtSxty <- US$DbyTSxty*(as.numeric(US$MarketCap)+as.numeric(US$LTD))
  US$InterestSxty <- as.numeric(Bond$Spread[US$SP]+USRF)/(1-US$TaxR)*US$DebtSxty
  US$TaxableIncomeSxty <- as.numeric(US$EBIT)-US$InterestSxty
  US$TaxSxty <- US$TaxableIncomeSxty*US$TaxR
  US$NetIncomeSxty <-US$TaxableIncomeSxty-US$TaxSxty
  US$NetOpIncomeSxty <-as.numeric(US$NetIncomeSxty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovSxty <- US$EBIT/US$InterestSxty
  US$TRSxty <- ifelse(US$InterestSxty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestSxty )
  US$BetaSxty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRSxty))*US$DbyESxty)))
  US$COESxty <- (as.numeric(US$BetaSxty)*as.numeric(USRP))+as.numeric(USRF)


  US$TaxableIncomeSxty <- as.numeric(US$EBIT)-US$InterestSxty
  US$PretaxCODSxty <- as.numeric(Bond$Spread[US$SP]) + USRF
  US$CODSxty <- as.numeric(US$PretaxCODSxty)*(1-as.numeric(US$TaxR))
  US$TRSxty <- ifelse(US$InterestSxty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestSxty )
  US$BetaSxty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRSxty))*US$DbyESxty)))
  US$COESxty <- (as.numeric(US$BetaSxty)*as.numeric(USRP))+as.numeric(USRF)
  US$WACCSxty <- (as.numeric(US$CODSxty)*as.numeric(US$DbyTSxty))+(as.numeric(US$COESxty)*(1-as.numeric(US$DbyTSxty)))
  US$InterestSxty <- as.numeric(as.numeric(Bond$Spread[US$SP])+USRF)/(1-US$TaxR)*US$DebtSxty
  US$TaxSxty <- US$TaxableIncomeSxty*US$TaxR
  US$PreTaxIntCovSxty <- US$EBIT/US$InterestSxty
  US$TaxRABSxty <- ifelse(US$InterestSxty<US$EBIT, US$TaxR,as.numeric(US$TaxR)*as.numeric(US$EBIT)/as.numeric(US$InterestSxty))
  US$NetIncomeSxty <-US$TaxableIncomeSxty-US$TaxSxty
  US$NetOpIncomeSxty <-as.numeric(US$NetIncomeSxty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovSxty <- US$EBIT/US$InterestSxty
  US$ValueSxty <- US$EV*(1+((US$WACC-US$WACCSxty)/(US$WACCSxty-USRF)))


  # Debt 70%

  US$DbyTSvnty <- 0.70
  US$DbyESvnty <- 2.3333
  US$DebtSvnty <- US$DbyTSvnty*(as.numeric(US$MarketCap)+as.numeric(US$LTD))
  US$InterestSvnty <- as.numeric(Bond$Spread[US$SP]+USRF)/(1-US$TaxR)*US$DebtSvnty
  US$TaxableIncomeSvnty <- as.numeric(US$EBIT)-US$InterestSvnty
  US$TaxSvnty <- US$TaxableIncomeSvnty*US$TaxR
  US$NetIncomeSvnty <-US$TaxableIncomeSvnty-US$TaxSvnty
  US$NetOpIncomeSvnty <-as.numeric(US$NetIncomeSvnty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovSvnty <- US$EBIT/US$InterestSvnty
  US$TRSvnty <- ifelse(US$InterestSvnty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestSvnty )
  US$BetaSvnty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRSvnty))*US$DbyESvnty)))
  US$COESvnty <- (as.numeric(US$BetaSvnty)*as.numeric(USRP))+as.numeric(USRF)


  US$TaxableIncomeSvnty <- as.numeric(US$EBIT)-US$InterestSvnty
  US$PretaxCODSvnty <- as.numeric(Bond$Spread[US$SP]) + USRF
  US$CODSvnty <- as.numeric(US$PretaxCODSvnty)*(1-as.numeric(US$TaxR))
  US$TRSvnty <- ifelse(US$InterestSvnty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestSvnty )
  US$BetaSvnty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRSvnty))*US$DbyESvnty)))
  US$COESvnty <- (as.numeric(US$BetaSvnty)*as.numeric(USRP))+as.numeric(USRF)
  US$WACCSvnty <- (as.numeric(US$CODSvnty)*as.numeric(US$DbyTSvnty))+(as.numeric(US$COESvnty)*(1-as.numeric(US$DbyTSvnty)))
  US$InterestSvnty <- as.numeric(as.numeric(Bond$Spread[US$SP])+USRF)/(1-US$TaxR)*US$DebtSvnty
  US$TaxSvnty <- US$TaxableIncomeSvnty*US$TaxR
  US$PreTaxIntCovSvnty <- US$EBIT/US$InterestSvnty
  US$TaxRABSvnty <- ifelse(US$InterestSvnty<US$EBIT, US$TaxR,as.numeric(US$TaxR)*as.numeric(US$EBIT)/as.numeric(US$InterestSvnty))
  US$NetIncomeSvnty <-US$TaxableIncomeSvnty-US$TaxSvnty
  US$NetOpIncomeSvnty <-as.numeric(US$NetIncomeSvnty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovSvnty <- US$EBIT/US$InterestSvnty
  US$ValueSvnty <- US$EV*(1+((US$WACC-US$WACCSvnty)/(US$WACCSvnty-USRF)))


  # Debt 80%

  US$DbyTEgty <- 0.80
  US$DbyEEgty <- 4
  US$DebtEgty <- US$DbyTEgty*(as.numeric(US$MarketCap)+as.numeric(US$LTD))
  US$InterestEgty <- as.numeric(Bond$Spread[US$SP]+USRF)/(1-US$TaxR)*US$DebtEgty
  US$TaxableIncomeEgty <- as.numeric(US$EBIT)-US$InterestEgty
  US$TaxEgty <- US$TaxableIncomeEgty*US$TaxR
  US$NetIncomeEgty <-US$TaxableIncomeEgty-US$TaxEgty
  US$NetOpIncomeEgty <-as.numeric(US$NetIncomeEgty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovEgty <- US$EBIT/US$InterestEgty
  US$TREgty <- ifelse(US$InterestEgty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestEgty )
  US$BetaEgty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TREgty))*US$DbyEEgty)))
  US$COEEgty <- (as.numeric(US$BetaEgty)*as.numeric(USRP))+as.numeric(USRF)



  US$TaxableIncomeEgty <- as.numeric(US$EBIT)-US$InterestEgty
  US$PretaxCODEgty <- as.numeric(Bond$Spread[US$SP]) + USRF
  US$CODEgty <- as.numeric(US$PretaxCODEgty)*(1-as.numeric(US$TaxR))
  US$TREgty <- ifelse(US$InterestEgty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestEgty )
  US$BetaEgty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TREgty))*US$DbyEEgty)))
  US$COEEgty <- (as.numeric(US$BetaEgty)*as.numeric(USRP))+as.numeric(USRF)
  US$WACCEgty <- (as.numeric(US$CODEgty)*as.numeric(US$DbyTEgty))+(as.numeric(US$COEEgty)*(1-as.numeric(US$DbyTEgty)))
  US$InterestEgty <- as.numeric(as.numeric(Bond$Spread[US$SP])+USRF)/(1-US$TaxR)*US$DebtEgty
  US$TaxEgty <- US$TaxableIncomeEgty*US$TaxR
  US$PreTaxIntCovEgty <- US$EBIT/US$InterestEgty
  US$TaxRABEgty <- ifelse(US$InterestEgty<US$EBIT, US$TaxR,as.numeric(US$TaxR)*as.numeric(US$EBIT)/as.numeric(US$InterestEgty))
  US$NetIncomeEgty <-US$TaxableIncomeEgty-US$TaxEgty
  US$NetOpIncomeEgty <-as.numeric(US$NetIncomeEgty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovEgty <- US$EBIT/US$InterestEgty
  US$ValueEgty <- US$EV*(1+((US$WACC-US$WACCEgty)/(US$WACCEgty-USRF)))

  # Debt 90%

  US$DbyTNinty <- 0.90
  US$DbyENinty <- 9
  US$DebtNinty <- US$DbyTNinty*(as.numeric(US$MarketCap)+as.numeric(US$LTD))
  US$InterestNinty <- as.numeric(Bond$Spread[US$SP]+USRF)/(1-US$TaxR)*US$DebtNinty
  US$TaxableIncomeNinty <- as.numeric(US$EBIT)-US$InterestNinty
  US$TaxNinty <- US$TaxableIncomeNinty*US$TaxR
  US$NetIncomeNinty <-US$TaxableIncomeNinty-US$TaxNinty
  US$NetOpIncomeNinty <-as.numeric(US$NetIncomeNinty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovNinty <- US$EBIT/US$InterestNinty
  US$TRNinty <- ifelse(US$InterestNinty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestNinty )
  US$BetaNinty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRNinty))*US$DbyENinty)))
  US$COENinty <- (as.numeric(US$BetaNinty)*as.numeric(USRP))+as.numeric(USRF)



  US$TaxableIncomeNinty <- as.numeric(US$EBIT)-US$InterestNinty
  US$PretaxCODNinty <- as.numeric(Bond$Spread[US$SP]) + USRF
  US$CODNinty <- as.numeric(US$PretaxCODNinty)*(1-as.numeric(US$TaxR))
  US$TRNinty <- ifelse(US$InterestNinty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestNinty )
  US$BetaNinty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRNinty))*US$DbyENinty)))
  US$COENinty <- (as.numeric(US$BetaNinty)*as.numeric(USRP))+as.numeric(USRF)
  US$WACCNinty <- (as.numeric(US$CODNinty)*as.numeric(US$DbyTNinty))+(as.numeric(US$COENinty)*(1-as.numeric(US$DbyTNinty)))
  US$InterestNinty <- as.numeric(as.numeric(Bond$Spread[US$SP])+USRF)/(1-US$TaxR)*US$DebtNinty
  US$TaxNinty <- US$TaxableIncomeNinty*US$TaxR
  US$PreTaxIntCovNinty <- US$EBIT/US$InterestNinty
  US$TaxRABNinty <- ifelse(US$InterestNinty<US$EBIT, US$TaxR,as.numeric(US$TaxR)*as.numeric(US$EBIT)/as.numeric(US$InterestNinty))
  US$NetIncomeNinty <-US$TaxableIncomeNinty-US$TaxNinty
  US$NetOpIncomeNinty <-as.numeric(US$NetIncomeNinty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovNinty <- US$EBIT/US$InterestNinty
  US$ValueNinty <- US$EV*(1+((US$WACC-US$WACCNinty)/(US$WACCNinty-USRF)))

  US$MaxEV <- ifelse(US$ValueZero>US$ValueTen,US$ValueZero,US$ValueTen)
  US$MaxEV <- ifelse(US$MaxEV>US$ValueTwty,US$MaxEV,US$ValueTwty)
  US$MaxEV<- ifelse(US$MaxEV>US$ValueTrty,US$MaxEV,US$ValueTrty)
  US$MaxEV<- ifelse(US$MaxEV>US$ValueFrty,US$MaxEV,US$ValueFrty)
  US$MaxEV<- ifelse(US$MaxEV>US$ValueFifty,US$MaxEV,US$ValueFifty)
  US$MaxEV<- ifelse(US$MaxEV>US$ValueSxty,US$MaxEV,US$ValueSxty)
  US$MaxEV<- ifelse(US$MaxEV>US$ValueSvnty,US$MaxEV,US$ValueSvnty)
  US$MaxEV<- ifelse(US$MaxEV>US$ValueEgty,US$MaxEV,US$ValueEgty)
  US$MaxEV<- ifelse(US$MaxEV>US$ValueNinty,US$MaxEV,US$ValueNinty)
  US$MaxEV <- ifelse(US$MaxEV>US$EV,US$MaxEV,US$EV)

  US$OptimalD  <- ifelse(US$MaxEV==US$ValueZero,0,ifelse(US$MaxEV==US$ValueTen,10,ifelse(US$MaxEV==US$ValueTwty,20,ifelse(US$MaxEV==US$ValueTrty,30,ifelse(US$MaxEV==US$ValueFrty,40,ifelse(US$MaxEV==US$ValueFifty,50,ifelse(US$MaxEV==US$ValueSxty,60,ifelse(US$MaxEV==US$ValueSvnty,70,ifelse(US$MaxEV==US$ValueEgty,80,ifelse(US$MaxEV==US$ValueNinty,90,0))))))))))

  return(US)}


