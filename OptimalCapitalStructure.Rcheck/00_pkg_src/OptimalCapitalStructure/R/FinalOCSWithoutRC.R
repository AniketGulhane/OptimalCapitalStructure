
FinlOCSWRC <- function(...){
  US <- data.frame(...)
  US$ValueNinty <-0
  US$ValueEgty <-0
  US$ValueSvnty <-0
  US$ValueSxty <-0
  US$ValueFifty <-0
  US$ValueFrty <-0
  US$ValueTrty <-0
  US$ValueTwty <-0
  US$ValueTen <-0

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

  US$PretaxCODTen <- (as.numeric(Bond$Spread[US$SP]) + USRF)
  US$InterestTen <- US$DebtTen*US$PretaxCODTen
  US$TaxableIncomeTen <- as.numeric(US$EBIT)-US$InterestTen
  US$TaxTen <- US$TaxableIncomeTen*US$TaxR
  US$NetIncomeTen <-US$TaxableIncomeTen-US$TaxTen
  US$NetOpIncomeTen <-as.numeric(US$NetIncomeTen)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovTen <- US$EBIT/US$InterestTen
  US$TRTen <- ifelse(US$InterestTen<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestTen )
  US$BetaTen <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRTen))*US$DbyETen)))
  US$COETen <- (as.numeric(US$BetaTen)*as.numeric(USRP))+as.numeric(USRF)




  US$SPTen <- 0
  US$SPTen <- ifelse(US$PreTaxIntCovTen< US$SPV,as.numeric(US$SP)+1,as.numeric(US$SP))
  US$SPVTen <- Bond$ICR[US$SPTen]
  US$PretaxCODTen <- as.numeric(Bond$Spread[US$SPTen]) + USRF
  US$InterestTen <- US$DebtTen*US$PretaxCODTen
  US$PreTaxIntCovTen <- US$EBIT/US$InterestTen



  US$SPTen1 <- ifelse(US$PreTaxIntCovTen< US$SPVTen,as.numeric(US$SPTen)+1,as.numeric(US$SPTen))
  US$SPVTen <- Bond$ICR[US$SPTen1]
  US$PretaxCODTen <- (as.numeric(Bond$Spread[US$SPTen1]) + USRF)
  US$InterestTen <- US$DebtTen*US$PretaxCODTen
  US$PreTaxIntCovTen <- US$EBIT/US$InterestTen

  US$SPTen2 <- ifelse(US$PreTaxIntCovTen< US$SPVTen,as.numeric(US$SPTen1)+1,as.numeric(US$SPTen1))
  US$SPVTen <- Bond$ICR[US$SPTen2]
  US$PretaxCODTen <- (as.numeric(Bond$Spread[US$SPTen2]) + USRF)
  US$InterestTen <- US$DebtTen*US$PretaxCODTen
  US$PreTaxIntCovTen <- US$EBIT/US$InterestTen


  US$SPTen3 <- ifelse(US$PreTaxIntCovTen< US$SPVTen,as.numeric(US$SPTen2)+1,as.numeric(US$SPTen2))
  US$SPVTen <- Bond$ICR[US$SPTen3]
  US$PretaxCODTen <- (as.numeric(Bond$Spread[US$SPTen3]) + USRF)
  US$InterestTen <- US$DebtTen*US$PretaxCODTen
  US$PreTaxIntCovTen <- US$EBIT/US$InterestTen

  US$SPTen4 <- ifelse(US$PreTaxIntCovTen< US$SPVTen,as.numeric(US$SPTen3)+1,as.numeric(US$SPTen3))
  US$SPVTen <- Bond$ICR[US$SPTen4]
  US$PretaxCODTen <- (as.numeric(Bond$Spread[US$SPTen4]) + USRF)
  US$InterestTen <- US$DebtTen*US$PretaxCODTen
  US$PreTaxIntCovTen <- US$EBIT/US$InterestTen

  US$SPTen5 <- ifelse(US$PreTaxIntCovTen< US$SPVTen,as.numeric(US$SPTen4)+1,as.numeric(US$SPTen4))
  US$SPVTen <- Bond$ICR[US$SPTen5]
  US$PretaxCODTen <- (as.numeric(Bond$Spread[US$SPTen5]) + USRF)
  US$InterestTen <-  US$DebtTen*US$PretaxCODTen
  US$PreTaxIntCovTen <- US$EBIT/US$InterestTen


  US$SPTen6 <- ifelse(US$PreTaxIntCovTen< US$SPVTen,as.numeric(US$SPTen5)+1,as.numeric(US$SPTen5))
  US$SPVTen <- Bond$ICR[US$SPTen6]
  US$PretaxCODTen <- (as.numeric(Bond$Spread[US$SPTen6]) + USRF)
  US$InterestTen <-  US$DebtTen*US$PretaxCODTen
  US$PreTaxIntCovTen <- US$EBIT/US$InterestTen


  US$SPTen7 <- ifelse(US$PreTaxIntCovTen< US$SPVTen,as.numeric(US$SPTen6)+1,as.numeric(US$SPTen6))
  US$SPVTen <- Bond$ICR[US$SPTen7]
  US$PretaxCODTen <-  (as.numeric(Bond$Spread[US$SPTen7]) + USRF)
  US$InterestTen <- US$DebtTen*US$PretaxCODTen
  US$PreTaxIntCovTen <- US$EBIT/US$InterestTen

  US$SPTen8 <- ifelse(US$PreTaxIntCovTen< US$SPVTen,as.numeric(US$SPTen7)+1,as.numeric(US$SPTen7))
  US$SPVTen <- Bond$ICR[US$SPTen8]
  US$PretaxCODTen <-  (as.numeric(Bond$Spread[US$SPTen8]) + USRF)
  US$InterestTen <- US$DebtTen*US$PretaxCODTen
  US$PreTaxIntCovTen <- US$EBIT/US$InterestTen

  US$SPTen9 <- ifelse(US$PreTaxIntCovTen< US$SPVTen,as.numeric(US$SPTen8)+1,as.numeric(US$SPTen8))
  US$SPVTen <- Bond$ICR[US$SPTen9]
  US$PretaxCODTen <-  (as.numeric(Bond$Spread[US$SPTen9]) + USRF)
  US$InterestTen <- US$DebtTen*US$PretaxCODTen
  US$PreTaxIntCovTen <- US$EBIT/US$InterestTen

  ifelse(US$SPTen<=14,US$SPTen9,14)
  US$SPVTen <- Bond$ICR[US$SPTen9]

  if(US$SPVTen >= MinRatingV)
  {

  US$PretaxCODTen <- as.numeric(Bond$Spread[US$SPTen7]) + USRF
  US$CODTen <- as.numeric(US$PretaxCODTen)*(1-as.numeric(US$TaxR))
  US$WACCTen <- (as.numeric(US$CODTen)*as.numeric(US$DbyTTen))+(as.numeric(US$COETen)*(1-as.numeric(US$DbyTTen)))
  US$ValueTen <- US$EV*(1+((US$WACC-US$WACCTen)/(US$WACCTen-USRF)))


  US$TaxableIncomeTen <- as.numeric(US$EBIT)-US$InterestTen
  US$PretaxCODTen <- as.numeric(Bond$Spread[US$SPTen9]) + USRF
  US$CODTen <- as.numeric(US$PretaxCODTen)*(1-as.numeric(US$TaxR))
  US$TRTen <- ifelse(US$InterestTen<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestTen )
  US$BetaTen <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRTen))*US$DbyETen)))
  US$COETen <- (as.numeric(US$BetaTen)*as.numeric(USRP))+as.numeric(USRF)
  US$WACCTen <- (as.numeric(US$CODTen)*as.numeric(US$DbyTTen))+(as.numeric(US$COETen)*(1-as.numeric(US$DbyTTen)))
  US$InterestTen <- as.numeric(as.numeric(Bond$Spread[US$SPTen7])+USRF)/(1-US$TaxR)*US$DebtTen
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


  US$InterestTwty <- as.numeric(Bond$Spread[US$SPTen9]+USRF)/(1-US$TaxR)*US$DebtTwty
  US$TaxableIncomeTwty <- as.numeric(US$EBIT)-US$InterestTwty
  US$TaxTwty <- US$TaxableIncomeTwty*US$TaxR
  US$NetIncomeTwty <-US$TaxableIncomeTwty-US$TaxTwty
  US$NetOpIncomeTwty <-as.numeric(US$NetIncomeTwty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovTwty <- US$EBIT/US$InterestTwty
  US$TRTwty <- ifelse(US$InterestTwty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestTwty )
  US$BetaTwty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRTwty))*US$DbyETwty)))
  US$COETwty <- (as.numeric(US$BetaTwty)*as.numeric(USRP))+as.numeric(USRF)

  US$SPTwty <- 0
  US$SPTwty <- ifelse(US$PreTaxIntCovTwty< US$SPVTen,as.numeric(US$SPTen9)+1,as.numeric(US$SPTen9))
  US$SPVTwty <- Bond$ICR[US$SPTwty]
  US$PretaxCODTwty <- as.numeric(Bond$Spread[US$SPTwty]) + USRF
  US$InterestTwty <- US$DebtTwty*US$PretaxCODTwty
  US$PreTaxIntCovTwty <- US$EBIT/US$InterestTwty



  US$SPTwty1 <- ifelse(US$PreTaxIntCovTwty< US$SPVTwty,as.numeric(US$SPTwty)+1,as.numeric(US$SPTwty))
  US$SPVTwty <- Bond$ICR[US$SPTwty1]
  US$PretaxCODTwty <- (as.numeric(Bond$Spread[US$SPTwty1]) + USRF)
  US$InterestTwty <- US$DebtTwty*US$PretaxCODTwty
  US$PreTaxIntCovTwty <- US$EBIT/US$InterestTwty

  US$SPTwty2 <- ifelse(US$PreTaxIntCovTwty< US$SPVTwty,as.numeric(US$SPTwty1)+1,as.numeric(US$SPTwty1))
  US$SPVTwty <- Bond$ICR[US$SPTwty2]
  US$PretaxCODTwty <- (as.numeric(Bond$Spread[US$SPTwty2]) + USRF)
  US$InterestTwty <- US$DebtTwty*US$PretaxCODTwty
  US$PreTaxIntCovTwty <- US$EBIT/US$InterestTwty


  US$SPTwty3 <- ifelse(US$PreTaxIntCovTwty< US$SPVTwty,as.numeric(US$SPTwty2)+1,as.numeric(US$SPTwty2))
  US$SPVTwty <- Bond$ICR[US$SPTwty3]
  US$PretaxCODTwty <- (as.numeric(Bond$Spread[US$SPTwty3]) + USRF)
  US$InterestTwty <- US$DebtTwty*US$PretaxCODTwty
  US$PreTaxIntCovTwty <- US$EBIT/US$InterestTwty

  US$SPTwty4 <- ifelse(US$PreTaxIntCovTwty< US$SPVTwty,as.numeric(US$SPTwty3)+1,as.numeric(US$SPTwty3))
  US$SPVTwty <- Bond$ICR[US$SPTwty4]
  US$PretaxCODTwty <- (as.numeric(Bond$Spread[US$SPTwty4]) + USRF)
  US$InterestTwty <- US$DebtTwty*US$PretaxCODTwty
  US$PreTaxIntCovTwty <- US$EBIT/US$InterestTwty

  US$SPTwty5 <- ifelse(US$PreTaxIntCovTwty< US$SPVTwty,as.numeric(US$SPTwty4)+1,as.numeric(US$SPTwty4))
  US$SPVTwty <- Bond$ICR[US$SPTwty5]
  US$PretaxCODTwty <- (as.numeric(Bond$Spread[US$SPTwty5]) + USRF)
  US$InterestTwty <-  US$DebtTwty*US$PretaxCODTwty
  US$PreTaxIntCovTwty <- US$EBIT/US$InterestTwty


  US$SPTwty6 <- ifelse(US$PreTaxIntCovTwty< US$SPVTwty,as.numeric(US$SPTwty5)+1,as.numeric(US$SPTwty5))
  US$SPVTwty <- Bond$ICR[US$SPTwty6]
  US$PretaxCODTwty <- (as.numeric(Bond$Spread[US$SPTwty6]) + USRF)
  US$InterestTwty <-  US$DebtTwty*US$PretaxCODTwty
  US$PreTaxIntCovTwty <- US$EBIT/US$InterestTwty


  US$SPTwty7 <- ifelse(US$PreTaxIntCovTwty< US$SPVTwty,as.numeric(US$SPTwty6)+1,as.numeric(US$SPTwty6))
  US$SPVTwty <- Bond$ICR[US$SPTwty7]
  US$PretaxCODTwty <-  (as.numeric(Bond$Spread[US$SPTwty7]) + USRF)
  US$InterestTwty <- US$DebtTwty*US$PretaxCODTwty
  US$PreTaxIntCovTwty <- US$EBIT/US$InterestTwty

  US$SPTwty8 <- ifelse(US$PreTaxIntCovTwty< US$SPVTwty,as.numeric(US$SPTwty7)+1,as.numeric(US$SPTwty7))
  US$SPVTwty <- Bond$ICR[US$SPTwty8]
  US$PretaxCODTwty <-  (as.numeric(Bond$Spread[US$SPTwty8]) + USRF)
  US$InterestTwty <- US$DebtTwty*US$PretaxCODTwty
  US$PreTaxIntCovTwty <- US$EBIT/US$InterestTwty

  US$SPTwty9 <- ifelse(US$PreTaxIntCovTwty< US$SPVTwty,as.numeric(US$SPTwty8)+1,as.numeric(US$SPTwty8))
  US$SPVTwty <- Bond$ICR[US$SPTwty9]
  US$PretaxCODTwty <-  (as.numeric(Bond$Spread[US$SPTwty9]) + USRF)
  US$InterestTwty <- US$DebtTwty*US$PretaxCODTwty
  US$PreTaxIntCovTwty <- US$EBIT/US$InterestTwty

  ifelse(US$SPTwty<=14,US$SPTwty9,14)
  US$SPVTwty <- Bond$ICR[US$SPTwty9]

  if(US$SPVTwty >= MinRatingV)
  {

  US$TaxableIncomeTwty <- as.numeric(US$EBIT)-US$InterestTwty
  US$PretaxCODTwty <- as.numeric(Bond$Spread[US$SPTwty9]) + USRF
  US$CODTwty <- as.numeric(US$PretaxCODTwty)*(1-as.numeric(US$TaxR))
  US$TRTwty <- ifelse(US$InterestTwty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestTwty )
  US$BetaTwty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRTwty))*US$DbyETwty)))
  US$COETwty <- (as.numeric(US$BetaTwty)*as.numeric(USRP))+as.numeric(USRF)
  US$WACCTwty <- (as.numeric(US$CODTwty)*as.numeric(US$DbyTTwty))+(as.numeric(US$COETwty)*(1-as.numeric(US$DbyTTwty)))
  US$InterestTwty <- as.numeric(as.numeric(Bond$Spread[US$SPTwty9])+USRF)/(1-US$TaxR)*US$DebtTwty
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


  US$InterestTrty <- as.numeric(Bond$Spread[US$SPTwty9]+USRF)/(1-US$TaxR)*US$DebtTrty
  US$TaxableIncomeTrty <- as.numeric(US$EBIT)-US$InterestTrty
  US$TaxTrty <- US$TaxableIncomeTrty*US$TaxR
  US$NetIncomeTrty <-US$TaxableIncomeTrty-US$TaxTrty
  US$NetOpIncomeTrty <-as.numeric(US$NetIncomeTrty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovTrty <- US$EBIT/US$InterestTrty
  US$TRTrty <- ifelse(US$InterestTrty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestTrty )
  US$BetaTrty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRTrty))*US$DbyETrty)))
  US$COETrty <- (as.numeric(US$BetaTrty)*as.numeric(USRP))+as.numeric(USRF)



  US$SPTrty <- 0
  US$SPTrty <- ifelse(US$PreTaxIntCovTrty< US$SPVTwty,as.numeric(US$SPTwty7)+1,as.numeric(US$SPTwty7))
  US$SPVTrty <- Bond$ICR[US$SPTrty]
  US$PretaxCODTrty <- as.numeric(Bond$Spread[US$SPTrty]) + USRF
  US$InterestTrty <- US$DebtTrty*US$PretaxCODTrty
  US$PreTaxIntCovTrty <- US$EBIT/US$InterestTrty



  US$SPTrty1 <- ifelse(US$PreTaxIntCovTrty< US$SPVTrty,as.numeric(US$SPTrty)+1,as.numeric(US$SPTrty))
  US$SPVTrty <- Bond$ICR[US$SPTrty1]
  US$PretaxCODTrty <- (as.numeric(Bond$Spread[US$SPTrty1]) + USRF)
  US$InterestTrty <- US$DebtTrty*US$PretaxCODTrty
  US$PreTaxIntCovTrty <- US$EBIT/US$InterestTrty

  US$SPTrty2 <- ifelse(US$PreTaxIntCovTrty< US$SPVTrty,as.numeric(US$SPTrty1)+1,as.numeric(US$SPTrty1))
  US$SPVTrty <- Bond$ICR[US$SPTrty2]
  US$PretaxCODTrty <- (as.numeric(Bond$Spread[US$SPTrty2]) + USRF)
  US$InterestTrty <- US$DebtTrty*US$PretaxCODTrty
  US$PreTaxIntCovTrty <- US$EBIT/US$InterestTrty


  US$SPTrty3 <- ifelse(US$PreTaxIntCovTrty< US$SPVTrty,as.numeric(US$SPTrty2)+1,as.numeric(US$SPTrty2))
  US$SPVTrty <- Bond$ICR[US$SPTrty3]
  US$PretaxCODTrty <- (as.numeric(Bond$Spread[US$SPTrty3]) + USRF)
  US$InterestTrty <- US$DebtTrty*US$PretaxCODTrty
  US$PreTaxIntCovTrty <- US$EBIT/US$InterestTrty

  US$SPTrty4 <- ifelse(US$PreTaxIntCovTrty< US$SPVTrty,as.numeric(US$SPTrty3)+1,as.numeric(US$SPTrty3))
  US$SPVTrty <- Bond$ICR[US$SPTrty4]
  US$PretaxCODTrty <- (as.numeric(Bond$Spread[US$SPTrty4]) + USRF)
  US$InterestTrty <- US$DebtTrty*US$PretaxCODTrty
  US$PreTaxIntCovTrty <- US$EBIT/US$InterestTrty

  US$SPTrty5 <- ifelse(US$PreTaxIntCovTrty< US$SPVTrty,as.numeric(US$SPTrty4)+1,as.numeric(US$SPTrty4))
  US$SPVTrty <- Bond$ICR[US$SPTrty5]
  US$PretaxCODTrty <- (as.numeric(Bond$Spread[US$SPTrty5]) + USRF)
  US$InterestTrty <-  US$DebtTrty*US$PretaxCODTrty
  US$PreTaxIntCovTrty <- US$EBIT/US$InterestTrty


  US$SPTrty6 <- ifelse(US$PreTaxIntCovTrty< US$SPVTrty,as.numeric(US$SPTrty5)+1,as.numeric(US$SPTrty5))
  US$SPVTrty <- Bond$ICR[US$SPTrty6]
  US$PretaxCODTrty <- (as.numeric(Bond$Spread[US$SPTrty6]) + USRF)
  US$InterestTrty <-  US$DebtTrty*US$PretaxCODTrty
  US$PreTaxIntCovTrty <- US$EBIT/US$InterestTrty


  US$SPTrty7 <- ifelse(US$PreTaxIntCovTrty< US$SPVTrty,as.numeric(US$SPTrty6)+1,as.numeric(US$SPTrty6))
  US$SPVTrty <- Bond$ICR[US$SPTrty7]
  US$PretaxCODTrty <-  (as.numeric(Bond$Spread[US$SPTrty7]) + USRF)
  US$InterestTrty <- US$DebtTrty*US$PretaxCODTrty
  US$PreTaxIntCovTrty <- US$EBIT/US$InterestTrty

  US$SPTrty8 <- ifelse(US$PreTaxIntCovTrty< US$SPVTrty,as.numeric(US$SPTrty7)+1,as.numeric(US$SPTrty7))
  US$SPVTrty <- Bond$ICR[US$SPTrty8]
  US$PretaxCODTrty <-  (as.numeric(Bond$Spread[US$SPTrty8]) + USRF)
  US$InterestTrty <- US$DebtTrty*US$PretaxCODTrty
  US$PreTaxIntCovTrty <- US$EBIT/US$InterestTrty

  US$SPTrty9 <- ifelse(US$PreTaxIntCovTrty< US$SPVTrty,as.numeric(US$SPTrty8)+1,as.numeric(US$SPTrty8))
  US$SPVTrty <- Bond$ICR[US$SPTrty9]
  US$PretaxCODTrty <-  (as.numeric(Bond$Spread[US$SPTrty9]) + USRF)
  US$InterestTrty <- US$DebtTrty*US$PretaxCODTrty
  US$PreTaxIntCovTrty <- US$EBIT/US$InterestTrty

  ifelse(US$SPTrty<=14,US$SPTrty9,14)
  US$SPVTrty <- Bond$ICR[US$SPTrty9]

  if(US$SPVTrty >= MinRatingV)
  {

  US$TaxableIncomeTrty <- as.numeric(US$EBIT)-US$InterestTrty
  US$PretaxCODTrty <- as.numeric(Bond$Spread[US$SPTrty9]) + USRF
  US$CODTrty <- as.numeric(US$PretaxCODTrty)*(1-as.numeric(US$TaxR))
  US$TRTrty <- ifelse(US$InterestTrty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestTrty )
  US$BetaTrty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRTrty))*US$DbyETrty)))
  US$COETrty <- (as.numeric(US$BetaTrty)*as.numeric(USRP))+as.numeric(USRF)
  US$WACCTrty <- (as.numeric(US$CODTrty)*as.numeric(US$DbyTTrty))+(as.numeric(US$COETrty)*(1-as.numeric(US$DbyTTrty)))
  US$InterestTrty <- as.numeric(as.numeric(Bond$Spread[US$SPTrty9])+USRF)/(1-US$TaxR)*US$DebtTrty
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
  US$InterestFrty <- as.numeric(Bond$Spread[US$SPTrty9]+USRF)/(1-US$TaxR)*US$DebtFrty
  US$TaxableIncomeFrty <- as.numeric(US$EBIT)-US$InterestFrty
  US$TaxFrty <- US$TaxableIncomeFrty*US$TaxR
  US$NetIncomeFrty <-US$TaxableIncomeFrty-US$TaxFrty
  US$NetOpIncomeFrty <-as.numeric(US$NetIncomeFrty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovFrty <- US$EBIT/US$InterestFrty
  US$TRFrty <- ifelse(US$InterestFrty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestFrty )
  US$BetaFrty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRFrty))*US$DbyEFrty)))
  US$COEFrty <- (as.numeric(US$BetaFrty)*as.numeric(USRP))+as.numeric(USRF)


  US$SPFrty <- 0
  US$SPFrty <- ifelse(US$PreTaxIntCovFrty< US$SPVTrty,as.numeric(US$SPTrty9)+1,as.numeric(US$SPTrty9))
  US$SPVFrty <- Bond$ICR[US$SPFrty]
  US$PretaxCODFrty <- as.numeric(Bond$Spread[US$SPFrty]) + USRF
  US$InterestFrty <- US$DebtFrty*US$PretaxCODFrty
  US$PreTaxIntCovFrty <- US$EBIT/US$InterestFrty



  US$SPFrty1 <- ifelse(US$PreTaxIntCovFrty< US$SPVFrty,as.numeric(US$SPFrty)+1,as.numeric(US$SPFrty))
  US$SPVFrty <- Bond$ICR[US$SPFrty1]
  US$PretaxCODFrty <- (as.numeric(Bond$Spread[US$SPFrty1]) + USRF)
  US$InterestFrty <- US$DebtFrty*US$PretaxCODFrty
  US$PreTaxIntCovFrty <- US$EBIT/US$InterestFrty

  US$SPFrty2 <- ifelse(US$PreTaxIntCovFrty< US$SPVFrty,as.numeric(US$SPFrty1)+1,as.numeric(US$SPFrty1))
  US$SPVFrty <- Bond$ICR[US$SPFrty2]
  US$PretaxCODFrty <- (as.numeric(Bond$Spread[US$SPFrty2]) + USRF)
  US$InterestFrty <- US$DebtFrty*US$PretaxCODFrty
  US$PreTaxIntCovFrty <- US$EBIT/US$InterestFrty


  US$SPFrty3 <- ifelse(US$PreTaxIntCovFrty< US$SPVFrty,as.numeric(US$SPFrty2)+1,as.numeric(US$SPFrty2))
  US$SPVFrty <- Bond$ICR[US$SPFrty3]
  US$PretaxCODFrty <- (as.numeric(Bond$Spread[US$SPFrty3]) + USRF)
  US$InterestFrty <- US$DebtFrty*US$PretaxCODFrty
  US$PreTaxIntCovFrty <- US$EBIT/US$InterestFrty

  US$SPFrty4 <- ifelse(US$PreTaxIntCovFrty< US$SPVFrty,as.numeric(US$SPFrty3)+1,as.numeric(US$SPFrty3))
  US$SPVFrty <- Bond$ICR[US$SPFrty4]
  US$PretaxCODFrty <- (as.numeric(Bond$Spread[US$SPFrty4]) + USRF)
  US$InterestFrty <- US$DebtFrty*US$PretaxCODFrty
  US$PreTaxIntCovFrty <- US$EBIT/US$InterestFrty

  US$SPFrty5 <- ifelse(US$PreTaxIntCovFrty< US$SPVFrty,as.numeric(US$SPFrty4)+1,as.numeric(US$SPFrty4))
  US$SPVFrty <- Bond$ICR[US$SPFrty5]
  US$PretaxCODFrty <- (as.numeric(Bond$Spread[US$SPFrty5]) + USRF)
  US$InterestFrty <-  US$DebtFrty*US$PretaxCODFrty
  US$PreTaxIntCovFrty <- US$EBIT/US$InterestFrty


  US$SPFrty6 <- ifelse(US$PreTaxIntCovFrty< US$SPVFrty,as.numeric(US$SPFrty5)+1,as.numeric(US$SPFrty5))
  US$SPVFrty <- Bond$ICR[US$SPFrty6]
  US$PretaxCODFrty <- (as.numeric(Bond$Spread[US$SPFrty6]) + USRF)
  US$InterestFrty <-  US$DebtFrty*US$PretaxCODFrty
  US$PreTaxIntCovFrty <- US$EBIT/US$InterestFrty


  US$SPFrty7 <- ifelse(US$PreTaxIntCovFrty< US$SPVFrty,as.numeric(US$SPFrty6)+1,as.numeric(US$SPFrty6))
  US$SPVFrty <- Bond$ICR[US$SPFrty7]
  US$PretaxCODFrty <-  (as.numeric(Bond$Spread[US$SPFrty7]) + USRF)
  US$InterestFrty <- US$DebtFrty*US$PretaxCODFrty
  US$PreTaxIntCovFrty <- US$EBIT/US$InterestFrty

  US$SPFrty8 <- ifelse(US$PreTaxIntCovFrty< US$SPVFrty,as.numeric(US$SPFrty7)+1,as.numeric(US$SPFrty7))
  US$SPVFrty <- Bond$ICR[US$SPFrty8]
  US$PretaxCODFrty <-  (as.numeric(Bond$Spread[US$SPFrty8]) + USRF)
  US$InterestFrty <- US$DebtFrty*US$PretaxCODFrty
  US$PreTaxIntCovFrty <- US$EBIT/US$InterestFrty

  US$SPFrty9 <- ifelse(US$PreTaxIntCovFrty< US$SPVFrty,as.numeric(US$SPFrty8)+1,as.numeric(US$SPFrty8))
  US$SPVFrty <- Bond$ICR[US$SPFrty9]
  US$PretaxCODFrty <-  (as.numeric(Bond$Spread[US$SPFrty9]) + USRF)
  US$InterestFrty <- US$DebtFrty*US$PretaxCODFrty
  US$PreTaxIntCovFrty <- US$EBIT/US$InterestFrty

  ifelse(US$SPFrty<=14,US$SPFrty9,14)
  US$SPVFrty <- Bond$ICR[US$SPFrty9]

  if(US$SPVFrty >= MinRatingV)
  {
  US$TaxableIncomeFrty <- as.numeric(US$EBIT)-US$InterestFrty
  US$PretaxCODFrty <- as.numeric(Bond$Spread[US$SPFrty9]) + USRF
  US$CODFrty <- as.numeric(US$PretaxCODFrty)*(1-as.numeric(US$TaxR))
  US$TRFrty <- ifelse(US$InterestFrty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestFrty )
  US$BetaFrty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRFrty))*US$DbyEFrty)))
  US$COEFrty <- (as.numeric(US$BetaFrty)*as.numeric(USRP))+as.numeric(USRF)
  US$WACCFrty <- (as.numeric(US$CODFrty)*as.numeric(US$DbyTFrty))+(as.numeric(US$COEFrty)*(1-as.numeric(US$DbyTFrty)))
  US$InterestFrty <- as.numeric(as.numeric(Bond$Spread[US$SPFrty9])+USRF)/(1-US$TaxR)*US$DebtFrty
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
  US$InterestFifty <- as.numeric(Bond$Spread[US$SPFrty9]+USRF)/(1-US$TaxR)*US$DebtFifty
  US$TaxableIncomeFifty <- as.numeric(US$EBIT)-US$InterestFifty
  US$TaxFifty <- US$TaxableIncomeFifty*US$TaxR
  US$NetIncomeFifty <-US$TaxableIncomeFifty-US$TaxFifty
  US$NetOpIncomeFifty <-as.numeric(US$NetIncomeFifty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovFifty <- US$EBIT/US$InterestFifty
  US$TRFifty <- ifelse(US$InterestFifty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestFifty )
  US$BetaFifty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRFifty))*US$DbyEFifty)))
  US$COEFifty <- (as.numeric(US$BetaFifty)*as.numeric(USRP))+as.numeric(USRF)


  US$SPFifty <- 0
  US$SPFifty <- ifelse(US$PreTaxIntCovFifty< US$SPVFrty,as.numeric(US$SPFrty9)+1,as.numeric(US$SPFrty9))
  US$SPVFifty <- Bond$ICR[US$SPFifty]
  US$PretaxCODFifty <- as.numeric(Bond$Spread[US$SPFifty]) + USRF
  US$InterestFifty <- US$DebtFifty*US$PretaxCODFifty
  US$PreTaxIntCovFifty <- US$EBIT/US$InterestFifty



  US$SPFifty1 <- ifelse(US$PreTaxIntCovFifty< US$SPVFifty,as.numeric(US$SPFifty)+1,as.numeric(US$SPFifty))
  US$SPVFifty <- Bond$ICR[US$SPFifty1]
  US$PretaxCODFifty <- (as.numeric(Bond$Spread[US$SPFifty1]) + USRF)
  US$InterestFifty <- US$DebtFifty*US$PretaxCODFifty
  US$PreTaxIntCovFifty <- US$EBIT/US$InterestFifty

  US$SPFifty2 <- ifelse(US$PreTaxIntCovFifty< US$SPVFifty,as.numeric(US$SPFifty1)+1,as.numeric(US$SPFifty1))
  US$SPVFifty <- Bond$ICR[US$SPFifty2]
  US$PretaxCODFifty <- (as.numeric(Bond$Spread[US$SPFifty2]) + USRF)
  US$InterestFifty <- US$DebtFifty*US$PretaxCODFifty
  US$PreTaxIntCovFifty <- US$EBIT/US$InterestFifty


  US$SPFifty3 <- ifelse(US$PreTaxIntCovFifty< US$SPVFifty,as.numeric(US$SPFifty2)+1,as.numeric(US$SPFifty2))
  US$SPVFifty <- Bond$ICR[US$SPFifty3]
  US$PretaxCODFifty <- (as.numeric(Bond$Spread[US$SPFifty3]) + USRF)
  US$InterestFifty <- US$DebtFifty*US$PretaxCODFifty
  US$PreTaxIntCovFifty <- US$EBIT/US$InterestFifty

  US$SPFifty4 <- ifelse(US$PreTaxIntCovFifty< US$SPVFifty,as.numeric(US$SPFifty3)+1,as.numeric(US$SPFifty3))
  US$SPVFifty <- Bond$ICR[US$SPFifty4]
  US$PretaxCODFifty <- (as.numeric(Bond$Spread[US$SPFifty4]) + USRF)
  US$InterestFifty <- US$DebtFifty*US$PretaxCODFifty
  US$PreTaxIntCovFifty <- US$EBIT/US$InterestFifty

  US$SPFifty5 <- ifelse(US$PreTaxIntCovFifty< US$SPVFifty,as.numeric(US$SPFifty4)+1,as.numeric(US$SPFifty4))
  US$SPVFifty <- Bond$ICR[US$SPFifty5]
  US$PretaxCODFifty <- (as.numeric(Bond$Spread[US$SPFifty5]) + USRF)
  US$InterestFifty <-  US$DebtFifty*US$PretaxCODFifty
  US$PreTaxIntCovFifty <- US$EBIT/US$InterestFifty


  US$SPFifty6 <- ifelse(US$PreTaxIntCovFifty< US$SPVFifty,as.numeric(US$SPFifty5)+1,as.numeric(US$SPFifty5))
  US$SPVFifty <- Bond$ICR[US$SPFifty6]
  US$PretaxCODFifty <- (as.numeric(Bond$Spread[US$SPFifty6]) + USRF)
  US$InterestFifty <-  US$DebtFifty*US$PretaxCODFifty
  US$PreTaxIntCovFifty <- US$EBIT/US$InterestFifty


  US$SPFifty7 <- ifelse(US$PreTaxIntCovFifty< US$SPVFifty,as.numeric(US$SPFifty6)+1,as.numeric(US$SPFifty6))
  US$SPVFifty <- Bond$ICR[US$SPFifty7]
  US$PretaxCODFifty <-  (as.numeric(Bond$Spread[US$SPFifty7]) + USRF)
  US$InterestFifty <- US$DebtFifty*US$PretaxCODFifty
  US$PreTaxIntCovFifty <- US$EBIT/US$InterestFifty

  US$SPFifty8 <- ifelse(US$PreTaxIntCovFifty< US$SPVFifty,as.numeric(US$SPFifty7)+1,as.numeric(US$SPFifty7))
  US$SPVFifty <- Bond$ICR[US$SPFifty8]
  US$PretaxCODFifty <-  (as.numeric(Bond$Spread[US$SPFifty8]) + USRF)
  US$InterestFifty <- US$DebtFifty*US$PretaxCODFifty
  US$PreTaxIntCovFifty <- US$EBIT/US$InterestFifty

  US$SPFifty9 <- ifelse(US$PreTaxIntCovFifty< US$SPVFifty,as.numeric(US$SPFifty8)+1,as.numeric(US$SPFifty8))
  US$SPVFifty <- Bond$ICR[US$SPFifty9]
  US$PretaxCODFifty <-  (as.numeric(Bond$Spread[US$SPFifty9]) + USRF)
  US$InterestFifty <- US$DebtFifty*US$PretaxCODFifty
  US$PreTaxIntCovFifty <- US$EBIT/US$InterestFifty

  ifelse(US$SPFifty<=14,US$SPFifty9,14)
  US$SPVFifty <- Bond$ICR[US$SPFifty9]

  if(US$SPVFifty >= MinRatingV)
  {
  US$TaxableIncomeFifty <- as.numeric(US$EBIT)-US$InterestFifty
  US$PretaxCODFifty <- as.numeric(Bond$Spread[US$SPFifty9]) + USRF
  US$CODFifty <- as.numeric(US$PretaxCODFifty)*(1-as.numeric(US$TaxR))
  US$TRFifty <- ifelse(US$InterestFifty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestFifty )
  US$BetaFifty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRFifty))*US$DbyEFifty)))
  US$COEFifty <- (as.numeric(US$BetaFifty)*as.numeric(USRP))+as.numeric(USRF)
  US$WACCFifty <- (as.numeric(US$CODFifty)*as.numeric(US$DbyTFifty))+(as.numeric(US$COEFifty)*(1-as.numeric(US$DbyTFifty)))
  US$InterestFifty <- as.numeric(as.numeric(Bond$Spread[US$SPFifty9])+USRF)/(1-US$TaxR)*US$DebtFifty
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
  US$InterestSxty <- as.numeric(Bond$Spread[US$SPFifty9]+USRF)/(1-US$TaxR)*US$DebtSxty
  US$TaxableIncomeSxty <- as.numeric(US$EBIT)-US$InterestSxty
  US$TaxSxty <- US$TaxableIncomeSxty*US$TaxR
  US$NetIncomeSxty <-US$TaxableIncomeSxty-US$TaxSxty
  US$NetOpIncomeSxty <-as.numeric(US$NetIncomeSxty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovSxty <- US$EBIT/US$InterestSxty
  US$TRSxty <- ifelse(US$InterestSxty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestSxty )
  US$BetaSxty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRSxty))*US$DbyESxty)))
  US$COESxty <- (as.numeric(US$BetaSxty)*as.numeric(USRP))+as.numeric(USRF)


  US$SPSxty <- 0
  US$SPSxty <- ifelse(US$PreTaxIntCovSxty< US$SPVFifty,as.numeric(US$SPFifty9)+1,as.numeric(US$SPFifty9))
  US$SPVSxty <- Bond$ICR[US$SPSxty]
  US$PretaxCODSxty <- as.numeric(Bond$Spread[US$SPSxty]) + USRF
  US$InterestSxty <- US$DebtSxty*US$PretaxCODSxty
  US$PreTaxIntCovSxty <- US$EBIT/US$InterestSxty



  US$SPSxty1 <- ifelse(US$PreTaxIntCovSxty< US$SPVSxty,as.numeric(US$SPSxty)+1,as.numeric(US$SPSxty))
  US$SPVSxty <- Bond$ICR[US$SPSxty1]
  US$PretaxCODSxty <- (as.numeric(Bond$Spread[US$SPSxty1]) + USRF)
  US$InterestSxty <- US$DebtSxty*US$PretaxCODSxty
  US$PreTaxIntCovSxty <- US$EBIT/US$InterestSxty

  US$SPSxty2 <- ifelse(US$PreTaxIntCovSxty< US$SPVSxty,as.numeric(US$SPSxty1)+1,as.numeric(US$SPSxty1))
  US$SPVSxty <- Bond$ICR[US$SPSxty2]
  US$PretaxCODSxty <- (as.numeric(Bond$Spread[US$SPSxty2]) + USRF)
  US$InterestSxty <- US$DebtSxty*US$PretaxCODSxty
  US$PreTaxIntCovSxty <- US$EBIT/US$InterestSxty


  US$SPSxty3 <- ifelse(US$PreTaxIntCovSxty< US$SPVSxty,as.numeric(US$SPSxty2)+1,as.numeric(US$SPSxty2))
  US$SPVSxty <- Bond$ICR[US$SPSxty3]
  US$PretaxCODSxty <- (as.numeric(Bond$Spread[US$SPSxty3]) + USRF)
  US$InterestSxty <- US$DebtSxty*US$PretaxCODSxty
  US$PreTaxIntCovSxty <- US$EBIT/US$InterestSxty

  US$SPSxty4 <- ifelse(US$PreTaxIntCovSxty< US$SPVSxty,as.numeric(US$SPSxty3)+1,as.numeric(US$SPSxty3))
  US$SPVSxty <- Bond$ICR[US$SPSxty4]
  US$PretaxCODSxty <- (as.numeric(Bond$Spread[US$SPSxty4]) + USRF)
  US$InterestSxty <- US$DebtSxty*US$PretaxCODSxty
  US$PreTaxIntCovSxty <- US$EBIT/US$InterestSxty

  US$SPSxty5 <- ifelse(US$PreTaxIntCovSxty< US$SPVSxty,as.numeric(US$SPSxty4)+1,as.numeric(US$SPSxty4))
  US$SPVSxty <- Bond$ICR[US$SPSxty5]
  US$PretaxCODSxty <- (as.numeric(Bond$Spread[US$SPSxty5]) + USRF)
  US$InterestSxty <-  US$DebtSxty*US$PretaxCODSxty
  US$PreTaxIntCovSxty <- US$EBIT/US$InterestSxty


  US$SPSxty6 <- ifelse(US$PreTaxIntCovSxty< US$SPVSxty,as.numeric(US$SPSxty5)+1,as.numeric(US$SPSxty5))
  US$SPVSxty <- Bond$ICR[US$SPSxty6]
  US$PretaxCODSxty <- (as.numeric(Bond$Spread[US$SPSxty6]) + USRF)
  US$InterestSxty <-  US$DebtSxty*US$PretaxCODSxty
  US$PreTaxIntCovSxty <- US$EBIT/US$InterestSxty


  US$SPSxty7 <- ifelse(US$PreTaxIntCovSxty< US$SPVSxty,as.numeric(US$SPSxty6)+1,as.numeric(US$SPSxty6))
  US$SPVSxty <- Bond$ICR[US$SPSxty7]
  US$PretaxCODSxty <-  (as.numeric(Bond$Spread[US$SPSxty7]) + USRF)
  US$InterestSxty <- US$DebtSxty*US$PretaxCODSxty
  US$PreTaxIntCovSxty <- US$EBIT/US$InterestSxty

  US$SPSxty8 <- ifelse(US$PreTaxIntCovSxty< US$SPVSxty,as.numeric(US$SPSxty7)+1,as.numeric(US$SPSxty7))
  US$SPVSxty <- Bond$ICR[US$SPSxty8]
  US$PretaxCODSxty <-  (as.numeric(Bond$Spread[US$SPSxty8]) + USRF)
  US$InterestSxty <- US$DebtSxty*US$PretaxCODSxty
  US$PreTaxIntCovSxty <- US$EBIT/US$InterestSxty

  US$SPSxty9 <- ifelse(US$PreTaxIntCovSxty< US$SPVSxty,as.numeric(US$SPSxty8)+1,as.numeric(US$SPSxty8))
  US$SPVSxty <- Bond$ICR[US$SPSxty9]
  US$PretaxCODSxty <-  (as.numeric(Bond$Spread[US$SPSxty9]) + USRF)
  US$InterestSxty <- US$DebtSxty*US$PretaxCODSxty
  US$PreTaxIntCovSxty <- US$EBIT/US$InterestSxty

  ifelse(US$SPSxty<=14,US$SPSxty9,14)
  US$SPVSxty <- Bond$ICR[US$SPSxty9]

  if(US$SPVSxty >= MinRatingV)
  {

  US$TaxableIncomeSxty <- as.numeric(US$EBIT)-US$InterestSxty
  US$PretaxCODSxty <- as.numeric(Bond$Spread[US$SPSxty9]) + USRF
  US$CODSxty <- as.numeric(US$PretaxCODSxty)*(1-as.numeric(US$TaxR))
  US$TRSxty <- ifelse(US$InterestSxty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestSxty )
  US$BetaSxty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRSxty))*US$DbyESxty)))
  US$COESxty <- (as.numeric(US$BetaSxty)*as.numeric(USRP))+as.numeric(USRF)
  US$WACCSxty <- (as.numeric(US$CODSxty)*as.numeric(US$DbyTSxty))+(as.numeric(US$COESxty)*(1-as.numeric(US$DbyTSxty)))
  US$InterestSxty <- as.numeric(as.numeric(Bond$Spread[US$SPSxty9])+USRF)/(1-US$TaxR)*US$DebtSxty
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
  US$InterestSvnty <- as.numeric(Bond$Spread[US$SPSxty9]+USRF)/(1-US$TaxR)*US$DebtSvnty
  US$TaxableIncomeSvnty <- as.numeric(US$EBIT)-US$InterestSvnty
  US$TaxSvnty <- US$TaxableIncomeSvnty*US$TaxR
  US$NetIncomeSvnty <-US$TaxableIncomeSvnty-US$TaxSvnty
  US$NetOpIncomeSvnty <-as.numeric(US$NetIncomeSvnty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovSvnty <- US$EBIT/US$InterestSvnty
  US$TRSvnty <- ifelse(US$InterestSvnty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestSvnty )
  US$BetaSvnty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRSvnty))*US$DbyESvnty)))
  US$COESvnty <- (as.numeric(US$BetaSvnty)*as.numeric(USRP))+as.numeric(USRF)


  US$SPSvnty <- 0
  US$SPSvnty <- ifelse(US$PreTaxIntCovSvnty< US$SPVSxty,as.numeric(US$SPSxty9)+1,as.numeric(US$SPSxty9))
  US$SPVSvnty <- Bond$ICR[US$SPSvnty]
  US$PretaxCODSvnty <- as.numeric(Bond$Spread[US$SPSvnty]) + USRF
  US$InterestSvnty <- US$DebtSvnty*US$PretaxCODSvnty
  US$PreTaxIntCovSvnty <- US$EBIT/US$InterestSvnty



  US$SPSvnty1 <- ifelse(US$PreTaxIntCovSvnty< US$SPVSxty,as.numeric(US$SPSvnty)+1,as.numeric(US$SPSvnty))
  US$SPVSvnty <- Bond$ICR[US$SPSvnty1]
  US$PretaxCODSvnty <- (as.numeric(Bond$Spread[US$SPSvnty1]) + USRF)
  US$InterestSvnty <- US$DebtSvnty*US$PretaxCODSvnty
  US$PreTaxIntCovSvnty <- US$EBIT/US$InterestSvnty

  US$SPSvnty2 <- ifelse(US$PreTaxIntCovSvnty< US$SPVSvnty,as.numeric(US$SPSvnty1)+1,as.numeric(US$SPSvnty1))
  US$SPVSvnty <- Bond$ICR[US$SPSvnty2]
  US$PretaxCODSvnty <- (as.numeric(Bond$Spread[US$SPSvnty2]) + USRF)
  US$InterestSvnty <- US$DebtSvnty*US$PretaxCODSvnty
  US$PreTaxIntCovSvnty <- US$EBIT/US$InterestSvnty


  US$SPSvnty3 <- ifelse(US$PreTaxIntCovSvnty< US$SPVSvnty,as.numeric(US$SPSvnty2)+1,as.numeric(US$SPSvnty2))
  US$SPVSvnty <- Bond$ICR[US$SPSvnty3]
  US$PretaxCODSvnty <- (as.numeric(Bond$Spread[US$SPSvnty3]) + USRF)
  US$InterestSvnty <- US$DebtSvnty*US$PretaxCODSvnty
  US$PreTaxIntCovSvnty <- US$EBIT/US$InterestSvnty

  US$SPSvnty4 <- ifelse(US$PreTaxIntCovSvnty< US$SPVSvnty,as.numeric(US$SPSvnty3)+1,as.numeric(US$SPSvnty3))
  US$SPVSvnty <- Bond$ICR[US$SPSvnty4]
  US$PretaxCODSvnty <- (as.numeric(Bond$Spread[US$SPSvnty4]) + USRF)
  US$InterestSvnty <- US$DebtSvnty*US$PretaxCODSvnty
  US$PreTaxIntCovSvnty <- US$EBIT/US$InterestSvnty

  US$SPSvnty5 <- ifelse(US$PreTaxIntCovSvnty< US$SPVSvnty,as.numeric(US$SPSvnty4)+1,as.numeric(US$SPSvnty4))
  US$SPVSvnty <- Bond$ICR[US$SPSvnty5]
  US$PretaxCODSvnty <- (as.numeric(Bond$Spread[US$SPSvnty5]) + USRF)
  US$InterestSvnty <-  US$DebtSvnty*US$PretaxCODSvnty
  US$PreTaxIntCovSvnty <- US$EBIT/US$InterestSvnty


  US$SPSvnty6 <- ifelse(US$PreTaxIntCovSvnty< US$SPVSvnty,as.numeric(US$SPSvnty5)+1,as.numeric(US$SPSvnty5))
  US$SPVSvnty <- Bond$ICR[US$SPSvnty6]
  US$PretaxCODSvnty <- (as.numeric(Bond$Spread[US$SPSvnty6]) + USRF)
  US$InterestSvnty <-  US$DebtSvnty*US$PretaxCODSvnty
  US$PreTaxIntCovSvnty <- US$EBIT/US$InterestSvnty


  US$SPSvnty7 <- ifelse(US$PreTaxIntCovSvnty< US$SPVSvnty,as.numeric(US$SPSvnty6)+1,as.numeric(US$SPSvnty6))
  US$SPVSvnty <- Bond$ICR[US$SPSvnty7]
  US$PretaxCODSvnty <-  (as.numeric(Bond$Spread[US$SPSvnty7]) + USRF)
  US$InterestSvnty <- US$DebtSvnty*US$PretaxCODSvnty
  US$PreTaxIntCovSvnty <- US$EBIT/US$InterestSvnty

  US$SPSvnty8 <- ifelse(US$PreTaxIntCovSvnty< US$SPVSvnty,as.numeric(US$SPSvnty7)+1,as.numeric(US$SPSvnty7))
  US$SPVSvnty <- Bond$ICR[US$SPSvnty8]
  US$PretaxCODSvnty <-  (as.numeric(Bond$Spread[US$SPSvnty8]) + USRF)
  US$InterestSvnty <- US$DebtSvnty*US$PretaxCODSvnty
  US$PreTaxIntCovSvnty <- US$EBIT/US$InterestSvnty

  US$SPSvnty9 <- ifelse(US$PreTaxIntCovSvnty< US$SPVSvnty,as.numeric(US$SPSvnty8)+1,as.numeric(US$SPSvnty8))
  US$SPVSvnty <- Bond$ICR[US$SPSvnty9]
  US$PretaxCODSvnty <-  (as.numeric(Bond$Spread[US$SPSvnty9]) + USRF)
  US$InterestSvnty <- US$DebtSvnty*US$PretaxCODSvnty
  US$PreTaxIntCovSvnty <- US$EBIT/US$InterestSvnty

  ifelse(US$SPSvnty<=14,US$SPSvnty9,14)
  US$SPVSvnty <- Bond$ICR[US$SPSvnty9]

  if(US$SPVSvnty >= MinRatingV)
  {
  US$TaxableIncomeSvnty <- as.numeric(US$EBIT)-US$InterestSvnty
  US$PretaxCODSvnty <- as.numeric(Bond$Spread[US$SPSvnty9]) + USRF
  US$CODSvnty <- as.numeric(US$PretaxCODSvnty)*(1-as.numeric(US$TaxR))
  US$TRSvnty <- ifelse(US$InterestSvnty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestSvnty )
  US$BetaSvnty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRSvnty))*US$DbyESvnty)))
  US$COESvnty <- (as.numeric(US$BetaSvnty)*as.numeric(USRP))+as.numeric(USRF)
  US$WACCSvnty <- (as.numeric(US$CODSvnty)*as.numeric(US$DbyTSvnty))+(as.numeric(US$COESvnty)*(1-as.numeric(US$DbyTSvnty)))
  US$InterestSvnty <- as.numeric(as.numeric(Bond$Spread[US$SPSvnty9])+USRF)/(1-US$TaxR)*US$DebtSvnty
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
  US$InterestEgty <- as.numeric(Bond$Spread[US$SPSvnty9]+USRF)/(1-US$TaxR)*US$DebtEgty
  US$TaxableIncomeEgty <- as.numeric(US$EBIT)-US$InterestEgty
  US$TaxEgty <- US$TaxableIncomeEgty*US$TaxR
  US$NetIncomeEgty <-US$TaxableIncomeEgty-US$TaxEgty
  US$NetOpIncomeEgty <-as.numeric(US$NetIncomeEgty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovEgty <- US$EBIT/US$InterestEgty
  US$TREgty <- ifelse(US$InterestEgty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestEgty )
  US$BetaEgty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TREgty))*US$DbyEEgty)))
  US$COEEgty <- (as.numeric(US$BetaEgty)*as.numeric(USRP))+as.numeric(USRF)


  US$SPEgty <- 0
  US$SPEgty <- ifelse(US$PreTaxIntCovEgty< US$SPVSvnty,as.numeric(US$SPSvnty9)+1,as.numeric(US$SPSvnty9))
  US$SPVEgty <- Bond$ICR[US$SPEgty]
  US$PretaxCODEgty <- as.numeric(Bond$Spread[US$SPEgty]) + USRF
  US$InterestEgty <- US$DebtSvnty*US$PretaxCODEgty
  US$PreTaxIntCovEgty <- US$EBIT/US$InterestEgty



  US$SPEgty1 <- ifelse(US$PreTaxIntCovEgty< US$SPVEgty,as.numeric(US$SPEgty)+1,as.numeric(US$SPEgty))
  US$SPVEgty <- Bond$ICR[US$SPEgty1]
  US$PretaxCODEgty <- (as.numeric(Bond$Spread[US$SPEgty1]) + USRF)
  US$InterestEgty <- US$DebtEgty*US$PretaxCODEgty
  US$PreTaxIntCovEgty <- US$EBIT/US$InterestEgty

  US$SPEgty2 <- ifelse(US$PreTaxIntCovEgty< US$SPVEgty,as.numeric(US$SPEgty1)+1,as.numeric(US$SPEgty1))
  US$SPVEgty <- Bond$ICR[US$SPEgty2]
  US$PretaxCODEgty <- (as.numeric(Bond$Spread[US$SPEgty2]) + USRF)
  US$InterestEgty <- US$DebtEgty*US$PretaxCODEgty
  US$PreTaxIntCovEgty <- US$EBIT/US$InterestEgty


  US$SPEgty3 <- ifelse(US$PreTaxIntCovEgty< US$SPVEgty,as.numeric(US$SPEgty2)+1,as.numeric(US$SPEgty2))
  US$SPVEgty <- Bond$ICR[US$SPEgty3]
  US$PretaxCODEgty <- (as.numeric(Bond$Spread[US$SPEgty3]) + USRF)
  US$InterestEgty <- US$DebtEgty*US$PretaxCODEgty
  US$PreTaxIntCovEgty <- US$EBIT/US$InterestEgty

  US$SPEgty4 <- ifelse(US$PreTaxIntCovEgty< US$SPVEgty,as.numeric(US$SPEgty3)+1,as.numeric(US$SPEgty3))
  US$SPVEgty <- Bond$ICR[US$SPEgty4]
  US$PretaxCODEgty <- (as.numeric(Bond$Spread[US$SPEgty4]) + USRF)
  US$InterestEgty <- US$DebtEgty*US$PretaxCODEgty
  US$PreTaxIntCovEgty <- US$EBIT/US$InterestEgty

  US$SPEgty5 <- ifelse(US$PreTaxIntCovEgty< US$SPVEgty,as.numeric(US$SPEgty4)+1,as.numeric(US$SPEgty4))
  US$SPVEgty <- Bond$ICR[US$SPEgty5]
  US$PretaxCODEgty <- (as.numeric(Bond$Spread[US$SPEgty5]) + USRF)
  US$InterestEgty <-  US$DebtEgty*US$PretaxCODEgty
  US$PreTaxIntCovEgty <- US$EBIT/US$InterestEgty


  US$SPEgty6 <- ifelse(US$PreTaxIntCovEgty< US$SPVEgty,as.numeric(US$SPEgty5)+1,as.numeric(US$SPEgty5))
  US$SPVEgty <- Bond$ICR[US$SPEgty6]
  US$PretaxCODEgty <- (as.numeric(Bond$Spread[US$SPEgty6]) + USRF)
  US$InterestEgty <-  US$DebtEgty*US$PretaxCODEgty
  US$PreTaxIntCovEgty <- US$EBIT/US$InterestEgty


  US$SPEgty7 <- ifelse(US$PreTaxIntCovEgty< US$SPVEgty,as.numeric(US$SPEgty6)+1,as.numeric(US$SPEgty6))
  US$SPVEgty <- Bond$ICR[US$SPEgty7]
  US$PretaxCODEgty <-  (as.numeric(Bond$Spread[US$SPEgty7]) + USRF)
  US$InterestEgty <- US$DebtEgty*US$PretaxCODEgty
  US$PreTaxIntCovEgty <- US$EBIT/US$InterestEgty

  US$SPEgty8 <- ifelse(US$PreTaxIntCovEgty< US$SPVEgty,as.numeric(US$SPEgty7)+1,as.numeric(US$SPEgty7))
  US$SPVEgty <- Bond$ICR[US$SPEgty8]
  US$PretaxCODEgty <-  (as.numeric(Bond$Spread[US$SPEgty8]) + USRF)
  US$InterestEgty <- US$DebtEgty*US$PretaxCODEgty
  US$PreTaxIntCovEgty <- US$EBIT/US$InterestEgty

  US$SPEgty9 <- ifelse(US$PreTaxIntCovEgty< US$SPVEgty,as.numeric(US$SPEgty8)+1,as.numeric(US$SPEgty8))
  US$SPVEgty <- Bond$ICR[US$SPEgty9]
  US$PretaxCODEgty <-  (as.numeric(Bond$Spread[US$SPEgty9]) + USRF)
  US$InterestEgty <- US$DebtEgty*US$PretaxCODEgty
  US$PreTaxIntCovEgty <- US$EBIT/US$InterestEgty

  ifelse(US$SPEgty<=14,US$SPEgty9,14)
  US$SPVEgty <- Bond$ICR[US$SPEgty9]

  if(US$SPVEgty >= MinRatingV)
  {

  US$TaxableIncomeEgty <- as.numeric(US$EBIT)-US$InterestEgty
  US$PretaxCODEgty <- as.numeric(Bond$Spread[US$SPEgty9]) + USRF
  US$CODEgty <- as.numeric(US$PretaxCODEgty)*(1-as.numeric(US$TaxR))
  US$TREgty <- ifelse(US$InterestEgty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestEgty )
  US$BetaEgty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TREgty))*US$DbyEEgty)))
  US$COEEgty <- (as.numeric(US$BetaEgty)*as.numeric(USRP))+as.numeric(USRF)
  US$WACCEgty <- (as.numeric(US$CODEgty)*as.numeric(US$DbyTEgty))+(as.numeric(US$COEEgty)*(1-as.numeric(US$DbyTEgty)))
  US$InterestEgty <- as.numeric(as.numeric(Bond$Spread[US$SPEgty9])+USRF)/(1-US$TaxR)*US$DebtEgty
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
  US$InterestNinty <- as.numeric(Bond$Spread[US$SPEgty9]+USRF)/(1-US$TaxR)*US$DebtNinty
  US$TaxableIncomeNinty <- as.numeric(US$EBIT)-US$InterestNinty
  US$TaxNinty <- US$TaxableIncomeNinty*US$TaxR
  US$NetIncomeNinty <-US$TaxableIncomeNinty-US$TaxNinty
  US$NetOpIncomeNinty <-as.numeric(US$NetIncomeNinty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovNinty <- US$EBIT/US$InterestNinty
  US$TRNinty <- ifelse(US$InterestNinty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestNinty )
  US$BetaNinty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRNinty))*US$DbyENinty)))
  US$COENinty <- (as.numeric(US$BetaNinty)*as.numeric(USRP))+as.numeric(USRF)


  US$SPNinty <- 0
  US$SPNinty <- ifelse(US$PreTaxIntCovNinty< US$SPVEgty,as.numeric(US$SPEgty9)+1,as.numeric(US$SPEgty9))
  US$SPVNinty <- Bond$ICR[US$SPNinty]
  US$PretaxCODNinty <- as.numeric(Bond$Spread[US$SPNinty]) + USRF
  US$InterestNinty <- US$DebtNinty*US$PretaxCODNinty
  US$PreTaxIntCovNinty <- US$EBIT/US$InterestNinty



  US$SPNinty1 <- ifelse(US$PreTaxIntCovNinty< US$SPVNinty,as.numeric(US$SPNinty)+1,as.numeric(US$SPNinty))
  US$SPVNinty <- Bond$ICR[US$SPNinty1]
  US$PretaxCODNinty <- (as.numeric(Bond$Spread[US$SPNinty1]) + USRF)
  US$InterestNinty <- US$DebtNinty*US$PretaxCODNinty
  US$PreTaxIntCovNinty <- US$EBIT/US$InterestNinty

  US$SPNinty2 <- ifelse(US$PreTaxIntCovNinty< US$SPVNinty,as.numeric(US$SPNinty1)+1,as.numeric(US$SPNinty1))
  US$SPVNinty <- Bond$ICR[US$SPNinty2]
  US$PretaxCODNinty <- (as.numeric(Bond$Spread[US$SPNinty2]) + USRF)
  US$InterestNinty <- US$DebtNinty*US$PretaxCODNinty
  US$PreTaxIntCovNinty <- US$EBIT/US$InterestNinty


  US$SPNinty3 <- ifelse(US$PreTaxIntCovNinty< US$SPVNinty,as.numeric(US$SPNinty2)+1,as.numeric(US$SPNinty2))
  US$SPVNinty <- Bond$ICR[US$SPNinty3]
  US$PretaxCODNinty <- (as.numeric(Bond$Spread[US$SPNinty3]) + USRF)
  US$InterestNinty <- US$DebtNinty*US$PretaxCODNinty
  US$PreTaxIntCovNinty <- US$EBIT/US$InterestNinty

  US$SPNinty4 <- ifelse(US$PreTaxIntCovNinty< US$SPVNinty,as.numeric(US$SPNinty3)+1,as.numeric(US$SPNinty3))
  US$SPVNinty <- Bond$ICR[US$SPNinty4]
  US$PretaxCODNinty <- (as.numeric(Bond$Spread[US$SPNinty4]) + USRF)
  US$InterestNinty <- US$DebtNinty*US$PretaxCODNinty
  US$PreTaxIntCovNinty <- US$EBIT/US$InterestNinty

  US$SPNinty5 <- ifelse(US$PreTaxIntCovNinty< US$SPVNinty,as.numeric(US$SPNinty4)+1,as.numeric(US$SPNinty4))
  US$SPVNinty <- Bond$ICR[US$SPNinty5]
  US$PretaxCODNinty <- (as.numeric(Bond$Spread[US$SPNinty5]) + USRF)
  US$InterestNinty <-  US$DebtNinty*US$PretaxCODNinty
  US$PreTaxIntCovNinty <- US$EBIT/US$InterestNinty


  US$SPNinty6 <- ifelse(US$PreTaxIntCovNinty< US$SPVNinty,as.numeric(US$SPNinty5)+1,as.numeric(US$SPNinty5))
  US$SPVNinty <- Bond$ICR[US$SPNinty6]
  US$PretaxCODNinty <- (as.numeric(Bond$Spread[US$SPNinty6]) + USRF)
  US$InterestNinty <-  US$DebtNinty*US$PretaxCODNinty
  US$PreTaxIntCovNinty <- US$EBIT/US$InterestNinty


  US$SPNinty7 <- ifelse(US$PreTaxIntCovNinty< US$SPVNinty,as.numeric(US$SPNinty6)+1,as.numeric(US$SPNinty6))
  US$SPVNinty <- Bond$ICR[US$SPNinty7]
  US$PretaxCODNinty <-  (as.numeric(Bond$Spread[US$SPNinty7]) + USRF)
  US$InterestNinty <- US$DebtNinty*US$PretaxCODNinty
  US$PreTaxIntCovNinty <- US$EBIT/US$InterestNinty

  US$SPNinty8 <- ifelse(US$PreTaxIntCovNinty< US$SPVNinty,as.numeric(US$SPNinty7)+1,as.numeric(US$SPNinty7))
  US$SPVNinty <- Bond$ICR[US$SPNinty8]
  US$PretaxCODNinty <-  (as.numeric(Bond$Spread[US$SPNinty8]) + USRF)
  US$InterestNinty <- US$DebtNinty*US$PretaxCODNinty
  US$PreTaxIntCovNinty <- US$EBIT/US$InterestNinty

  US$SPNinty9 <- ifelse(US$PreTaxIntCovNinty< US$SPVNinty,as.numeric(US$SPNinty8)+1,as.numeric(US$SPNinty8))
  US$SPVNinty <- Bond$ICR[US$SPNinty9]
  US$PretaxCODNinty <-  (as.numeric(Bond$Spread[US$SPNinty9]) + USRF)
  US$InterestNinty <- US$DebtNinty*US$PretaxCODNinty
  US$PreTaxIntCovNinty <- US$EBIT/US$InterestNinty

  ifelse(US$SPNinty<=14,US$SPNinty9,14)
  US$SPVNinty <- Bond$ICR[US$SPNinty9]

  if(US$SPVNinty >= MinRatingV)
  {
  US$TaxableIncomeNinty <- as.numeric(US$EBIT)-US$InterestNinty
  US$PretaxCODNinty <- as.numeric(Bond$Spread[US$SPNinty9]) + USRF
  US$CODNinty <- as.numeric(US$PretaxCODNinty)*(1-as.numeric(US$TaxR))
  US$TRNinty <- ifelse(US$InterestNinty<US$EBIT,US$TaxR, US$TaxR*US$EBIT/US$InterestNinty )
  US$BetaNinty <- as.numeric(US$BetaZero)*(1+(((1-as.numeric(US$TRNinty))*US$DbyENinty)))
  US$COENinty <- (as.numeric(US$BetaNinty)*as.numeric(USRP))+as.numeric(USRF)
  US$WACCNinty <- (as.numeric(US$CODNinty)*as.numeric(US$DbyTNinty))+(as.numeric(US$COENinty)*(1-as.numeric(US$DbyTNinty)))
  US$InterestNinty <- as.numeric(as.numeric(Bond$Spread[US$SPNinty9])+USRF)/(1-US$TaxR)*US$DebtNinty
  US$TaxNinty <- US$TaxableIncomeNinty*US$TaxR
  US$PreTaxIntCovNinty <- US$EBIT/US$InterestNinty
  US$TaxRABNinty <- ifelse(US$InterestNinty<US$EBIT, US$TaxR,as.numeric(US$TaxR)*as.numeric(US$EBIT)/as.numeric(US$InterestNinty))
  US$NetIncomeNinty <-US$TaxableIncomeNinty-US$TaxNinty
  US$NetOpIncomeNinty <-as.numeric(US$NetIncomeNinty)+as.numeric(US$EBITDA) -as.numeric(US$EBIT)
  US$PreTaxIntCovNinty <- US$EBIT/US$InterestNinty
  US$ValueNinty <- US$EV*(1+((US$WACC-US$WACCNinty)/(US$WACCNinty-USRF)))
  }
  }
  }
  }
  }
  }
  }
  }
  }


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


