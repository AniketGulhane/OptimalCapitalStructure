Stat <- function(...){
  US<- data.frame(...)


  output <-data.frame(EVNew=ifelse(US$MaxEV>=US$EV,US$MaxEV,US$EV))
  output$WACCNew <-  ifelse(US$MaxEV==US$ValueZero,0,ifelse(US$MaxEV==US$ValueTen,US$WACCTen,ifelse(US$MaxEV==US$ValueTwty,US$WACCTwty,ifelse(US$MaxEV==US$ValueTrty,US$WACCTrty,ifelse(US$MaxEV==US$ValueFrty,US$WACCFrty,ifelse(US$MaxEV==US$ValueFifty,US$WACCFifty,ifelse(US$MaxEV==US$ValueSxty,US$WACCSxty,ifelse(US$MaxEV==US$ValueSvnty,US$WACCSvnty,ifelse(US$MaxEV==US$ValueEgty,US$WACCEgty,ifelse(US$MaxEV==US$ValueNinty,US$WACCNinty,US$WACC))))))))))
  output$WACCNew <- ifelse(output$WACCNew<output$WACC,output$WACCNew,output$WACC)
  output$CompName <- US$Company
  output$EV <-US$EV
  output$WACCOld <-US$WACC
  output$Debt <- ifelse(output$EVNew ==output$EV,0, US$OptimalD)

  output$ROIC <- as.numeric(unlist(sqldf('Select ROIC from US inner join output on US.Company = output.CompName')))
  Zero <-count(output$Debt == 0)
  Ten <-count(output$Debt == 10)
  Twty <-count(output$Debt == 20)
  Trty <-count(output$Debt == 30)
  Frty <-count(output$Debt == 40)
  Fifty <-count(output$Debt == 50)
  Sxty <-count(output$Debt == 60)
  Svnty <-count(output$Debt == 70)
  Egty <-count(output$Debt == 80)
  Ninty <-count(output$Debt == 90)

  Zero[,1][is.na(Zero[,1])] <- "empty"
  Ten[,1][is.na(Ten[,1])] <- "empty"
  Twty[,1][is.na(Twty[,1])] <- "empty"
  Trty[,1][is.na(Trty[,1])] <- "empty"
  Frty[,1][is.na(Frty[,1])] <- "empty"
  Fifty[,1][is.na(Fifty[,1])] <- "empty"
  Sxty[,1][is.na(Sxty[,1])] <- "empty"
  Svnty[,1][is.na(Svnty[,1])] <- "empty"
  Egty[,1][is.na(Egty[,1])] <- "empty"
  Ninty[,1][is.na(Ninty[,1])] <- "empty"

  T <- data.frame(Freq = double(),Debt = double())
  T[nrow(T) + 1,] <- c(as.numeric(Zero$freq[Zero$x == "TRUE"]),"0")
  T[nrow(T) + 1,] <- c(as.numeric(Ten$freq[Ten$x == "TRUE"]),"10")
  T[nrow(T) + 1,] <- c(as.numeric(Twty$freq[Twty$x == "TRUE"]),"20")
  T[nrow(T) + 1,] <- c(as.numeric(Trty$freq[Trty$x == "TRUE"]),"30")
  T[nrow(T) + 1,] <- c(as.numeric(Frty$freq[Frty$x == "TRUE"]),"40")
  T[nrow(T) + 1,] <- c(as.numeric(Fifty$freq[Fifty$x == "TRUE"]),"50")
  T[nrow(T) + 1,] <- c(as.numeric(Sxty$freq[Sxty$x == "TRUE"]),"60")
  T[nrow(T) + 1,] <- c(as.numeric(Svnty$freq[Svnty$x == "TRUE"]),"70")
  T[nrow(T) + 1,] <- c(as.numeric(Egty$freq[Egty$x == "TRUE"]),"80")
  T[nrow(T) + 1,] <- c(as.numeric(Ninty$freq[Ninty$x == "TRUE"]),"90")


  typeof(T)

  colnames(T)[colnames(T)!="Debt"] <- "Count"
  p <- ggplot(T,aes(x=Debt,y=T$Count,binwidth=1000 ))+ geom_histogram(stat = 'identity', fill =T$Count)

  output$EVC <- ((as.numeric(output$EVNew)-as.numeric(output$EV))/output$EV)*100
  output$EQO <- US$MarketCap
  output$DBV <- ifelse(output$Debt==0,US$DebtZero,ifelse(output$Debt==10,US$DebtTen,ifelse(output$Debt==20,US$DebtTwty,ifelse(output$Debt==30,US$DebtTrty,ifelse(output$Debt==40,US$DebtFrty,ifelse(output$Debt==50,US$DebtFifty,ifelse(output$Debt==60,US$DebtSxty,ifelse(output$Debt==70,US$DebtSvnty,ifelse(output$Debt==80,US$DebtEgty,US$DebtNinty)))))))))
  output$EVR <- output$EVNew/output$EV
  output$LTD <- US$LTD

  output$PR<-as.numeric(unlist(sqldf('select DR.PR from DR inner join output on DR.CompName=output.CompName')))


  return(output)

}
