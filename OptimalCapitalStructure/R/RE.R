Reinvestment <- function(...){
  output<- data.frame(...)

c <- sqldf('select CompName,PR, WACCNew, DBV, ROIC, EQO, EVNew from output')
c$ROIC <- c$ROIC/100
c$WACCNew <- as.numeric(unlist(c$WACCNew))
c$WACCCOMP <- c$WACCNew+0.04
c$ROICN <- c$ROIC-ROICRI



c$v1 <- c$DBV*c$ROICN/(1+c$WACCNew)^1
c$v2 <- c$DBV*c$ROICN/(1+c$WACCNew)^2
c$v3 <- c$DBV*c$ROICN/(1+c$WACCNew)^3
c$v4 <- c$DBV*c$ROICN/(1+c$WACCNew)^4
c$v5 <- c$DBV*c$ROICN/(1+c$WACCNew)^5
c$v6 <- c$DBV*c$ROICN/(1+c$WACCNew)^6
c$v7 <- c$DBV*c$ROICN/(1+c$WACCNew)^7
c$v8 <- c$DBV*c$ROICN/(1+c$WACCNew)^8
c$v9 <- c$DBV*c$ROICN/(1+c$WACCNew)^9
c$v10 <- c$DBV*c$ROICN/(1+c$WACCNew)^10


c$IV10 <- c$DBV/(1+c$WACCNew)^10
c$IV5 <- c$DBV/(1+c$WACCNew)^5
c$IV6 <- c$DBV/(1+c$WACCNew)^6
c$IV7 <- c$DBV/(1+c$WACCNew)^7
c$IV8 <- c$DBV/(1+c$WACCNew)^8
c$IV9 <- c$DBV/(1+c$WACCNew)^9
c$IV1 <- c$DBV/(1+c$WACCNew)^1
c$IV2 <- c$DBV/(1+c$WACCNew)^2
c$IV3 <- c$DBV/(1+c$WACCNew)^3
c$IV4 <- c$DBV/(1+c$WACCNew)^4


c$TV10 <- c$IV10 +c$v10+c$v9+c$v8+c$v7+c$v6+c$v5+c$v4+c$v3+c$v2+c$v1
c$TV5<- c$IV5+c$v5+c$v4+c$v3+c$v2+c$v1
c$TV1 <- c$v1+c$IV1
c$TV2 <- c$v2+c$v1+c$IV2
c$TV3 <- c$v3+c$v2+c$v1+c$IV3
c$TV4 <- c$v4+c$v3+c$v2+c$v1+ c$IV4
c$TV6 <- c$v6+c$v5+c$v4+c$v3+c$v2+c$v1+c$IV6
c$TV7 <- c$v7+c$v6+c$v5+c$v4+c$v3+c$v2+c$v1+c$IV7
c$TV8 <- c$v8+c$v7+c$v6+c$v5+c$v4+c$v3+c$v2+c$v1+c$IV8
c$TV9 <- c$v9+c$v8+c$v7+c$v6+c$v5+c$v4+c$v3+c$v2+c$v1+c$IV9




c$EQD1 <- c$TV1/c$EQO
c$EQD2 <- c$TV2/c$EQO
c$EQD3 <- c$TV3/c$EQO
c$EQD4 <- c$TV4/c$EQO
c$EQD5 <- c$TV5/c$EQO
c$EQD6 <- c$TV6/c$EQO
c$EQD7 <- c$TV7/c$EQO
c$EQD8 <- c$TV8/c$EQO
c$EQD9 <- c$TV9/c$EQO
c$EQD10 <- c$TV10/c$EQO

F1 <- data.frame(companyName =c$CompName)
F1$Equityvalue0 <- c$EQO
F1$Equityvalue1 <- c$TV1+c$EQO
F1$Equityvalue2 <- c$TV2+c$EQO
F1$Equityvalue3 <- c$TV3+c$EQO
F1$Equityvalue4 <- c$TV4+c$EQO
F1$Equityvalue5 <- c$TV5+c$EQO
F1$Equityvalue6 <- c$TV6+c$EQO
F1$Equityvalue7 <- c$TV7+c$EQO
F1$Equityvalue8 <- c$TV8+c$EQO
F1$Equityvalue9 <- c$TV9+c$EQO
F1$Equityvalue10 <- c$TV10+c$EQO
return(F1)

}
