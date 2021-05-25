cal.Pws_Ta = function(T_a){
  Ta <- T_a + 273.15
  c1 <- -5.6745359e3
  c2 <- 6.3925247
  c3 <- -9.677843e-3
  c4 <- 6.2215701e-7
  c5 <- 2.0747825e-9
  c6 <- -9.484024e-13
  c7 <- 4.1635019
  c8 <- -5.8002206e3
  c9 <- 1.3914993
  c10 <- -4.8640239e-2
  c11 <- 4.1764768e-5
  c12 <- -1.4452093e-8
  c13 <- 6.5459673
  Pws <- numeric(length = length(T_a))
  Pws[Ta < 273.15] <- exp(c1 / Ta + c2 + c3 * Ta + c4 * Ta ^ 2 + c5 * Ta ^ 3 + c6 * Ta ^ 4 + c7 * log(Ta))
  Pws[Ta >= 273.15] <- exp(c8 / Ta + c9 + c10 * Ta + c11 * Ta ^ 2 + c12 * Ta ^ 3 + c13 * log(Ta))
  return(Pws)
}
cal.Pa_Ta.RH = function(Ta, RH){
  Pa <- RH/100*cal.Pws_Ta(Ta)
  return(Pa)
}
cal.Pa_d = function(d,B=101325){
  Pa <- (B*d)/(d+621.945)
  return(Pa)
}
cal.d_Pa = function(Pa,B=101325){
  d <- 621.945*Pa/(B-Pa)
  return(d)
}
cal.d_Td = function(Td,B=101325){
  Pa <- cal.Pws_Ta(Td)
  d <- cal.d_Pa(Pa,B)
  return(d)
}
cal.d_Ta.h = function(Ta, h){
  d <- (h-1.006*Ta)/(2501+1.86*Ta)*1000
  return(d)
}
cal.d_Ta.RH = function(Ta,RH,B=101325){
  d <- cal.d_Pa(cal.Pa_Ta.RH(Ta, RH), B)
  return(d)
}
cal.h_Ta.d = function(Ta,d){
  h <- 1.006*Ta+0.001*d*(2501+1.86*Ta)
  return(h)
}