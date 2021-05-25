load(paste(psypath, "pws.RData", sep = "/"))
source(paste(psypath, "psychrometric.R", sep = "/"))

lb_cn <- labs(x = expression(paste("含湿量/", "g·k",g[干空气]^-1)),
              y = expression(paste("空气温度/", degree, "C")))
lb_en <- labs(x = expression(paste("Humidity ratio/", " g·k",g[dry~air]^-1)),
              y = expression(paste("Air temperature/", degree, "C")))

cal.y = function(h, d){
  y <- h/1.006 - scale.factor*d
  return(y)
}
cal.d_y = function(ta, y){
  d <- (y-ta)/(0.001*(2501+1.86*ta)/1.006-scale.factor)
  return(d)
}
interpol <- function(x,y,x0){
  tmp <- x[x<=x0]
  n <- length(tmp)
  x1 <- x[n]
  x2 <- x[n+1]
  y1 <- y[n]
  y2 <- y[n+1]
  y0 <- y1 + (y2-y1)/(x2-x1)*(x0-x1)
  return(y0)
}


draw_psy <- function(t.min=0, t.max=40, d.max=30, B=101325, linesize = 1, scale.factor = 2.45){
  calRHline <- function(df){
    df <- mutate(df,
                 d = cal.d_Pa(Pw,B),
                 h = cal.h_Ta.d(Ta,d),
                 y = cal.y(h,d)
                 )
    return(df)
  }
  Satline <- calRHline(Satline)
  RHline.10 <- calRHline(RHline.10)
  RHline.5 <- calRHline(RHline.5)
  Satline_draw <- subset(Satline, y<=t.max&d<=d.max&y>=t.min)
  RHline.10_draw <- subset(RHline.10, y<=t.max&d<=d.max&y>=t.min)
  RHline.5_draw <- subset(RHline.5, y<=t.max&d<=d.max&y>=t.min)
  
  dline1 <- data.frame()
  dline5 <- data.frame()
  
  for(i in 1:(d.max-1)){
    d <- c(i,i)
    y <- c(t.max, max(0,interpol(Satline$d, Satline$y, i)))
    if(y[2]>t.max)break
    df <- data.frame(d, y)
    if(i%%5==0){
      dline5 <- rbind(dline5,df)
    }else{
      dline1 <- rbind(dline1,df)
    }
  }
  d <- c(d.max, d.max)
  y <- c(t.max, min(t.max,max(0,interpol(Satline$d, Satline$y, d.max))))
  dline0 <- data.frame(d, y)
  
  
  Tline <- subset(Satline, Ta%%1==0&y<=t.max&d<=d.max&y>=t.min)
  Tline <- data.frame(Ta=Tline$Ta, y=Tline$y, d=Tline$d)
  Tline1 <- subset(Tline, Ta>=t.min)
  Tline2 <- subset(Tline, Ta<t.min)
  
  df <- data.frame(Ta=Tline1$Ta, y=Tline1$Ta, d=0*Tline1$y)
  tline <- rbind(Tline1,df)
  tmax <- max(tline$Ta)
  if(nrow(Tline2)>0){
    tmin <- min(Tline2$Ta)
  }else{
    tmin <- min(Tline1$Ta)
  }
  
  
  if(tmax<t.max){
    t1 <- (tmax+1):t.max
    n <- length(t1)
    h1 <- cal.h_Ta.d(t1,d.max)
    y1 <- cal.y(h1,d.max)
    t11 <- t1[y1<=t.max]
    y11 <- y1[y1<=t.max]
    n1 <- length(t11)
    df1 <- data.frame(Ta=t11, d=rep(d.max,n1), y=y11)
    t12 <- t1[y1>t.max]
    y2 <- rep(t.max, n-n1)
    d2 <- cal.d_y(t12,y2)
    df2 <- data.frame(Ta=t12, d=d2, y=y2)
    df3 <- data.frame(Ta=t1, d=0*t1, y=t1)
    tline <- rbind(tline,df1,df2,df3)
  }
  if(tmin<t.min){
    t1 <- tmin:(t.min-1)
    n <- length(t1)
    y1 <- rep(t.min,n)
    d1 <- cal.d_y(t1,y1)
    df1 <- data.frame(Ta=t1, d=d1, y=y1)
    df2 <- data.frame(Ta=Tline2$Ta, d=Tline2$d, y=Tline2$y)
    tline <- rbind(tline, df1, df2)
  }
  
  tline1 <- subset(tline, Ta%%5!=0)
  tline5 <- subset(tline, Ta%%5==0)
  
  y <- c(t.min,t.min)
  d <- c(0, interpol(Satline$y, Satline$d, t.min))
  l0 <- data.frame(y,d)
  
  
  g <- ggplot()+
    geom_line(data = RHline.10_draw, aes(d,y,group = RH), size = linesize)+
    geom_line(data = RHline.5_draw, aes(d,y,group = RH), colour = "grey", linetype="dashed", size = linesize) +
    geom_line(data = Satline_draw, aes(d,y)) +
    geom_line(data = dline5, aes(x=d,y=y,group = d), size = linesize) +
    geom_line(data = dline1, aes(x=d,y=y,group = d), colour = "grey", linetype="dashed", size = linesize) +
    geom_line(data = tline5, aes(x=d,y=y,group = Ta), size = linesize) +
    geom_line(data = tline1, aes(x=d,y=y,group = Ta), colour = "grey", linetype="dashed", size = linesize) +
    geom_line(data = l0, aes(x=d,y=y)) +
    geom_line(data = dline0, aes(x=d,y=y)) +
    scale_x_continuous(breaks = seq(0,d.max,5), limits = c(0,d.max), position = "top", expand = c(0,0))+
    scale_y_continuous(breaks = seq(t.min,t.max,5), limits = c(t.min,t.max), expand = c(0,0)) +
    theme(
      panel.background = element_blank(),
      axis.line.x = element_line(colour = "black"),
      axis.line.y = element_line(colour = "black")
    )
  return(g)
}