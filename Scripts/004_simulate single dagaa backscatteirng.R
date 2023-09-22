# KRM simulation
    pacman::p_load(KRMr,ggplot2, readxl,truncnorm,dplyr,reshape2,colorRamps,plotly)
    # source("F:/Dagaa_xray_FEB_2022/shpplot_yang.R")
    # list <-  list.files("F:/Dagaa_xray_FEB_2022",pattern = 'scaled')
    dir <- c("C:\\Users\\yangy\\OneDrive - University of St Andrews\\PhD papers\\chapter7 dagaa TS model\\R scripts\\")

    setwd(dir)
    source(paste0(dir, "krm.dagaa.R"))
    source(paste0(dir, "krm.dagaa.sim.R"))

###########################################################
################## single dagaa 
    dir <- c("C:\\Users\\yangy\\OneDrive - University of St Andrews\\PhD papers\\chapter7 dagaa TS model\\raw shapefile\\")
    setwd(dir)
    list <-  list.files(dir, pattern = 'scaled')

    meas <- read.csv(list[16])
    meas = meas/100
    L = (max(meas$x_fb) - min(meas$x_fb))

    # simulation
    theta.min = 65
    theta.max = 115
    #theta2 <- seq(theta.min,theta.max,1) + 8
    f.min = 38
    f.max = 260
    
    layout(matrix(c(1,2), 2, 2, byrow = TRUE))
    shplot_yang1(x_fb = meas$x_fb, w_fb = meas$w_fb,
                 x_sb = meas$x_sb, w_sb = meas$w_sb,
                 z_fbU = meas$z_fbU, z_fbL = meas$z_fbL,
                 z_sbU = meas$z_sbU, z_sbL = meas$z_sbL, 
                 x_sb2 = na.omit(meas$x_sb2),
                 w_sb2 = na.omit(meas$w_sb2),
                 z_sbU2 = na.omit(meas$z_sbU2),
                 z_sbL2 = na.omit(meas$z_sbL2))
    
    TS = krm.dagaa.sim(frequency = seq(f.min,f.max,1) * 1000,
                      c.w = 1493,
                      rho.w = 1000,
                      theta= seq(theta.min,theta.max,1),
                      c.fb = 1575,
                      c.sb = 345,
                      rho.sb = 2.64,
                      rho.fb = 1080,
                      L= L,
                      x_fb = na.omit(meas$x_fb),
                      x_sb = na.omit(meas$x_sb),
                      w_fb = na.omit(meas$w_fb),
                      w_sb = na.omit(meas$w_sb),
                      z_fbU = na.omit(meas$z_fbU),
                      z_fbL = na.omit(meas$z_fbL),
                      z_sbU = na.omit(meas$z_sbU),
                      z_sbL = na.omit(meas$z_sbL),
                      x_sb2 = na.omit(meas$x_sb2),
                      w_sb2 = na.omit(meas$w_sb2),
                      z_sbU2 = na.omit(meas$z_sbU2),
                      z_sbL2 = na.omit(meas$z_sbL2)
                      )
    
    #2D Plot function===
    # d cast long data to wide matrix
    head(TS)
    TS_wide <- dcast(TS[,c(1,2,5)], theta ~ frequency, value.var="TS") 
    TS_wide <- TS_wide[, -c(1)] %>% as.matrix()
    

    RB <- colorRampPalette(colors = c("darkblue","blue", "#008080", "#FFFF00", "#DC143C"),bias = 1)
    x <- seq(from = theta.min, to = theta.max, length.out = nrow(TS_wide))
    y <- seq(f.min, f.max, length.out =ncol(TS_wide)) # change band width range
    
    levels <- seq(-90,-47,1)

    dev.new()
    png(file="Figure 6.jpg",width=558, height=491)
    filled.contour(x = x, y = y, z = TS_wide, levels = levels, color.palette = RB, axes = T,frame.plot = T,
                   plot.title={title(xlab="Tilt angle  (degrees)",
                                     ylab="Frequency (kHz)",
                                     cex.lab=1.5)},
                   key.title=title((main="TS (dB)"),
                                    cex.main=1.2, line = 0.6, adj=0.1),
                   plot.axes = list(axis(1,seq(theta.min, theta.max, by = 10),
                                    cex.axis=1.5),
                          	    axis(2,seq(40, f.max,50),cex.axis=1.5)))
    dev.off()
  