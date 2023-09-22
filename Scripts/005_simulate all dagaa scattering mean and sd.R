# KRM simulation
  pacman::p_load(KRMr,ggplot2, readxl,truncnorm,dplyr,reshape2,colorRamps,plotly,abind)

  dir <- c("C:\\Users\\yangy\\OneDrive - University of St Andrews\\PhD papers\\chapter7 dagaa TS model\\R scripts\\")
  setwd(dir)
  source(paste0(dir, "shpplot_yang.R"))
  source(paste0(dir, "krm.dagaa.R"))
  source(paste0(dir, "krm.dagaa.sim.R"))

  dir <- c("C:\\Users\\yangy\\OneDrive - University of St Andrews\\PhD papers\\chapter7 dagaa TS model\\raw shapefile\\")
  setwd(dir)
  list <-  list.files(dir, pattern = 'scaled')

for(i in 1:16){
  meas <- read.csv(list[i],header = T)
  layout(matrix(c(1,2), 2, 2, byrow = TRUE))
  shplot_yang(x_fb = meas$x_fb, w_fb = meas$w_fb,
              x_sb = meas$x_sb, w_sb = meas$w_sb,
              z_fbU = meas$z_fbU, z_fbL = meas$z_fbL,
              z_sbU = meas$z_sbU, z_sbL = meas$z_sbL,
              x_sb2 = meas$x_sb2, w_sb2 = meas$w_sb2,
              z_sbU2 = meas$z_sbU2, z_sbL2 = meas$z_sbL2)
  meas = meas/100
  L = (max(meas$x_fb) - min(meas$x_fb))
  meas = 0.04/L*meas  # scaled to 4 cm 

  # simulation
  theta.min = 65
  theta.max = 115
  #theta2 <- seq(theta.min,theta.max,1) + 6
  f.min = 45
  f.max = 260
  
  TS = krm.dagaa.sim(frequency =seq(f.min,f.max,1) * 1000,
                    c.w = 1485,
                    rho.w = 1000,
                    theta= seq(theta.min,theta.max,1), #seq(65,115),
                    #theta2 = theta2,
                    c.fb = 1575,
                    c.sb = 345,
                    rho.sb = 2.64,
                    rho.fb = 1080,
                    L= 0.04,
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
                    z_sbL2 = na.omit(meas$z_sbL2))
  
  TS <- TS %>%
    mutate(#z = 10^((TS - 20*log10(L))/20), # TS=10*log10(Lbs^2) , RSL = Lbs/L, Lbs = sqrt(10^(TS/10))
           z = sqrt(10^(TS/10))/L,
           x = theta,
           y = 0.04/(1485/frequency))
 
  TS_wide <- dcast(TS[,c(11,12,13)], x ~ y, value.var="z") 
  TS_wide <- TS_wide[, -c(1)] %>% as.matrix()
  
  assign(paste0("simulation_",i),TS_wide)
}

# get mean and deviation 
  TS_mean <- (simulation_1+simulation_2+simulation_3+simulation_4+simulation_5+simulation_6+simulation_7+simulation_8+simulation_9+simulation_10+simulation_11+simulation_12+simulation_13+simulation_14+simulation_15+simulation_16)/16

  m <- abind(simulation_1, simulation_2, simulation_3, simulation_4, simulation_5, simulation_6, simulation_7, simulation_8, simulation_9, simulation_10, simulation_11, simulation_12, simulation_13, simulation_14, simulation_15, simulation_16, along=3)

  TS_deviation <- apply(m, 1:2, sd)
  range(TS_deviation)

# greek letter unicode:  https://stackoverflow.com/questions/27690729/greek-letters-symbols-and-line-breaks-inside-a-ggplot-legend-label
# plotly
  dim(TS_mean)
  axx <- list(
  title = 'L/\u03BB',
  ticktext = list("0","1", "2", "3", "4","5","6","7"), 
  tickvals = list(0, 30, 60, 90, 120, 150, 180,206),
  tickmode = "array"
)

  axy <- list(
  title = "\u0398",
  ticktext = list("65","75", "85", "95", "105"), 
  tickvals = list(0, 11, 21, 31, 41),
  tickmode = "array"
)

  axz <- list(
  title = "RSL",
  nticks = 8,
  range = c(0, 0.1)
)
  summary(TS$L/(1493/TS$frequency))
  min(TS_wide)
  max(TS_wide)

  TS_deviation <- TS_deviation/100

  plot_ly(showscale = T, showlegend = TRUE) %>% 
       add_surface(z = ~ TS_mean, colors = c("darkblue","blue", "green","yellow", "red")) %>%
       add_trace(type = "surface", z=TS_deviation) %>%
       layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz,
              camera = list(eye = list(x = -3, y = 1.25, z = 1.25))),
              legend = list(x = 0.1, y = 1))

  htmlwidgets::saveWidget(as_widget(fig), "dagaa krm simulation.html")

# 2D plot
  colors <- matlab.like(400)
  RB <- colorRampPalette(colors = c("darkblue","blue", "#008080", "#FFFF00", "#DC143C"),bias = 1)

  x <- seq(from = theta.min, to = theta.max, length.out = nrow(TS_mean))
  y <- seq(1, 7, length.out = ncol(TS_mean)) # change band width range

  max(TS_mean)
  min(TS_mean)

  levels <- seq(0, 0.085, 0.003)
  png(file="Figure 7a.jpg",width=558, height=491)

  filled.contour(x = x, y = y, z = TS_mean, levels = levels, color.palette = RB, axes = T,
                 plot.title={title(xlab="Tilt angle (degrees)",ylab="L/\u03BB",cex.lab=1.5)},
                 key.title=title((main="RSL"),cex.main=1.2, line = 0.6, adj=0.1),
                 plot.axes = list(axis(1,seq(theta.min, theta.max, by = 10),
                                  cex.axis=1.5), 
                                  axis(2,seq(1, 7, 1),
                                  cex.axis=1.5)))
  dev.off()

# Deviation
  x <- seq(from = theta.min, to = theta.max, length.out = nrow(TS_deviation))
  y <- seq(1, 7, length.out = ncol(TS_deviation)) # change band width range

  max(TS_deviation)
  min(TS_deviation)

  levels <- seq(0, 0.017, 0.0005)
  png(file="Figure 7b.jpg",width=558, height=491)
  filled.contour(x = x, y = y, z = TS_deviation, levels = levels, color.palette = RB, axes = T,
               plot.title={title(xlab="Tilt angle (degrees)",ylab="L/\u03BB",cex.lab=1.5)},
               key.title=title((main="s.d."),cex.main=1.2, line = 0.6, adj=0.1),
               plot.axes = list(axis(1,seq(theta.min, theta.max, by = 10),
                                cex.axis=1.5), 
                                axis(2,seq(1, 7, 1),
                                cex.axis=1.5)))
  dev.off()