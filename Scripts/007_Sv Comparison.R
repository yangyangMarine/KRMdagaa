# school simulation compare to measured school
  
  pacman::p_load(KRMr,ggplot2, readxl,truncnorm,dplyr,reshape2,colorRamps, gridExtra,ggsci,readr)
  dir <- c("C:\\Users\\yangy\\OneDrive - University of St Andrews\\PhD papers\\chapter7 dagaa TS model\\R scripts\\")
  setwd(dir)
  source(paste0(dir, "krm.dagaa.sim.R"))
  source(paste0(dir, "krm.dagaa.R"))

  dir <- ('E:\\Dagaa_survey_FEB_2022\\analysis_KRM\\')
  setwd(dir)
    # observed dagaa
    schoolFR <- read.csv("manual selected schools_WBFR_dagaa3.csv", header = F) %>% t() %>% 
      as.data.frame() 
    colnames(schoolFR) <- schoolFR[c(1),]
    schoolFR  <- schoolFR[-c(1,nrow(schoolFR),nrow(schoolFR)-1),] %>% 
      as.data.frame()
    chars <- sapply(schoolFR, is.character)
    
    #convert all character columns to numeric
    schoolFR[ , chars] <- as.data.frame(apply(schoolFR[ , chars], 2, as.numeric))
    schoolFR <- schoolFR %>%
      select(c(16:231)) 

    # school SV
    df <- 10^(schoolFR/10)
    df_mean <- apply(df,2,FUN = mean) %>% as.data.frame()
    df_ci <- apply(as.matrix(df), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))}) %>% as.data.frame() %>% t()
    dagaa = cbind(10*log10(df_mean), 10*log10(df_ci))
    dagaa$f = c(45:260)
    colnames(dagaa) = c("rf","up","low","f")
    glimpse(dagaa)
    dagaa[c(1:8,40:55,115:125,206:216),c(1:3,5)] = NA
    dagaa$group = c(rep("Dagaa",216))
    dagaa = dagaa[,-5]

    # relative school frequency response r(f)
    dfr <- schoolFR - schoolFR[,26]
    dfr_mean <- apply(dfr,2,FUN = mean) %>% as.data.frame()
    dfr_ci <- apply(as.matrix(dfr), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))}) %>% as.data.frame() %>% t()
    dagaar = cbind(dfr_mean,dfr_ci)
    dagaar$f = c(45:260)
    colnames(dagaar) = c("rf","up","low","f")
    glimpse(dagaar)
    dagaar[c(1:8,40:55,115:125,206:216),c(1:3,5)] = NA
    dagaar$group = c(rep("Dagaa",216))
    dagaar = dagaar[,-5]
    
########################
######## simulation ####
########################
    dir <- c("C:\\Users\\yangy\\OneDrive - University of St Andrews\\PhD papers\\chapter7 dagaa TS model\\results")
    setwd(dir)
    df <- read_xlsx("length distribution trawl 30.xlsx", sheet=1)
    # length_class = hist(df$SL, breaks = 40) 
    # L = length_class$mids/100
    # L_prop = length_class$counts/123
    
    dir <- c("C:\\Users\\yangy\\OneDrive - University of St Andrews\\PhD papers\\chapter7 dagaa TS model\\raw shapefile\\")
    setwd(dir)
    list <-  list.files(dir, pattern = 'scaled')
    set.seed(123)
    theta = rnorm(n = 500, mean = 93.1, sd = 13.1)
    L = rnorm(n = 500, mean = 0.0328, sd = 0.00475)
    simulation = 0
    
    for ( j in 1:16) {
      
      meas <- read.csv(list[j],header = T)
      meas = meas/100
      #L = (max(meas$x_fb) - min(meas$x_fb))
      
      rsl=0
      
      for(i in 1:500){
        TS = krm.dagaa.sim(frequency = c(35:260) * 1000,
                          c.w = 1493,
                          rho.w = 1000,
                          theta= theta[i], #seq(65,115),
                          c.fb = 1575,
                          c.sb = 345,
                          rho.sb = 2.64,
                          rho.fb = 1080,
                          L= L[i],
                          x_fb = na.omit(meas$x_fb),
                          x_sb = na.omit(meas$x_sb),
                          w_fb = na.omit(meas$w_fb),
                          w_sb = na.omit(meas$w_sb),
                          z_fbU = na.omit(meas$z_fbU),
                          z_fbL = na.omit(meas$z_fbL),
                          z_sbU = na.omit(meas$z_sbU),
                          z_sbL = na.omit(meas$z_sbL),
                          x_sb2 = na.omit(meas$x_sb),
                          w_sb2 = na.omit(meas$w_sb),
                          z_sbU2 = na.omit(meas$z_sbU),
                          z_sbL2 = na.omit(meas$z_sbL))
        
        rsl = cbind(rsl, sqrt(10^(TS$TS/10))*0.002)
      }
      
      rslm <- rsl[,-c(1)] # remove empty length class bin
      rslm <- 10*log10(apply(rslm,1,sum)^2) %>% as.data.frame()
      simulation = cbind(simulation, rslm)
      print(j)
    }
 
    #  relative response    
    simulation1 = simulation[,-c(1)]
  
    # simulation1 = t(t(simulation1)/t(simulation1)[,52])
    simulation1 = mapply('-', simulation1, simulation1[52,])
    
    simu_mean_r <- apply(simulation1,1,FUN = mean) %>% as.data.frame()
    simu_ci_r <- apply(as.matrix(simulation1), 1, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))}) %>% as.data.frame() %>% t()
    
    simu_mean_r = simu_mean_r + 
    simu_ci_r = simu_ci_r + 2
    
    simu_r = cbind(simu_mean_r,simu_ci_r, c(35:260), c("KRM model"))
    colnames(simu_r) = c("rf","low","up","f","group")
    
    combine_r = rbind(dagaar, simu_r)
    
    rf <- ggplot(combine_r, aes(x = f, y=rf,  fill = group)) + 
      geom_line(size=1) +
      geom_ribbon(aes(ymin = low, ymax = up), 
                  alpha=0.6) +
      theme_bw() +
      ylim(-15,5) +
      xlim(0,260) +
      xlab("Frequency (kHz)") +
      ylab("r (f)") +
      geom_vline(xintercept = c(70,120,200), linetype="dotted", color = "black", size=1) +
      scale_fill_manual(values = c("#8B0000","#696969")) +
      theme(legend.title = element_blank(),
            legend.position=c(0.8,0.85),
            legend.background=element_rect(fill="white",linetype="solid", colour ="black"),
            axis.text.x = element_text(size=20),
            title=element_text(size=20),
            axis.text.y=element_text(size=20),
            legend.text=element_text(size=15)) 
  
    # backscattering frequency response 
    simulation2 = simulation[,-c(1)]
    simulation2 = 10^(simulation2/10)
    simu_mean <- apply(simulation2,1,FUN = mean) %>% as.data.frame()
    simu_ci <- apply(as.matrix(simulation2), 1, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))}) %>% as.data.frame() %>% t()
    
    simu = cbind(10*log10(simu_mean*75),10*log10(simu_ci*75), c(35:260), c("KRM model"))
    colnames(simu) = c("rf","low","up","f","group")
    
    combined = rbind(dagaa, simu)
    
    sv = ggplot(combined, aes(x = f, y=rf,  fill = group)) + 
      geom_line(size=1) +
      geom_ribbon(aes(ymin = low, ymax = up), 
                  alpha=0.6) +
      theme_bw() +
      xlim(0,260) +
      ylim(-50,-35) +
      xlab("Frequency (kHz)") +
      ylab("Sv (dB)") +
      scale_fill_manual(values = c("#8B0000","#696969")) +
      theme(legend.title = element_blank(),
            legend.position="none",
            legend.background=element_rect(fill="transparent"),
            axis.text.x = element_text(size=20),
            title=element_text(size=20),
            axis.text.y=element_text(size=20),
            legend.text=element_text(size=15)
            #legend.box.background = element_rect(colour = "black")) +
      geom_vline(xintercept = c(70,120,200), linetype="dotted", color = "black", size=1)
    
    grid.arrange(sv, rf, ncol=2)
    
    
    