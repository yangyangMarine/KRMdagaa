# Echogram plot
  library(ggplot2)
  library(dplyr)
  library(scico)
  library(lubridate)
  library(scales)
  library(RColorBrewer) # built own color palette
  library(readr)
  library(gridExtra)
  library(reshape2)
  setwd("D:\\Dagaa_survey_FEB_2022\\analysis_KRM")
 # data loading, raw data exported by Echoview-echogram-export-by regions by cells-cell size 0.1m*1ping
  
  #C build color palette
  cols<-c('white','grey80','#000099','#99FF99','#FFFF00','#FF9999','#B22222',"#990000")
  pal<-colorRampPalette(cols, bias = 1, space = "rgb", interpolate ="linear") #### increase bias - increase red
  image(x=1:20,y=1,z=as.matrix(1:20),col=pal(100))
  
  #
  df_70 <- read_csv("echogram_sv_70khz_cell.csv",col_names = T) %>% 
    dplyr::filter(Sv_mean >= -150, Sv_mean <= 0)  #### increase Sv_mean - increase blue
  df_70$Sv_mean[df_70$Sv_mean <= -100] = NA  
  df_70$Layer = df_70$Layer/10
  df_70$Interval = df_70$Interval -6824
  summary(df_70$Sv_mean)
  df_120 <- read_csv("echogram_sv_120khz_cell.csv",col_names = T) %>% 
    dplyr::filter(Sv_mean >= -150, Sv_mean <= -0) 
  df_120$Sv_mean[df_120$Sv_mean <= -100] = NA
  df_120$Layer = df_120$Layer/10
  df_120$Interval = df_120$Interval -6824
  
  df_200 <- read_csv("echogram_sv_200khz_cell.csv",col_names = T) %>% 
    dplyr::filter(Sv_mean >= -150, Sv_mean <= -00) 
  df_200$Sv_mean[df_200$Sv_mean <= -100] = NA
  df_200$Layer = df_200$Layer/10
  df_200$Interval = df_200$Interval -6824
  
  # Ping number as x axis  
  p70 <-  ggplot(df_70, aes(Interval, Layer)) +
    geom_raster(aes(fill = Sv_mean )) +
    scale_x_continuous(limits = c(min(df_70$Interval),max(df_70$Interval)), expand = c(0, 0)) +
    scale_y_reverse(limits = c(23, 2), expand = c(0,0)) + 
    theme_bw() + 
    scale_fill_gradientn(colors = pal(100),na.value = 'white',limits=c(-80, -18)) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
          legend.position = 'none',
          text = element_text(size=12),
          legend.background=element_blank(),
          legend.text=element_text(),
          axis.text.y=element_text(size=10),
          axis.text.x=element_text(size=10),
          axis.ticks.y=element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    # legend.title = element_text(angle = -90, hjust = 0.5, vjust = 0.5)) +
    xlab('') +
    ylab('Depth (m)') +
    labs(fill = expression('Sv (dB re 1'~m^-2~'/'~m^-3~')')) + 
    ggtitle("45-90 kHz")
  
    p120 <- ggplot(df_120, aes(Interval, Layer)) +
      geom_raster(aes(fill = Sv_mean )) +
      scale_x_continuous(limits = c(min(df_120$Interval),max(df_120$Interval)), expand = c(0, 0)) +
      scale_y_reverse(limits = c(23, 2), expand = c(0,0)) + 
      theme_bw() + 
      scale_fill_gradientn(colors = pal(100),na.value = 'white',limits=c(-80, -15)) + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            legend.position = 'none',
            text = element_text(size=12),
            legend.background=element_blank(),
            legend.text=element_text(),
            axis.text.y=element_text(size=10),
            axis.text.x=element_text(size=10),
            axis.ticks.y=element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      # legend.title = element_text(angle = -90, hjust = 0.5, vjust = 0.5)) +
      xlab('') +
      ylab('Depth (m)') +
      labs(fill = expression('Sv (dB re 1'~m^-2~'/'~m^-3~')')) + 
      ggtitle("90-160 kHz")
    
    p200 <- ggplot(df_200, aes(Interval, Layer)) +
      geom_raster(aes(fill = Sv_mean )) +
      scale_x_continuous(limits = c(min(df_200$Interval),max(df_200$Interval)), expand = c(0, 0)) +
      scale_y_reverse(limits = c(23, 2), expand = c(0,0)) + 
      theme_bw() + 
      scale_fill_gradientn(colors = pal(100),na.value = 'white',limits=c(-80, -18)) + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            legend.position = 'none',
            text = element_text(size=12),
            legend.background=element_blank(),
            legend.text=element_text(),
            axis.text.y=element_text(size=10),
            axis.text.x=element_text(size=10),
            axis.ticks.y=element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      # legend.title = element_text(angle = -90, hjust = 0.5, vjust = 0.5)) +
      xlab('Ping #') +
      ylab('Depth (m)') +
      labs(fill = expression('Sv (dB re 1'~m^-2~'/'~m^-3~')')) + 
      ggtitle("160-250 kHz")

    

    grid.arrange(p70,p120,p200,ncol=1)
      
    ggsave("site1_18.png",plot = gridp,width = 130, height =150 ,units = "mm" ,dpi = 600)  
    
    ggplot(df_200, aes(Interval, Layer)) +
      geom_raster(aes(fill = Sv_mean )) +
      scale_x_continuous(limits = c(min(df_200$Interval),max(df_200$Interval)), expand = c(0, 0)) +
      scale_y_reverse(limits = c(23, 2), expand = c(0,0)) + 
      theme_bw() + 
      scale_fill_gradientn(colors = pal(100),na.value = 'white',limits=c(-90, -20)) + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),

            text = element_text(size=12),
            legend.text=element_text(),
            legend.title =element_text(),
            axis.text.y=element_text(size=10),
            axis.text.x=element_text(size=10),
            axis.ticks.y=element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      # legend.title = element_text(angle = -90, hjust = 0.5, vjust = 0.5)) +
      xlab('Ping #') +
      ylab('Depth (m)') +
      labs(fill = expression('Sv (dB re 1'~m^-1~')')) + 
      ggtitle("160-250 kHz")
    