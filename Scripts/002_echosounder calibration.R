# calibartion curve plot
  library(ggplot2)
  dir <- ('F:\\Dagaa_survey_FEB_2022\\analysis_KRM\\')
  setwd(dir)

# load raw data
  df <- read.csv('calibration value.csv', header = T) %>% select(c(1,2,3)) %>% slice(c(1:772))
  df2 <- read.csv('calibration value.csv', header = T) %>% select(c(1,2,3)) %>% slice(c(773:1131))

# generate plot and save as .png

  p <- ggplot() + 
  	geom_line(data=df, aes(x=Frequency, y=Gain, group = Type, linetype= Type), size=1) + 
  	theme_bw() + 
  	geom_line(data=df2, aes(x=Frequency, y=Gain*40, group = Type, linetype= Type),size=1) +
  	scale_y_continuous(name = expression('Gain (dB)'),limits = c(0, 40), 
                       sec.axis = sec_axis(~./40, name='RMS error (dB)')) + 
   	theme(legend.title =element_blank(),
          legend.position=c(0.2,0.85),
          legend.background=element_rect(fill="transparent",colour=NA),
          axis.text.x = element_text(size=20),
          title=element_text(size=20),
          axis.text.y=element_text(size=20),
          legend.text=element_text(size=15))

# save plot 
  ggsave(".png", plot = p, width = 250,height =200 ,units = "mm" ,dpi = 300)


