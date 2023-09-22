############################################
# Shape generator for KRM modelling
############################################
  rm(list=ls())
  pacman::p_load(dplyr,zoo)
  dir <- "F:/Dagaa_xray_FEB_2022"
  setwd(dir)

  for(i in 9:24){
  raw <- read_excel("raw xray dimensions.xlsx",sheet = i) %>% as.data.frame()

###############################################
###############    body shape construction
###############################################

  # set position of head tip
  raw$scale <- raw$scale[1]
  raw$offset <- raw$lateral1 - raw$lateral1[1] + 1
  
  # up coordinates minus lower coordi
  xmean <- rollmean(raw$offset,2)
  yu <- (head(raw$lateral2,-1) - raw$lateral2[-1])/2
  yl <- -yu
  w <- (head(raw$dorsal2,-1) - raw$dorsal2[-1])
  
  # select odd/even rows only
  raw2 <- cbind(xmean,yu,yl,w) %>% as.data.frame()
  xmean2 <- raw2 %>% filter(row_number() %% 2 == 1) %>% select(c(1))
  xmean2 <- xmean2*raw$scale
  
  yu2 <- raw2 %>% filter(row_number() %% 2 == 1) %>% select(c(2))
  yu2 <- yu2*raw$scale
  yl2 <- -yu2
  w2 <-  raw2 %>% filter(row_number() %% 2 == 1) %>% select(c(4))
  w2 <- w2*raw$scale
  # 3 point triangle average
  xmean3 <- xmean2
  
  yu3 <- rollmean(yu2,k = 3,fill = NA)  # 3 point average
  yu3[1] <- head(yu2,1) ; yu3[length(yu3)] <- tail(yu2,1)   # head=head tail=tail
  yu3 <- data.frame(Reduce(rbind, yu3))
  
  yl3 <- rollmean(yl2,k = 3,fill = NA)  # 3 point average
  yl3[1] <- head(yl2,1) ; yl3[length(yl3)] <- tail(yl2,1)   # head=head tail=tail
  yl3 <- data.frame(Reduce(rbind, yl3))
  
  w3 <- rollmean(w2,k = 3,fill = NA)  # 3 point average
  w3[1] <- head(w2,1) ; w3[length(w3)] <- tail(w2,1)   # head=head tail=tail
  w3 <- data.frame(Reduce(rbind, w3))
  
  bshape <- cbind(xmean3,yu3,yl3,w3)
  colnames(bshape) <- c('x_fb',	'z_fbU','z_fbL', 'w_fb')
  rm(xmean,yu,yl,w,xmean2,yu2,yl2,w2,xmean3,yu3,yl3,w3,raw2)

###############################################
#### swimbladder anterior shape construction
###############################################
  raw <- read_excel("raw xray dimensions.xlsx",sheet = i) %>% as.data.frame()
  raw <- raw %>% tidyr::drop_na(lateral3)
  raw$scale <- raw$scale[1]
  raw$offset <- raw$lateral3 - raw$lateral1[1] + 1
  
  # up coordinates minus lower coordi
  xmean <- as.data.frame(rollmean(raw$offset,2))
  yz <-  as.data.frame(raw$lateral4 - raw$`lateral mid offset`[1])  #minus lateral middle reference line

  w <-  as.data.frame((head(raw$dorsal4,-1) - raw$dorsal4[-1]))
  
  # select odd/even rows only
  raw2 <- gdata::cbindX(xmean,yz,w) %>% as.data.frame()
  
  xmean2 <- raw2 %>% filter(row_number() %% 2 == 1) %>% select(c(1)) # even row
  xmean2 <- xmean2*raw$scale
  
  yu2 <- raw2 %>% filter(row_number() %% 2 == 1) %>% select(c(2))
  yu2 <- yu2*raw$scale
  yl2 <- raw2 %>% filter(row_number() %% 2 == 0) %>% select(c(2))
  yl2 <- yl2*raw$scale
  w2 <-  raw2 %>% filter(row_number() %% 2 == 1) %>% select(c(3))
  w2 <- w2*raw$scale
  
  # 3 point triangle average
  xmean3 <- xmean2
  
  yu3 <- rollmean(yu2,k = 3,fill = NA)  # 3 point average
  yu3[1] <- head(yu2,1) ; yu3[length(yu3)] <- tail(yu2,1)   # head=head tail=tail
  yu3 <- data.frame(Reduce(rbind, yu3))
  
  yl3 <- rollmean(yl2,k = 3,fill = NA)  # 3 point average
  yl3[1] <- head(yl2,1) ; yl3[length(yl3)] <- tail(yl2,1)   # head=head tail=tail
  yl3 <- data.frame(Reduce(rbind, yl3))
  
  w3 <- rollmean(w2,k = 3,fill = NA)  # 3 point average
  w3[1] <- head(w2,1) ; w3[length(w3)] <- tail(w2,1)   # head=head tail=tail
  w3 <- data.frame(Reduce(rbind, w3))
  
  sb1shape <- cbind(xmean3,yu3*0.6,yl3*0.6,w3*0.6)
  rm(xmean,yu,yl,w,xmean2,yu2,yl2,w2,xmean3,yu3,yl3,w3,raw2)
  
###############################################
#### swimbladder posterior shape construction
###############################################
  raw <- read_excel("raw xray dimensions.xlsx",sheet = i) %>% as.data.frame()
  raw <- raw %>% tidyr::drop_na(lateral5)
  raw$scale <- raw$scale[1]
  raw$offset <- raw$lateral5 - raw$lateral1[1] + 1
  
  # up coordinates minus lower coordi
  xmean <- as.data.frame(rollmean(raw$offset,2))
  yz <-  as.data.frame(raw$lateral6 - raw$`lateral mid offset`[1])  #minus lateral middle reference line
  
  w <-  as.data.frame((head(raw$dorsal6,-1) - raw$dorsal6[-1]))
  
  # select odd/even rows only
  raw2 <- gdata::cbindX(xmean,yz,w) %>% as.data.frame()
  
  xmean2 <- raw2 %>% filter(row_number() %% 2 == 1) %>% select(c(1)) # even row
  xmean2 <- xmean2*raw$scale
  
  yu2 <- raw2 %>% filter(row_number() %% 2 == 1) %>% select(c(2))
  yu2 <- yu2*raw$scale
  yl2 <- raw2 %>% filter(row_number() %% 2 == 0) %>% select(c(2))
  yl2 <- yl2*raw$scale
  w2 <-  raw2 %>% filter(row_number() %% 2 == 1) %>% select(c(3))
  w2 <- w2*raw$scale
  
  # 3-point triangle average
  xmean3 <- xmean2
  
  yu3 <- rollmean(yu2,k = 3,fill = NA)  # 3 point average
  yu3[1] <- head(yu2,1) ; yu3[length(yu3)] <- tail(yu2,1)   # head=head tail=tail
  yu3 <- data.frame(Reduce(rbind, yu3))
  
  yl3 <- rollmean(yl2,k = 3,fill = NA)  # 3 point average
  yl3[1] <- head(yl2,1) ; yl3[length(yl3)] <- tail(yl2,1)   # head=head tail=tail
  yl3 <- data.frame(Reduce(rbind, yl3))
  
  w3 <- rollmean(w2,k = 3,fill = NA)  # 3 point average
  w3[1] <- head(w2,1) ; w3[length(w3)] <- tail(w2,1)   # head=head tail=tail
  w3 <- data.frame(Reduce(rbind, w3))
  
  sb2shape <- cbind(xmean3,yu3*0.6,yl3*0.6,w3*0.6)

# combine body and swimbladders
  
  shape <- gdata::cbindX(bshape, sb1shape, sb2shape) 
  colnames(shape) <- c('x_fb',	'z_fbU','z_fbL', 'w_fb','x_sb', 'z_sbU', 'z_sbL','w_sb','x_sb2','z_sbU2','z_sbL2','w_sb2'
)
  write.csv(shape,file = paste0("shape meta_scaled_",i,".csv"),na = '',row.names=FALSE)
}

# plot the shape of single fish
list <-  list.files("F:/Dagaa_xray_FEB_2022",pattern = '.csv')
for(i in 1:19){
  shape <- read.csv(list[i],header = T)
  layout(matrix(c(1,2), 2, 2, byrow = TRUE))
  print(shplot_yang(x_fb = shape$x_fb, w_fb = shape$w_fb,
              x_sb = shape$x_sb, w_sb = shape$w_sb,
              z_fbU = shape$z_fbU, z_fbL = shape$z_fbL,
              z_sbU = shape$z_sbU, z_sbL = shape$z_sbL,
              x_sb2 = shape$x_sb2, w_sb2 = shape$w_sb2,
              z_sbU2 = shape$z_sbU2, z_sbL2 = shape$z_sbL2))
}

