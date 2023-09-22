# Echoview automation for dagaa school detection, and wideband frequency response export
# under R version 3.5.0
# A predefined .EV file runs on Echoview 13.1 is needed
  rm(list=ls())

# install.packages("RDCOMClient", repos = "http://www.omegahat.org/R")
  library(RDCOMClient)
  library(beepr)

# loading .EV file and .raw files
  dir <- ('D:\\Dagaa_survey_FEB_2022\\echosounder\\FM_trawl all')
  setwd(dir)

  allfiles<-list.files(dir, pattern='.raw', recursive=T, full.names=T)
  
  # remove .idx and .raw.evi files
  eviLoc = grep('.idx', allfiles)
  allfiles <- allfiles[-eviLoc]

  eviLoc = grep('.raw.evi', allfiles)
  allfiles <- allfiles[-eviLoc]
  
  EVApp <- COMCreate('EchoviewCom.EvApplication')
  EVFile <- EVApp$OpenFile("D:\\Dagaa_survey_FEB_2022\\analysis_KRM\\STWB.EV")
  
  fileset1 <- EVFile[["Filesets"]]$FindByName('Fileset 1')
  
  for(n in allfiles){fileset1[["DataFiles"]]$Add(n)}
  
  # School detection
  ExpVariable <- EVFile[['Variables']]$FindByName("Processed data 120 WB water column")
  PingNo <- ExpVariable$MeasurementCount()
  ExpVariable$DetectSchools('Schools',1,PingNo)
  objVariable_school <- EVFile[['Variables']]$FindByName("VBS120")
  
  # EXPORT school integration at central frequency
  regionclass <- EVFile[['RegionClasses']]$FindByName("Schools")
  objVariable_school$ExportIntegrationByRegions(file.path("F:\\Dagaa_survey_FEB_2022\\analysis_KRM\\school integration.csv"), regionclass)
  
  # export echogram as image file
  imageV <- EVFile[['Variables']]$FindByName("Fileset 1: Sv pulse compressed wideband pings T1 (channel 3)")
  imageV$ExportEchogramToImage(file.path("D:\\Dagaa_survey_FEB_2022\\analysis_KRM\\echogramall.tiff"),500,1,16500)
  
  # Export school wideband frequency response
  objVariable2 <- EVFile[['Variables']]$FindByName("Fileset 1: TS pulse compressed wideband pings T1 (channel 3)") 
  objVariable2$ExportSingleTargetWidebandFrequencyResponseByRegions(file.path("F:\\Dagaa_survey_FEB_2022\\analysis_KRM\\tracks FR.csv"), T, F, 0.4, 0, 1.0, 1, objVariable2, regionclass1, 1)
  
  # Detect single fish tracks
  objVariable <- EVFile[['Variables']]$FindByName("STD120")
  PingNo <- objVariable$MeasurementCount()
  objVariable$DetectFishTracks("dagaa", 1, PingNo)

  # export fish tracks
  regionclass1 <- EVFile[['RegionClasses']]$FindByName("dagaa")
  objVariable$ExportFishTracksByRegions(file.path("F:\\Dagaa_survey_FEB_2022\\analysis_KRM\\tracks integration.csv"),regionclass1)

  EVApp$Quit()
