#Geoff Downes 27Oct2021
#Called by chunk "intervalComparison" by "ThreeResi_SilviScanAnalysis.Rmd"

#The purpose here is to prepare the segment mean data from the 20mm segments for comparison between the SS data and the data from the three resi instruments

ssSegmentMeans <- function(int,ssData,rNames,maxSeg) {

  ssSegmentMeans <- data.table()   #Create the variable

  #### Prepare the resTrace density data
  tNames <- substr(rNames,5,8)
  Company <- substr(rNames,1,3)
  sNames <- names(ssData)
  ssSamples <- substr(sNames,4,7)

  #Generate the SilviScan interval means
  for (i in 1:length(sNames))  {
    ss   <- (ssData[i][[1]])
    x    <- length(ss$Den)
    l    <- min(maxSeg-1,round(floor(x/(int*4)),0))

    j <- 1

    #Create an array the length of the SS profile where each element indicates the segment that point belongs to.
    interval <- NULL
    for (j in 1:l) {
      a <- integer((int*4))+j  #x4 to address the resolution difference in the SS data (25 micron)
      interval <- c(interval,a)
    }

    ss$Intervals <- integer(length(x))
    ss$Intervals[x:(x-length(interval))] <- (interval)
    ss$Intervals[which(ss$Intervals==0)] <- l+1

    #Generate the interval m  eans
    segmentMeans <- ss[,.(Posn = mean(Posn)
                      ,Den = mean(Den)#*adjSS   #0.8 to approximate basic density
                      ,MFA = mean(MFA)
                      ,MOE = mean(MOE)
                      ,Wall = mean(Wall)
                      ,Crs  = mean(Crs)
                      ,DiffIntens = mean(DiffIntens)
                      ,RayAngle = mean(RayAngle)
                      )
                      ,by = "Intervals"
                    ]
#browser()
    #names(ssIntMeans)
    segmentMeans$SampleName    <- ssSamples[i]
    segmentMeans$SegmentLength <- int/10
    segmentMeans <- segmentMeans[order(-Posn),]

    #Combine into a single data table
    ssSegmentMeans <- rbind.data.frame(ssSegmentMeans,segmentMeans)


  }

  #Remove intervals where SS rayangle is GT 20
  a <- which(abs(ssSegmentMeans$RayAngle) > 20)
  ssSegmentMeans   <- ssSegmentMeans[!a]

  return(ssSegmentMeans)
}

  ####################### END SO FAR

resiSegmentMeans <- function(resiData) {

  for (i in 1:length(sNames))  {
    #for (i in 1:2)  {
    #print(i);print(tNames[i])

    x    <- rev(which(resTraces$FileName == tNames[i]))
    resi <- (resTraces[x])
    l <- round(floor(length(x)/int),0)
    pos <- NULLssIntervalMeans

    for (j in 1:l) {
      a <- integer(int)+j
      pos <- c(pos,a)
    }

    resi$Intervals  <- integer(length(x))
    resi$Intervals[1:length(pos)] <- pos
    resi$Intervals[which(resi$Intervals==0)] = l+1

    resiIntMeans <- resi[,.(
      Posn = mean(Position)
      ,Den = mean(resTraces)
    )
    ,by = "Intervals"]

    resiIntMeans <- resiIntMeans[order(-Posn),]



    #merge into a common data set
    DT <- merge(ssIntMeans,resiIntMeans, by = "Intervals")

    intervalMeans <- rbind(intervalMeans, DT)
    #print(paste("end loop: f = ", f))
    rm(ss, resi, ssIntMeans, resiIntMeans)
    rm(pos, l, x)


  }


 return(returnData)
}




ssOWMeans <- function(ssData,int) {
  #browser()
  sList <- names(ssData)
  owDenSS <- numeric()
  for (i in 1:length(sList)) {
    den <- rev(ssData[[i]]$Den)
    #plot(den)
    owd <- mean(den[1:2000])   #50 mm x 40 or 50 x 1/0.025
    #convert to basic density
    # FPInnovations intercept was 0.22
    owDenSS[i] <- round(1000 * owd / (1080+int*owd),0)#
    #owDen$Filename[i] = sList[i]



  }
  #browser()
  returnData <- data.table(cbind(sList,owDenSS))
  #names(owDen) <- sList
  return(returnData)

}
