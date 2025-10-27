FQ_ggplot_grobs <- function(df, xLab, yLab, label, xmn, xmx, ymn, ymx, fit, addLegend, nm) {
    browser()
    rename(df[,1])(nm)
    fs <- 10 #fontsize
    #nm <- names(df)[1]
    nGrp    <- as.integer(length(levels(unlist(df[,1]))))
    # shapeCount <- nGrp
    x <- as.numeric(df$x)
    y <- as.numeric(df$y)
    sma <- (lmodel2(y~x))
    smaCoef <- round(as.numeric(sma$regression.results[3,2:3]), digits = 4)
    olsCoef <- round(as.numeric(sma$regression.results[1,2:3]), digits = 3)

    #To use the RMSEP and SEP returned by function the x values need to be in the same units as the y values
    ifelse(fit == TRUE, PrY <- x*smaCoef[2] + (smaCoef[1]), PrY <- x)
    desc <- round(FQRMSEP(PrY,y),3)
    grob1  = grobTree(textGrob(label, x=0.01,  y=0.95, hjust=0,gp=gpar(col="black"
                                                                       , fontsize=fs+8, fontface="bold")))
    grob2  = grobTree(textGrob(paste("SMA: y = ",smaCoef[2],"x +", round(smaCoef[1],3))
                               ,x=0.25, y=0.05, hjust=0,gp=gpar(col="black", fontsize=fs, col = "red")))
    grob2a = grobTree(textGrob(paste("OLS: y = ",olsCoef[2],"x +", round(olsCoef[1],3))
                               ,x=0.25,  y=0.25, hjust=0,gp=gpar(col="black", fontsize=fs, col = "blue")))

    grob5 = grobTree(textGrob(paste("r2 =", desc[3])
                              ,x=0.5,  y=0.2, hjust=0,gp=gpar(col="black", fontsize=fs, col="black")))
    ifelse(fit == FALSE, message <- paste("RMSEP = ",desc[4]),message<- "RMSEP = NA")
    grob3 = grobTree(textGrob(message,x=0.5,  y=0.15, hjust=0,gp=gpar(col="black", fontsize=fs, col="black")))
    grob4 = grobTree(textGrob(paste("SEP = ",desc[5])
                              ,x=0.5,  y=0.1, hjust=0,gp=gpar(col="black", fontsize=fs, col="black")))

    grobs <- list(grob.label=grob1, grob.sma=grob2, grob.ols = grob2a, grob.rmsep=grob3
                  ,grob.sep=grob4, grob.rsq=grob5, smaCoef = smaCoef, olsCoef=olsCoef)

return(grobs)
}


#GGplot for regression analysis
#
FQ_ggplot <- function(df, xLab, yLab, label, xmn, xmx, ymn, ymx, fit, addLegend, nm) {
            fs <- 10 #fontsize
            #browser()
            #rename(df$Group)(nm)
            #nm <- names(df)[1]
            nGrp    <- as.integer(length(levels(unlist(df[,1]))))
            #print(nGrp)
            # shapeCount <- nGrp
            x <- as.numeric(df$x)
            y <- as.numeric(df$y)
            sma <- (lmodel2(y~x))
            smaCoef <- round(as.numeric(sma$regression.results[3,2:3]), digits = 4)
            olsCoef <- round(as.numeric(sma$regression.results[1,2:3]), digits = 3)

            #To use the RMSEP and SEP returned by function the x values need to be in the same units as the y values
            ifelse(fit == TRUE, PrY <- x*smaCoef[2] + (smaCoef[1]), PrY <- x)
            desc <- round(FQRMSEP(PrY,y),3)

            #xmn <- floor(min(df$x));xmx <- ceiling(max(df$x))
            #ymn <- floor(min(df$y));ymx <- ceiling(max(df$y))
            grob1  = grobTree(textGrob(label, x=0.01,  y=0.95, hjust=0,gp=gpar(col="black"
                                                                               , fontsize=fs+6, fontface="bold")))
            grob2  = grobTree(textGrob(paste("SMA: y = ",smaCoef[2],"x +", round(smaCoef[1],3))
                                       ,x=0.25, y=0.05, hjust=0,gp=gpar(col="black", fontsize=fs, col = "red")))
            grob2a = grobTree(textGrob(paste("OLS: y = ",olsCoef[2],"x +", round(olsCoef[1],3))
                                       ,x=0.25,  y=0.25, hjust=0,gp=gpar(col="black", fontsize=fs, col = "blue")))
            #gtext1 = grobTree(textGrob("OLS", x=0.9,  y=0.6, hjust=0,gp=gpar(col="black", fontsize=fs, col = "blue")))
            #gtext2 = grobTree(textGrob("SMA", x=0.9,  y=0.95, hjust=0,gp=gpar(col="black", fontsize=fs, col = "red")))
            d3 = desc[3]
            grob5 = grobTree(textGrob(paste(expression(paste("",r^2, "="), d3))
                                      ,x=0.6,  y=0.2, hjust=0,gp=gpar(col="black", fontsize=fs, col="black")))
            ifelse(fit == FALSE, message <- paste("RMSEP = ",desc[4]),message<- "RMSEP = NA")
            grob3 = grobTree(textGrob(message,x=0.6,  y=0.1, hjust=0,gp=gpar(col="black", fontsize=fs, col="black")))
            grob4 = grobTree(textGrob(paste("SEP = ",desc[5])
                                      ,x=0.6,  y=0.15, hjust=0,gp=gpar(col="black", fontsize=fs, col="black")))

            #Setup slope and intercept for thw 1:1 line if x and y scales match ok
            #browser()
            if (xmn != ymn) {lty <- 0}else{lty <- 4}


            fig <- ggplot(data = df, aes(x,y, group = Group))+xlim(c(xmn,xmx))+ylim(c(ymn,ymx)) +
                xlab(xLab) +
                ylab(yLab) +
                theme_classic() +
                #geom_point(aes(x,y, color = Group, shape = Group), cex=1.25) +
                geom_point(aes(shape = Group, color = Group), cex=1.5) +
                geom_abline(intercept = 0, slope = 1, col = "black", linetype = lty) +
                #geom_abline(slope = olsCoef[2], intercept = olsCoef[1], color = "blue", lty = 3)+
                geom_abline(slope = smaCoef[2], intercept = smaCoef[1], color = "red", lty = 2)+
                #geom_abline(slope = avOlsCoef[2], intercept = avOlsCoef[1], color = "red")+
                scale_shape_manual(values=c(1:max(nGrp)))+
                #scale_color_manual(values=c("black","blue","red","green","orange", "grey", "maroon4", "seagreen3"
                #,"turquoise","olivedrab4","brown2", "aquamarine4","greenyellow", "mediumorchid3", ))+
                scale_color_manual(values=blue2green(nGrp))+
                annotation_custom(grob1) + annotation_custom(grob2) +
                annotation_custom(grob4) + annotation_custom(grob5) +
                theme(text = element_text(size=14),plot.margin = unit(c(0.75, 0.75, 0.1, 0.1), "cm"))

            if(fit == FALSE) fig <- fig +annotation_custom(grob3)


            fig <- fig+guides(colour = guide_legend(title=nm), shape = guide_legend(title=nm))
        return(fig)
        }



FQ_ggplot_NoSEP <- function(df, xLab, yLab, label, xmn, xmx, ymn, ymx, fit, addLegend, nm) {

    fs <- 10 #fontsize
    nGrp    <- as.integer(length(unique(df$Group)))
    # shapeCount <- nGrp
    x <- as.numeric(df$x)
    y <- as.numeric(df$y)
    sma <- (lmodel2(y~x))
    smaCoef <- round(as.numeric(sma$regression.results[3,2:3]), digits = 4)
    olsCoef <- round(as.numeric(sma$regression.results[1,2:3]), digits = 3)

    #To use the RMSEP and SEP returned by function the x values need to be in the same units as the y values
    ifelse(fit == TRUE, PrY <- x*smaCoef[2] + (smaCoef[1]), PrY <- x)
    desc <- round(FQRMSEP(PrY,y),3)

    #xmn <- floor(min(df$x));xmx <- ceiling(max(df$x))
    #ymn <- floor(min(df$y));ymx <- ceiling(max(df$y))
    grob1  = grobTree(textGrob(label, x=0.01,  y=0.95, hjust=0,gp=gpar(col="black"
                                                                       , fontsize=fs+6, fontface="bold")))
    grob2  = grobTree(textGrob(paste("SMA: y = ",smaCoef[2],"x +", round(smaCoef[1],3))
                               ,x=0.5, y=0.1, hjust=0,gp=gpar(col="black", fontsize=fs, col = "red")))
    grob2a = grobTree(textGrob(paste("OLS: y = ",olsCoef[2],"x +", round(olsCoef[1],3))
                               ,x=0.5,  y=0.20, hjust=0,gp=gpar(col="black", fontsize=fs, col = "blue")))
    #gtext1 = grobTree(textGrob("OLS", x=0.9,  y=0.6, hjust=0,gp=gpar(col="black", fontsize=fs, col = "blue")))
    #gtext2 = grobTree(textGrob("SMA", x=0.9,  y=0.95, hjust=0,gp=gpar(col="black", fontsize=fs, col = "red")))
    grob5 = grobTree(textGrob(paste("r2 =", desc[3])
                              ,x=0.5,  y=0.05, hjust=0,gp=gpar(col="black", fontsize=fs, col="black")))
    grob3 = grobTree(textGrob(paste("RMSEP = ",desc[4])
                              ,x=0.5,  y=0.1, hjust=0,gp=gpar(col="black", fontsize=fs, col="black")))
    grob4 = grobTree(textGrob(paste("SEP = ",desc[5])
                              ,x=0.5,  y=0.05, hjust=0,gp=gpar(col="black", fontsize=fs, col="black")))

    if (xmn != ymn) {lty <- 0}else{lty <- 4}

    fig <- ggplot(data = df, aes())+xlim(c(xmn,xmx))+ylim(c(ymn,ymx)) +
        xlab(xLab) +
        ylab(yLab) +
        theme_classic() +
        theme(text = element_text(size=14),plot.margin = unit(c(0.75, 0.75, 0.1, 0.1), "cm"))+
        geom_point(aes(x,y, shape = Group, color = Group), cex=1.25) +
        #geom_smooth(method = "lm", se = FALSE) +
        geom_abline(intercept = 0, slope = 1, col = "black", linetype = lty) +
        #geom_point(data = df[which(df$Group == "Oberon")], aes(x,y), pch = 16, bg = "red", cex = 3 ) +
        #geom_point(data = df[which(df$Group == "Tumba")], aes(x,y), pch = 18 , bg = "blue", cex = 3) +
        #annotate("text", x = 3.65, y = 3.9, label = "1:1 line") +
        #geom_abline(slope = olsCoef[2], intercept = olsCoef[1], color = "blue", lty = 3)+
        geom_abline(slope = smaCoef[2], intercept = smaCoef[1], color = "red", lty = 2)+
        #geom_abline(slope = avOlsCoef[2], intercept = avOlsCoef[1], color = "red")+
        scale_shape_manual(values=c(1:max(nGrp)),  guide = addLegend)+
        #scale_color_manual(values=c("black","blue","red","green","orange", "grey", "maroon4", "seagreen3","turquoise","olivedrab4","brown2", "aquamarine4","greenyellow", "mediumorchid3", ))+
        scale_color_manual(values=blue2green(length(unique(df$Group))),  guide = addLegend)+
        annotation_custom(grob1) +
        annotation_custom(grob2) +
        annotation_custom(grob5)

    if (addLegend == TRUE) fig <- fig+guides(colour = guide_legend(title=nm), shape = guide_legend(title=nm))

    return(fig)
}



#GGplot for regression analysis
#
FQ_ggplot_GENERIC <- function(df, xLab, yLab, label, xmn, xmx, ymn, ymx, fit, addLegend, nm) {
    fs <- 10 #fontsize
    #browser()
    #rename(df$Group)(nm)
    #nm <- names(df)[1]
    nGrp    <- as.integer(length(levels(unlist(df[,1]))))
    # shapeCount <- nGrp
    x <- as.numeric(df$x)
    y <- as.numeric(df$y)

    grob1  = grobTree(textGrob(label, x=0.01,  y=0.95, hjust=0,gp=gpar(col="black"
                                                                       , fontsize=fs+6, fontface="bold")))
    fig <- ggplot(df) +
        aes(x,y,color = Group, shape = Group)+
        xlim(c(xmn,xmx))+
        ylim(c(ymn,ymx)) +
        xlab(xLab) +
        ylab(yLab) +
        theme_classic() +
        geom_point(aes(x,y, color = Group), cex=1.25) +
        scale_shape_manual(values=c(1:max(nGrp)),  guide = addLegend)+
        scale_color_manual(values=blue2green(nGrp), guide = addLegend)+
        annotation_custom(grob1) +
        theme(text = element_text(size=14),plot.margin = unit(c(0.75, 0.75, 0.1, 0.1), "cm"))

    if (addLegend == TRUE) fig <- fig+guides(colour = guide_legend(title=nm), shape = guide_legend(title=nm))
    return(fig)
}

#Define the RMSEP function called by above
FQRMSEP <- function(x,y) {
    ##1  Calculate averages for Actual and predicted values
    Av.Act <- mean(na.omit(y))
    Av.Pr  <- mean(na.omit(x))

    # check for presence and position of NA's
    naX <- !is.na(x)
    x <- x[naX]
    y <- y[naX]
    naY <- !is.na(y)
    x <- x[naY]
    y <- y[naY]

    #Remove all pairs where either x or y is NA

    ##2    Calculate N
    Nx <- length(y)
    Ny <- length(x)
    #print(Nx)
    #print(Ny)
    if(Nx != Ny) print("error catastrophe")

    ##3    Calculate the square of the predicted minus actual
    sqPRACT <- (x-y)^2
    ##4 Calculate the sum of the above squares
    ##5	Divide this by N and get square root to give RMSEP
    RMSEP <- sqrt(sum(sqPRACT)/Nx)
    ##6	Calculate the bias (mean of predicted minus mean of actual)
    Bias <- Av.Pr - Av.Act
    ##7	Calculate the square of the predicted minus actual minus bias
    sqPRACTBIAS <- (x-y-Bias)^2
    ##8	Calculate the sum of the above squares
    ##9	Divide this by N-1 and get square root to give SEP
    SEP <- sqrt(sum(sqPRACTBIAS)/(Nx-1))
    ##Calculate the r2
    r2 <- cor(x,y, use = "pairwise.complete.obs")^2

    #RMSE^2 = SEP^2 + BIAS^2
    RMSEP = sqrt(SEP^2 + Bias^2)

    #Calculate the RPDC value also RPD = SDy/SEP (ratio of standard error of prediction to sample standard deviation)
    #R2= coefficient of determination = ratio of the total variance to the explained variance
    #    R2= (SDy2 - SEC2)/SDy2
    #    R2=1-(SEC2/SDy2)
    #    1-R2=SEC2/SDy2
    #    RPD=SDy/SEC=1/sqrt(1-R)

    #RPD <- sd(x)/SEP
    #print(RPD)
    RPD <- 1/sqrt(1-r2)   #Not sure why this is useful.  High r2 will give high RPD so why not just use r2???
    #print(RPD)

    #### NEED TO INCLUDE OPTION FOR RMA REGRESSION USING PACKAGE "LMODEL2"


    #return an array with the various calculations
    print_sw <- 0
    if (print_sw == 1) {
        print(paste("Actual mean = ", round(Av.Act, digits=2)))
        print(paste("Predicted mean = ", round(Av.Pr, digits=2)))
        print(paste("Variance explained (r2) = ", round(r2, digits=2)))
        print(paste("RMSEP = ", round(RMSEP, digits=2)))
        print(paste("SEP = ", round(SEP, digits=2)))
        print(paste("Bias = ", round(Bias, digits=2)))
        print(paste("RPD = ", round(RPD, digits=2)))
    }

    #Return the calculated values
    return(round(c(Av.Act, Av.Pr, r2, RMSEP, SEP, Bias, RPD),digits = 3))
}
