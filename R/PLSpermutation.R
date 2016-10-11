PLSpermutation <- function(data = NULL, info = NULL, repeats = 200, ncomp = 3,
                           scalemethod = "auto") {
  # browser()
  options(warn=-1)
  if (is.null(data))  stop("sample is NULL")
  if (is.null(info)) stop("info must not be NULL")

  #load needed packages
  need.packages1<-c("plspm","plsdepot","pls","lars",
                    "ROCR","ggplot2","pROC","ellipse")

  packages<-library()[[2]][,1]
  for (i in need.packages1) {
    if (!any(packages==i)) {install.packages(i)}
  }


  need.packages2<-c("SXTscale","SXTvip")
  for (i in need.packages2) {
    if (!any(packages==i)) stop(paste("Please install",i,"package form Japse Shen!"))
  }

  require(plsdepot)
  require(pls);  require(ROCR)
  require(SXTscale); require(SXTvip)

  int <- data
  index <- NULL
  for (i in 1:length(info)) {
    index1 <- as.character(info[[i]])
    index <- c(index,index1)
  }
  if (length(which(index==""))!=0)  {index<-index[-which(index=="")]}

  index <- index[!is.na(index)]
  index <- match(index,rownames(int))
  index <- index[!is.na(index)]
  int <- int[index,]

  #######
  name <- rownames(int)
  # browser()
  Y <- NULL
  label <- list()
  for (i in 1:length( info )) {
    label[[i]] <- match(as.character(info[[i]]),name)
    label[[i]] <- label[[i]][!is.na(label[[i]])]
    Y[label[[i]]] <- i-1
  }

  int.scale <- SXTscale(int,method = scalemethod)
  # int.Y<-SXTscale(Y,method=scalemethod)

  require(SXTdummy)
  dummy <- SXTdummy(Y)
  # int.dummy<-SXTscale(dummy,method=scalemethod)
  pls <- plsreg1(int.scale,Y,comps = ncomp)
  Q2 <- pls$Q2[ncomp,5]
  R2 <- sum(pls$R2)

  save(pls,file = "pls")
  save(Q2, file = "Q2")
  save(R2, file = "R2")

  ##begin repeat
  q2 <- NULL
  r2 <- NULL
  cor <- NULL
  for (i in 1:repeats) {
    temp.Y <- Y[order(sample(1:length(Y), length(Y)))]
    temp.dummy <- SXTdummy(temp.Y)
    temp.pls <- plsreg1(int.scale,temp.Y,comps = ncomp)
    q2[i] <- temp.pls$Q2[ncomp,5]
    r2[i] <- sum(temp.pls$R2)
    cor[i] <- abs(cor(Y, temp.Y))
    cat(i); cat(" ")
  }

  save(q2, file = "q2_200")
  save(r2, file = "r2_200")
  save(cor, file = "cor")

  ##draw perumtation test
  pdf("Permutation test.pdf")
  par(xpd = F)
  par(mar=c(5,5,4,2))
  plot(x = 0, y = 0, xlim = c(0,1), ylim = c(min(c(q2,r2)),1.3*max(c(q2,r2))), col = "white", xlab = "Correlation",
       ylab = "Values (Q2, R2)",
       cex.axis = 1.3, cex.lab = 1.3)
  abline( h = 0, lty = 2)

  points(x = cor, y = q2, col = "palegreen", pch = 19)
  points(x = cor, y = r2, col = "royalblue", pch = 19)

  points(x = 1, y = Q2, col = "palegreen", pch = 19)
  points(x = 1, y = R2, col = "royalblue", pch = 19)

  lm.r2 <- lm(c(R2,r2)~c(1, cor))
  lm.q2 <- lm(c(Q2,q2)~c(1, cor))

  intercept.q2 <- lm.q2$coefficients[1]
  intercept.r2 <- lm.r2$coefficients[1]

  segments(x0 = 0, y0 = intercept.q2, x1 = 1, y1 = Q2, lty = 2, lwd = 2)
  segments(x0 = 0, y0 = intercept.r2, x1 = 1, y1 = R2, lty =2, lwd = 2)


  legend("bottomright", title = "Intercepts",
         legend = c(paste("Q2",round(intercept.q2,2), sep = ": "), paste("R2",round(intercept.r2,2),sep=": ")),
         col = c("palegreen", "royalblue"), pch = 19, pt.cex = 1.3, cex = 1.3, bty = "n")
  par(xpd = T)
  dev.off()
}





