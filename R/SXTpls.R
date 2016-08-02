SXTpls<-function(sample=NULL,qc=NULL,info=NULL,
                 #used data
                 scalemethod="auto", plsmethod=c("plsreg","plsr"),
                 width=7,height=7,QC=FALSE,text=FALSE,ellipse=FALSE,
                 color=c("palegreen","firebrick1","royalblue","yellow","black","cyan","gray48"),
                 shape=c(17,19,15,18,2,8,11),cexa=1)
  #parameter setting
{
  # browser()
  options(warn=-1)
  if (is.null(sample))  stop("sample is NULL")
  if (!is.null(qc))
  {if (ncol(sample)!=ncol(qc)) stop("the column number of sample and qc must be same")}
  if (is.null(qc)&QC) stop("QC shoud be FALSE because qc is NULL")
  if (is.null(info)) stop("info must not be NULL")


  #load needed packages
  need.packages1<-c("plspm","plsdepot","scatterplot3d","pls","lars",
                    "ROCR","ggplot2","pROC","ellipse")

  packages<-library()[[2]][,1]
  for (i in need.packages1) {
    if (!any(packages==i)) {install.packages(i)}
  }


  need.packages2<-c("SXTscale","SXTvip")
  for (i in need.packages2) {
    if (!any(packages==i)) stop(paste("Please install",i,"package form Japse Shen!"))
  }

  require(plspm); require(plsdepot); require(scatterplot3d)
  require(pls); require(lars); require(ROCR); require(ggplot2)
  require(pROC); require(ellipse); require(SXTscale); require(SXTvip)

  int<-sample
  index<-NULL
  for (i in 1:length(info)) {
    index1<-as.character(info[[i]])
    index<-c(index,index1)
  }
  if (length(which(index==""))!=0)  {index<-index[-which(index=="")]}

  index<-index[!is.na(index)]
  index<-match(index,rownames(int))
  index<-index[!is.na(index)]
  int<-int[index,]

  ifelse(QC,int<-rbind(int,qc),int<-int)
  #######
  q<-grep("QC",rownames(int))
  name<-rownames(int)
  # browser()
  Y<-NULL
  label<-list()
  for (i in 1:length( info )) {
    label[[i]]<-match(as.character(info[[i]]),name)
    label[[i]]<-label[[i]][!is.na(label[[i]])]
    Y[label[[i]]]<-i-1
  }
  if (QC) {Y[q]<- length(info)}

  int.scale <- SXTscale(int,method=scalemethod)
  # int.Y<-SXTscale(Y,method=scalemethod)
  int.Y <- Y
  ncompa <- nrow(int) - 1

  if (plsmethod=="plsr") {
    pls1<-plsr(int.Y~int.scale,scale=FALSE,validation="CV",ncomp=ncompa,method = "oscorespls")
    save(pls1,file="pls1")
    #########select the number of compents#################

    msep<-MSEP(pls1)
    save(msep,file="msep")
    msep<-msep$val[,,]

    yesnot<-"y"
    while (yesnot=="y"|yesnot=="") {
      comps.number<-readline("How many comps do you want to see? ")
      while (!exists("comps.number")|comps.number=="")
      {cat("You must give a comps number to continute!!!\n")
        comps.number<-readline("How many comps do you want to see? ")}
      comps.number<-as.numeric(comps.number)
      plot(x=c(1:comps.number),y=msep[1,2:(comps.number+1)],type="b",col="firebrick1",pch=20,
           xlab="ncomp",ylab="MSEP",cex.lab=1.3,cex.axis=1.3)
      points(x=c(1:comps.number),y=msep[2,2:(comps.number+1)],type="b",pch=2)
      legend("top",legend = c("CV","adjCV"),col = c("firebrick1","black"),pch=c(20,2),
             bty = "n",cex = 1.3,pt.cex = 1.3)
      yesnot<-readline("Do you want to see the next plot? (y/n)")
    }

    pdf("MSEP plot.pdf")
    plot(x=c(1:comps.number),y=msep[1,2:(comps.number+1)],type="b",col="firebrick1",pch=20,
         xlab="ncomp",ylab="MSEP",cex.lab=1.3,cex.axis=1.3)
    points(x=c(1:comps.number),y=msep[2,2:(comps.number+1)],type="b",pch=2)
    legend("top",legend = c("CV","adjCV"),col = c("firebrick1","black"),pch=c(20,2),
           bty = "n",cex = 1.3,pt.cex = 1.3)
    dev.off()

    number<-readline("Please type number and press Enter  to continute:  ")
    while (!exists("number")|number=="") {cat("You must give a number to continute!!!\n")
      number<-readline("Please type comps number value and press Enter  to continute: ")}
    number<-as.numeric(number)

    ##################construct final pls model###################
    pls2<-plsr(int.Y~int.scale,scale=FALSE,validation="CV",ncomp=number,method = "oscorespls")
    save(pls2,file="pls2")
    vip<-SXTvip(pls2)
    save(vip,file="vip")

    scores<-scores(pls2)
    x<-scores[,1]
    y<-scores[,2]
    if (number>2) {z<-scores[,3];zmin<-1.2*min(z);zmax<-1.2*max(z)}

    xmin<-1.2*min(x)
    xmax<-1.2*max(x)
    ymin<-1.2*min(y)
    ymax<-1.2*max(y)
  }

  else {
    require(SXTdummy)
    dummy<-SXTdummy(Y)
    # int.dummy<-SXTscale(dummy,method=scalemethod)
    int.dummy <- dummy
    pls1<-plsreg2(int.scale,int.dummy,comps=ncompa)
    save(pls1,file="pls1")
    #########select the number of compents#################
    Q2cum<-pls1$Q2cum
    Q2cum<-Q2cum[,ncol(Q2cum)]

    yesnot<-"y"
    while (yesnot=="y"|yesnot=="") {
      comps.number<-readline("How many comps do you want to see? ")
      while (!exists("comps.number")|comps.number=="") {cat("You must give a comps number to continute!!!\n")
        comps.number<-readline("How many comps do you want to see? ")}
      comps.number<-as.numeric(comps.number)
      barplot(Q2cum[1:comps.number],xlab="ncomp",ylab="Q2cum",cex.lab=1.3,cex.axis=1.3)
      a<-barplot(Q2cum[1:comps.number],xlab="ncomp",ylab="Q2cum",cex.lab=1.3,cex.axis=1.3)
      abline(h=0)
      points(a,Q2cum[1:comps.number],type="b",col="red",pch=20,cex=2)
      yesnot<-readline("Do you want to see the next plot? (y/n)")
    }
    pdf("Q2cum plot.pdf",width=7,height=7)
    barplot(Q2cum[1:comps.number],xlab="ncomp",ylab="Q2cum",cex.lab=1.3,cex.axis=1.3)
    a<-barplot(Q2cum[1:comps.number],xlab="ncomp",ylab="Q2cum",cex.lab=1.3,cex.axis=1.3)
    abline(h=0)
    points(a,Q2cum[1:comps.number],type="b",col="red",pch=20,cex=2)
    dev.off()

    number<-readline("Please type number and press Enter  to continute:  ")
    while (!exists("number")|number=="") {cat("You must give a number to continute!!!\n")
      number<-readline("Please type comps number value and press Enter  to continute: ")}
    number<-as.numeric(number)

    ##################construct final pls model###################
    cat(paste("Construct PLS model with all peaks using",number,"comps ...","\n"))
    pls2<-plsreg2(int.scale,int.dummy,comps=number)
    save(pls2,file="pls2")
    vip<-pls2$VIP
    Q2cum<-pls2$Q2cum
    Q2<-pls2$Q2
    expvar<-pls2$expvar

    write.csv(cbind(expvar,Q2cum),"R2Q2.csv",row.names = F)

    ##draw barplot of Q2cum, R2Xcum and R2Ycum
    Q2R2<-cbind( expvar[,c(2,4)], Q2cum[,3])
    colnames(Q2R2)[3 ]<- "Q2cum"
    pdf("Q2R2cum.pdf",width=8,height = 6)
    barplot( t(Q2R2), beside = T, col = c( "palegreen", " firebrick1", "royalblue"),
             cex.lab=1.3, cex.axis=1.3, cex.names = 1.3)
    abline( h=0)
    legend( "topleft", legend = c( "R2Xcum", "R2Ycum", "Q2cum"),pch=15,
            col = c( "palegreen", " firebrick1", "royalblue"), cex = 1.3, pt.cex = 1.3,bty = "n")
    dev.off()

    save(vip,file="vip")
    save(Q2,file="Q2")
    save(Q2cum,file="Q2cum")
    save(expvar,file="expvar")

    x<-pls2$x.scores[,1]
    y<-pls2$x.scores[,2]
    if (number>2) {z<-pls2$x.scores[,3];zmin<-1.2*min(z);zmax<-1.2*max(z)}

    xmin<-1.2*min(x)
    xmax<-1.2*max(x)
    ymin<-1.2*min(y)
    ymax<-1.2*max(y)
  }

  legend<-NULL
  for (i in 1:length(label)) {
    legend[label[[i]]]<- names(info)[i]
  }
  if (QC) {legend[q]<-"QC"}

  colour<-NULL
  colourlist<-color
  for (i in 1:length(label)) {
    colour[label[[i]]]<-colourlist[i]
  }
  if (QC) {colour[q]<-colourlist[length(info)+1]}

  pcha<-NULL
  pchalist<-shape
  for (i in 1:length(label)) {
    pcha[label[[i]]]<-pchalist[i]
  }
  if (QC) {pcha[q]<-pchalist[length(info)+1]}

  #PLS 2D
  pdf("plsplot 2d t1 vs t2.pdf",width=width,height=height)
  plot(x,y,xlim=c(xmin,xmax),ylim=c(ymin,ymax),col=colour,pch=pcha,xlab="t[1]",ylab="t[2]",
       cex=cexa,cex.axis=1.3,cex.lab=1.3)
  abline(v=0,lty=2)
  abline(h=0,lty=2)
  if (text) {text(x,y,rownames(int),pos=4)}
  if (ellipse) {lines(ellipse(0,scale=c(sd(x),sd(y)),centre=c(mean(x),mean(y))),lty=2)}

  if (QC) {legend("topleft",c( names(info),"QC"),
  pch=pchalist[1:(length(info)+1)],col=colourlist[1:(length(info)+1)],bty="n",cex=1.3)
  }else{legend("topleft",names(info),
               pch=pchalist[1: length(info)],col=colourlist[1:length(info)],bty="n",cex=1.3)}
  dev.off()

  #PLS 3D
  if (number>2) {
    pdf("plsplot 3d.pdf",width=width,height=height)
    scatterplot3d(x,y,z,color=colour,xlab="t[1]",ylab="t[2]",zlab="t[3]",angle=50,
                  pch=pcha,box=FALSE,cex.symbol=cexa,cex.lab=1.3,cex.axis=1.3,
                  xlim=c(xmin,xmax),ylim=c(ymin,ymax),zlim=c(zmin,zmax))
    if (QC) {
      legend("topleft",c( names(info),"QC"),
             pch=pchalist[1:(length(info)+1)],col=colourlist[1:(length(info)+1)],bty="n",cex=1.3)
    }else{
      legend("topleft",names(info),
             pch=pchalist[1:length(info)],col=colourlist[1:length(info)],bty="n",cex=1.3)
    }
    dev.off()
  }

  cat("PLS analysis is done\n")
}

