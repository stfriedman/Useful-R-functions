barplot_phylo <- function(x, bar.col, tree, bar.width, bar.offset, scale, 
                          tip.labels, tip.cex, tip.col, tip.offset, legend, legend.cex) {
  x <- if(hasArg(scale)) x * scale else x
  col <- if(hasArg(bar.col)) {
    if (all(bar.col %in% colors())) 
      bar.col 
    else {
      l <- levels(as.factor(bar.col))
      l <- l[!is.na(l) & l != ""]
      gg_color <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
      }
      lcol <- setNames(sapply(gg_color(length(l)), color.id), l)
      col <- lcol[bar.col]
      col[is.na(col) | col == ""] <- "grey30"
      col
    }
  } else "grey80"
  tree <- tree
  obj <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  
  if(obj$type != "fan") 
    stop("This function only works for a fan phylogeny. Re-plot phylogeny as a fan and then try this function again.")
  
  w <- if(hasArg(bar.width)) bar.width else 0.2
  h <- max(nodeHeights(tree))
  sw <- if(hasArg(bar.offset)) bar.offset else 1
  if(length(w) == 1){
    for(i in 1:length(x)){
      theta<-atan(obj$yy[i]/obj$xx[i])
      s<-if(obj$xx[i]>0) 1 else -1
      dx<-s*h*cos(theta)+s*cos(theta)*sw
      dy<-s*h*sin(theta)+s*sin(theta)*sw
      x1<-dx+(w/2)*cos(pi/2-theta)-s*min(0,min(x))*cos(theta)
      y1<-dy-(w/2)*sin(pi/2-theta)-s*min(0,min(x))*sin(theta)
      x2<-dx-(w/2)*cos(pi/2-theta)-s*min(0,min(x))*cos(theta)
      y2<-dy+(w/2)*sin(pi/2-theta)-s*min(0,min(x))*sin(theta)
      x3<-s*x[i]*cos(theta)+x2
      y3<-s*x[i]*sin(theta)+y2
      x4<-s*x[i]*cos(theta)+x1
      y4<-s*x[i]*sin(theta)+y1
      polygon(c(x1,x2,x3,x4),c(y1,y2,y3,y4),col=col[i], border="transparent")
    }
  } else {
    for(i in 1:length(x)){
      theta<-atan(obj$yy[i]/obj$xx[i])
      s<-if(obj$xx[i]>0) 1 else -1
      dx<-s*h*cos(theta)+s*cos(theta)*sw
      dy<-s*h*sin(theta)+s*sin(theta)*sw
      x1<-dx+(w[i]/2)*cos(pi/2-theta)-s*min(0,min(x))*cos(theta)
      y1<-dy-(w[i]/2)*sin(pi/2-theta)-s*min(0,min(x))*sin(theta)
      x2<-dx-(w[i]/2)*cos(pi/2-theta)-s*min(0,min(x))*cos(theta)
      y2<-dy+(w[i]/2)*sin(pi/2-theta)-s*min(0,min(x))*sin(theta)
      x3<-s*x[i]*cos(theta)+x2
      y3<-s*x[i]*sin(theta)+y2
      x4<-s*x[i]*cos(theta)+x1
      y4<-s*x[i]*sin(theta)+y1
      polygon(c(x1,x2,x3,x4),c(y1,y2,y3,y4),col=col[i], border="transparent")
    }
    
  }
  if(hasArg(tip.labels)){
    if(tip.labels == TRUE) {
      tips <- tree$tip.label
      tip.cex <- if(hasArg(tip.cex)) tip.cex else 1
      tip.col <- if(hasArg(tip.col)) tip.col else "black"
      tip.offset <- if(hasArg(tip.offset)) tip.offset else 0
      for(i in 1:length(tips)){
        tmp <- rect2polar(obj$xx[i], obj$yy[i])
        angle <- atan(obj$yy[i]/obj$xx[i])*180/pi
        tmp <- polar2rect(tmp$r + tip.offset, tmp$angle)
        
        if(obj$xx[i] < 0) {
          text(tmp$x,tmp$y, paste(gsub("_"," ",tips[i])," ", sep=""),
               pos=2,srt=angle, offset=0, cex=tip.cex, col=tip.col)
        } else {
          text(tmp$x,tmp$y, paste(" ",gsub("_"," ",tips[i]),sep=""),
               pos=4,srt=angle, offset=0, cex=tip.cex, col=tip.col)
        }
        
      }
      
    }
    
    if(hasArg(legend) & legend == TRUE & all(!bar.col %in% colors()))  {
      legend.cex <- if(hasArg(legend.cex)) legend.cex else 1
      legend("topleft", names(lcol), col=lcol, bty = "n", pch=16, cex = legend.cex)
    }
  }
}
