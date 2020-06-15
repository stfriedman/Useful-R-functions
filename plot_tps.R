library(geomorph)
library(tidyverse)
library(jpeg)

compile_tps <- function(file, num_lm){
  tps <- readland.tps(file, specID="imageID", readcurves = TRUE)
  
  tps_photos <- paste0(dimnames(tps)[[3]], ".jpg")
  dir_photos <- list.files(pattern = ".jpg")
  
  if(!all(tps_photos %in% dir_photos)){
    f <- tps_photos[!tps_photos %in% dir_photos]
    cat(c("\n \n Error: The following photos are missing from your working directory: \n",
           paste(f, collapse =", ")))
    opt <- options(show.error.messages=FALSE)
    on.exit(options(opt))
    stop()
  }
  
  n <- nrow(tps[,,1])
  output <- list()
  
  for(i in 1:length(dimnames(tps)[[3]])){
    landmarks <- as.data.frame(tps[1:num_lm,,i])
    image <- readJPEG(paste(dimnames(tps)[[3]][i],".jpg", sep=""))
    
    # getting scaling factor
    f <- readLines(file)
    cline <- grep("SCALE",f, value=TRUE)
    scale <- as.numeric(substr(cline[i], start=7, stop=13))
    
    if(n > num_lm){
    semilandmarks <- as.data.frame(tps[(num_lm+1):n,,i])
    output[[i]] <- list(landmarks = landmarks, 
                        semilandmarks = semilandmarks,
                        image = image, scale = scale)
    } else {
      output[[i]] <- list(landmarks = landmarks, 
                          image = image, scale = scale)
    }
  }
  names(output) <- dimnames(tps)[[3]]
  output
}


plot_tps <- function(x, pdf = TRUE, pdf_name) {
  if(pdf == TRUE){
    if(hasArg(pdf_name)){
      file =  paste0(pdf_name, ".pdf")
    } else {
      file <- "plot_tps.pdf"
      }
    pdf(file = file)
    }
  if(vec_depth(x) == 3){
    plot(seq(0, dim(x$image)[2], length.out = 10), 
         seq(0, dim(x$image)[1], length.out = 10), type = "n", xlab = "", 
         ylab = "", asp=1, tck = 0, axes=FALSE)
    rasterImage(x$image, 1, 1, dim(x$image)[2], dim(x$image)[1], interpolate=TRUE)
    x1 <- x$landmarks[,1]/x$scale
    y1 <- x$landmarks[,2]/x$scale
    if(any(names(x) == "semilandmarks")){
      sx <- x$semilandmarks[,1]/x$scale 
      sy <- x$semilandmarks[,2]/x$scale
      points(sx, sy, pch=16, col="blue2", cex=0.3)
    }
    points(x1, y1, pch=16, col="red3", cex=1)
    text(x1, y1, cex=0.5, labels = 1:nrow(x$landmarks))
  } else {
    for(i in 1:length(names(x))){
      x1 <- x[[i]]$landmarks[,1]/x[[i]]$scale
      y1 <- x[[i]]$landmarks[,2]/x[[i]]$scale
      plot(seq(0, dim(x[[i]]$image)[2], length.out = 10), 
           seq(0, dim(x[[i]]$image)[1], length.out = 10), type = "n", xlab = "", 
           ylab = "", asp=1, tck = 0, axes=FALSE)
      rasterImage(x[[i]]$image, 1, 1, dim(x[[i]]$image)[2], dim(x[[i]]$image)[1], 
                  interpolate=TRUE)
      points(x1, y1, pch=16, col="red3", cex=1)
      if(any(names(x[[i]]) == "semilandmarks")){
        sx <- x[[i]]$semilandmarks[,1]/x[[i]]$scale 
        sy <- x[[i]]$semilandmarks[,2]/x[[i]]$scale
        points(sx, sy, pch=16, col="blue2", cex=0.3)
      }
      text(x1, y1, cex=0.5, labels = 1:nrow(x[[i]]$landmarks))
      mtext(side=3, line=1, cex=1, names(x)[[i]])
      mtext(side=3, line=0, cex=0.7, paste("scale =", round(x[[i]]$scale, 3)))
    }
  }
  if(pdf == TRUE){
    dev.off() 
    cat(paste0("PDF has been written to ", getwd()[[1]], "/", file))
  }
}


################# EXAMPLE ##################
## Must set working directory to location with both TPS file and images. 
setwd("~/Desktop/Brandl_collab/test")

#num_lm is the number of (non-semi) landmarks
x <- compile_tps("Test2_rand_Robin.TPS", num_lm = 20) 

## Missing landmarks plotted in lower right corner of image.
# If pdf = FALSE function will plot all photos with associated landmarks in R window. WARNING: this can take a while and will crash R if there are too many photos. If pdf = TRUE (recommended) then function will write all files to a pdf instead; user can designate the pdf name with the pdf name argument. 
plot_tps(x, pdf = TRUE, pdf_name = "my_file")

