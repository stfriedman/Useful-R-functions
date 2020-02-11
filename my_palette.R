my_cols <- function(name = c("muted", "pastel", "bold", "seq_blue", "blpnk", "blyel", "blorg", "muted_seq", "for_v", "four_col", "wes"), n, rev = FALSE) {

  muted = c("#3F415C", "#778A97", "#A1B6C2", "#CBCED1", "#E6CFC6", "#C0A8AC")
  pastel = c("#CBE4F9", "#CDF5F6", "#EFF9DA", "#F9EBDF", "#F9D8D6", "#D6CDEA")
  seq_blue = c("#1c394aff", "#2a566fff", "#387394ff", "#4690b9ff", "#6ba6c7ff", 
                  "#90bcd5ff", "#b5d2e3ff", "#dae8f1ff")
  blpnk = c("#DCB4B7", "#EED4D5", "#E9E5E2", "#57828C", "#2A5B67")
  bold = c( "#FF5C3E", "#FFD36E", "#D6D953", "#5CD89F", "#18A2CC", "#005D68",
            "#545454",  "#653265", "#ED6F7E")
  four_col <- c("#B7A9C3", "#E8ABA7", "#C5D8EB", "#7DBBAE")
  blyel = c("#0A1C4F", "#007991", "#1EA888", "#8BE09C", "#EDD76D")
  blorg = c("#2364AA", "#3DA5D9", "#5BBFB5", "#FFCF23", "#EA8131")
  for_v <- c("#0A1C4F","#2364AA", "#3DA5D9", "#5BBFB5", "#8BE09C", "#FFCF23", "#EA8131")
  muted_seq <- c( "#BD7644", "#AA7E2B", "#8E7F3D", "#98A440","#678F58", 
                  "#235133","#25546A", "#49879D", "#97A7AB")
  wes <- c("#75BFC1", "#CD2216", "#E8A252", "#D4DAB2", "#6B7181")
    
  
  name = match.arg(name)
  orig = eval(parse(text = name))
  
  x <- length(orig)
  if(!hasArg(n)) n <- x
 
   if(n > x) 
    stop(paste("The", name, "palette only has", x, "colors. Adjust your n accordingly or pick a different palette"))
  
  if(rev == TRUE) orig <- rev(orig)
  
  rgb = t(col2rgb(orig))
  temp = matrix(NA, ncol = 3, nrow = n)
  x = seq(0, 1, ,length(orig))
  xg = seq(0, 1, ,n)
  for (k in 1:3) {
    hold = spline(x, rgb[, k], n = n)$y
    hold[hold < 0] = 0
    hold[hold > 255] = 255
    temp[, k] = round(hold)
  }
  palette = rgb(temp[, 1], temp[, 2], temp[, 3], maxColorValue = 255)
  palette
}
  


view_all_pals <- function() {
  pals <- formals(my_cols)$name
  pals <- as.character(pals)[-1]
  n <- 9
  
  nr <- length(pals)
  nc <- max(n)
  ylim <- c(0, nr)
  oldpar <- par(mgp = c(2, 0.25, 0))
  plot(1, 1, xlim = c(0, nc), ylim = ylim, type = "n", axes = FALSE, 
       bty = "n", xlab = "", ylab = "")
  for (i in 1:nr) {
    nj <- length(my_cols(pals[i]))
    if (pals[i] == "") 
      next
    shadi <- my_cols(pals[i])
    rect(xleft = 0:(nj - 1), ybottom = i - 1, xright = 1:nj, 
         ytop = i - 0.2, col = shadi, border = "light grey")
  }
  text(rep(-0.1, nr), (1:nr) - 0.6, labels = pals, xpd = TRUE, 
       adj = 1)
}


### EXAMPLE 
# view_all_pals()
