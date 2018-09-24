boxHist <- function(v,name)
{
  vLen    <- length(v)
  vMin    <- min(v, na.rm = T)
  vMax    <- max(v, na.rm = T)
  vBreaks <- sqrt(length(v))
  vMean   <- mean(v, na.rm = T)
  vMedian <- median(v, na.rm = T)
  vSD     <- sd(v)

  l <- layout(mat = matrix(c(1,2),2,1, byrow=T), height = c(1,8))
  p <- par(mar = c(0, 3.1, 1.1, 2.1))
  b <- boxplot(v, horizontal = T, xaxt = 'n', col ='#5E4FA2', frame = F)
  p <- par(mar=c(4, 3.1, 1.1, 2.1))
  h <- hist(v, 
			col = 'gray',
			# col = c('#9E0142','#D53E4F','#F46D43','#FDAE61','#FEE08B','#FFFFBF','#E6F598','#ABDDA4','#66C2A5','#3288BD','#5E4FA2'),
            breaks = vBreaks,
            main = NA,
            xlab = name,
			ylab = "Frequency",
            labels = F )

  xFit <- seq(vMin, vMax, length.out = 100)
  yFit <- dnorm(xFit, mean = vMean, sd = vSD)
  yFit <- yFit * diff(h$mids[1:2]) * vLen
  
  x <- lines(xFit, yFit, col = 'darkblue', lwd = 3)
  x <- abline(v = vMean, col = 'red', lwd = 3, lty = 1)
  x <- abline(v = vMedian, col = 'red', lwd = 3, lty = 2)
}