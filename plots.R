yxScatter <- function(y, x = NA, title = NA)
{
    if(is.na(title))
    {
        if(is.na(x))
        {
            t <- tail(strsplit(deparse(substitute(y)), '\\$')[[1]], 1)
        }
        else
        {
            t <- paste(tail(strsplit(deparse(substitute(x)), '\\$')[[1]], 1), 'vs.',
                       tail(strsplit(deparse(substitute(y)), '\\$')[[1]], 1))
        }
    }

    if(is.na(x))
    {
        n <- 1:length(y)
    }
    else
    {
        n <- x
    }

    p <- qplot(n, y) +
        geom_point(size = 4, alpha = 0.5, color = Cancer$color) +
        theme_wsj() +
        theme(legend.position = 'none',
              text = element_text(size = 16, family = 'gloria'),
              title = element_text(size = 30, family = 'gloria'),
              plot.title = element_text(hjust = 0.5)) +
    ggtitle(t)

    if(is.na(x))
    {
        p
    }
    else
    {
        p + geom_smooth(method = 'lm', lwd = 3, alpha = 0.5, color = 'red', fullrange = T)
    }
}

boxHist <- function(v, title = NA)
{
    if(is.na(title))
    {
        t <- tail(strsplit(deparse(substitute(y)), '\\$')[[1]], 1)
    }

    vLen    <- length(v)
    vMin    <- min(v, na.rm = T)
    vMax    <- max(v, na.rm = T)
    vBreaks <- seq(vMin, vMax, length.out = 10)
    vMean   <- mean(v, na.rm = T)
    vMedian <- median(v, na.rm = T)
    vSD     <- sd(v)

    layout(mat = matrix(c(1, 2), 2, 1, byrow = T), heights = c(2, 8))

    par(mar = c(0, 3, 3, 1), bg = 'blanchedalmond')

    b <- boxplot(v, main = t,
                 horizontal = T,
                 xaxt = 'n',
                 col ='#5E4FA2',
                 frame = F)

    par(mar = c(5, 3, 0, 1))

    d <- hist(v, breaks = vBreaks, plot = F)
    h <- hist(v, breaks = vBreaks,
              col = c('#9E0142','#D53E4F','#F46D43','#FDAE61','#FEE08B','#FFFFBF','#E6F598','#ABDDA4','#66C2A5'),
              border = NA,
              main = NA,
              xlab = sprintf('Mean is %6.3f, Median is %6.3f', vMean, vMedian),
              ylab = NA,
              ylim = c(0, max(d$counts) * 1.1),
              labels = T,
              font.lab = 2)

    xFit <- seq(vMin, vMax, length.out = 100)
    yFit <- dnorm(xFit, mean = vMean, sd = vSD)
    yFit <- yFit * diff(h$mids[1:2]) * vLen

    x <- lines(xFit, yFit, col = '#5E4FA2C0', lwd = 3)
    x <- abline(v = vMean, col = '#3288BDC0', lwd = 3, lty = 1)
    x <- abline(v = vMedian, col = '#3288BDC0', lwd = 3, lty = 2)
}
