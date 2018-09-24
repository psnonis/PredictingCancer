outliers.summ <- function(data, variable) {
x <- data[[variable]]
quart.1 <- summary(x)[2]
quart.3 <- summary(x)[5]
iqr_hs <- IQR(x, na.rm = TRUE)

ext.indic <- x > (quart.3 + 3*iqr_hs) |
                     x < (quart.1 - 3*iqr_hs)
mild.indic <- x > (quart.3 + 1.5*iqr_hs) |
                     x < (quart.1 - 1.5*iqr_hs)

mild.outl <<- data[mild.indic, ]
ext.outl <<- data[ext.indic, ]
					 
print(paste("Outliers: ", sum(mild.indic, na.rm = TRUE), 
            " (", round(sum(mild.indic, na.rm = TRUE)/nrow(Cancer)*100, 2), "%)", sep = ""))
print(paste("Extreme outliers: ", sum(ext.indic, na.rm = TRUE), 
            " (", round(sum(ext.indic, na.rm = TRUE)/nrow(Cancer)*100, 2), "%)", sep = ""))
}