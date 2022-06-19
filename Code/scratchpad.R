# Create the input vectors.
colors = c("green","orange","brown")
months <- c("Mar","Apr","May","Jun","Jul")
regions <- c("East","West","North")

# Create the matrix of the values.
Values <- matrix(c(2,9,3,11,9,4,8,7,3,12,5,2,8,10,11), nrow = 3, ncol = 5, byrow = TRUE)

# Give the chart file a name
png(file = "barchart_stacked.png")

# Create the bar chart
barplot(Values, main = "total revenue", names.arg = months, xlab = "month", ylab = "revenue", col = colors)

# Add the legend to the chart
legend("topleft", regions, cex = 1.3, fill = colors)

# Save the file
dev.off()




library("RColorBrewer")

HOLD <- matrix(c(0.80, 0.70, 0.70, 0.65, 0.56, 0.56, 0.78, 0.54, 0.40, 0.76, 0.23, 0.45), ncol=6, nrow=2)

colnames(HOLD) <- c("Infolettre", "Promotion", "Abonne", "Comédie", "Drame", "Suspense")
rownames(HOLD) <- c("D.T.", "F.T.")

HOLD

SEQUENTIAL <- brewer.pal(2, "PuBu")

par(mfrow=c(1,3))
barplot(HOLD[,1] * 100, main="Infolettre", ylab="%", ylim=c(0, 100), las=1,  col=SEQUENTIAL)
barplot(HOLD[,2] * 100, main="Promotion", ylab="%", ylim=c(0,100), las=1, col=SEQUENTIAL)
barplot(HOLD[,3] * 100, main="Abonne", ylab="%", ylim=c(0,100), las=1, col=SEQUENTIAL)

par(mfrow=c(1,3))
BP = barplot(HOLD[,1] * 100, main="Infolettre", ylab="%", ylim=c(0, 100), las=1,  col=SEQUENTIAL)
text(BP, HOLD[,1]*100, labels=HOLD[,1]*100, pos=3)

BP = barplot(HOLD[,2] * 100, main="Promotion", ylab="%", ylim=c(0,100), las=1, col=SEQUENTIAL)
text(BP, HOLD[,2]*100, labels=HOLD[,2]*100, pos=3)

BP = barplot(HOLD[,3] * 100, main="Abonne", ylab="%", ylim=c(0,100), las=1, col=SEQUENTIAL)
text(BP, HOLD[,3]*100, labels=HOLD[,3]*100, pos=3)
