
png(paste("BLOG/passgrid.png", sep=""), width=1000, height=1400)
drawatlas("throw", " ", " ")
abline(v=((-3:3)*10))
abline(h=((-1:7)*10))
dev.off()