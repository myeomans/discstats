

df <- read.table(text="xCoord yCoord   pts
11.4     14.9     2
2.6       1.1      0
4.8       4.1      2
-14.4    8.2      2
4.2       0.3      0
0.4       0.0     2
-23.2   -1.1      3", header=TRUE)
h <- hexbin (x=df$xCoord, y = df$yCoord, IDs = TRUE, xbins=50)
pts.binned <- hexTapply (h, df$pts, FUN=mean)

df.binned <- data.frame (xCoord  = h@xcm, 
                         yCoord  = h@ycm, FGA = h@count, pts = pts.binned)

chart.player <- ggplot (df.binned, aes (x =xCoord , 
                                        y =yCoord , col = pts, size = FGA)) + coord_fixed() + 
  geom_point()  + scale_colour_gradient("Points/Attempt", low = "green", high="red")