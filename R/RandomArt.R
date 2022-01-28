# Random art
# Daniel van der Meer

make_art <- function(seed){
  set.seed(seed)
  x <- seq(-20, 20, 0.1)
  spread = sample(seq(-0.2, 0.4), 1)
  slope = sample(seq(-10, 0, 1), 1)

  range_spread <- seq(spread, spread+4, 0.2)
  range_slope <- seq(slope, slope+20, 0.3)

  l <- slope*x+3
  par(bg = colors()[sample(c(2:600), 1)])
  plot(NULL, xlim = c(-20, -10),
       ylim = c(-20, -10),
       frame = F,
       axes = F,
       xlab = "",
       ylab = "")
  col <- 1
  for (i in range_spread){
    for (j in range_slope){
      lines(x, i*sin(i*x)+(j*x+3),
            col = col,
            lw = 4)
      col = col+1
      if (col == 657){
        col = 1
      }
    }
  }
}


