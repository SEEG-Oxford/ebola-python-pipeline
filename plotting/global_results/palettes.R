# colour palettes for EVD spread modelling results

# load Color Brewer R package
require(RColorBrewer)

# convert an RColorBrewer div palette into a colour ramp
divRamp <- function(name = c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu",
                             "PuOr", "PRGn", "PiYG", "BrBG")) {
  
  # match the name
  name <- match.arg(name)
  
  # fetch the palette
  pal <- brewer.pal(n = 11, name = name)
  
  # convert to a ramp
  ramp <- colorRampPalette(pal)
  
  # return this
  return (ramp)
  
}

# convert an RColorBrewer seq palette into a colour ramp
seqRamp <- function(name = c("YlOrRd", "YlOrBr", "YlGnBu", "YlGn", "Reds",
                             "RdPu", "Purples", "PuRd", "PuBuGn", "PuBu",
                             "OrRd", "Oranges", "Greys", "Greens", "GnBu",
                             "BuPu", "BuGn", "Blues")) {
  
  # match the name
  name <- match.arg(name)
  
  # fetch the palette
  pal <- brewer.pal(n = 9, name = name)
  
  # convert to a ramp
  ramp <- colorRampPalette(pal)
  
  # return this
  return (ramp)
  
}

# pal <- divRamp()
# plot(1:1000, pch = 16, cex = 5, col = pal(1000))
