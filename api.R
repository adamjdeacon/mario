#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Give some info about a card
#* @param card the card to test
#* @get /issuer
function(card="") {
  checkLuhn::issuer(card)
}

#* Draws a pretty graph
#* @png
#* @get /make-me-a-graph
function() {
  options(scipen=999)  # turn-off scientific notation like 1e+48
  
  theme_set(theme_bw())  # pre-set the bw theme.
  data("midwest", package = "ggplot2")
  # midwest <- read.csv("http://goo.gl/G1K41K")  # bkup data source
  
  # Scatterplot
  gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
    geom_point(aes(col=state, size=popdensity)) + 
    geom_smooth(method="loess", se=F) + 
    xlim(c(0, 0.1)) + 
    ylim(c(0, 500000)) + 
    labs(subtitle="Area Vs Population", 
         y="Population", 
         x="Area", 
         title="Scatterplot", 
         caption = "Source: midwest")
  
  plot(gg)
}

