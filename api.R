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


#* Make some kind of Iris Clustering plot that I don't understand
#* @png
#* @get /plot
function() {
  # devtools::install_github("hrbrmstr/ggalt")
  library(ggplot2)
  library(ggalt)
  library(ggfortify)
  theme_set(theme_classic())
  
  # Compute data with principal components ------------------
  df <- iris[c(1, 2, 3, 4)]
  pca_mod <- prcomp(df)  # compute principal components
  
  # Data frame of principal components ----------------------
  df_pc <- data.frame(pca_mod$x, Species=iris$Species)  # dataframe of principal components
  df_pc_vir <- df_pc[df_pc$Species == "virginica", ]  # df for 'virginica'
  df_pc_set <- df_pc[df_pc$Species == "setosa", ]  # df for 'setosa'
  df_pc_ver <- df_pc[df_pc$Species == "versicolor", ]  # df for 'versicolor'
  
  # Plot ----------------------------------------------------
  g <- ggplot(df_pc, aes(PC1, PC2, col=Species)) + 
    geom_point(aes(shape=Species), size=2) +   # draw points
    labs(title="Iris Clustering", 
         subtitle="With principal components PC1 and PC2 as X and Y axis",
         caption="Source: Iris") + 
    coord_cartesian(xlim = 1.2 * c(min(df_pc$PC1), max(df_pc$PC1)), 
                    ylim = 1.2 * c(min(df_pc$PC2), max(df_pc$PC2))) +   # change axis limits
    geom_encircle(data = df_pc_vir, aes(x=PC1, y=PC2)) +   # draw circles
    geom_encircle(data = df_pc_set, aes(x=PC1, y=PC2)) + 
    geom_encircle(data = df_pc_ver, aes(x=PC1, y=PC2))
  print(g)
}
