myEdgeSimple <- function (treeobj, digits = 3, abbreviate = FALSE, numInRow = 3) 
{
  function(split, ordered = FALSE, left = TRUE) {
    if (is.numeric(split)) 
      split <- round(split, digits = digits)
    if (is.character(split) & abbreviate > 0) 
      split <- abbreviate(split, as.numeric(abbreviate))
    if (!ordered) {
      if (length(split) > 1) {
        nrows <- ceiling(length(split)/numInRow)
        
        res <- "{"
        for (i in 1:nrows) {
          if (i == nrows) {
            myto <- length(split)%%numInRow
            if (myto == 0) myto <- numInRow
            res <- paste0(res, paste(split[(1:myto)+numInRow*(i-1)], collapse = ", "))
          } else {
            res <- paste0(res, paste(split[(1:numInRow)+numInRow*(i-1)], collapse = ", "), ",\n")
          }
        }
        split <- paste0(res, "}")
      }
      split <- gsub(":", "-", split)  
    }
    else {
      if (left) 
        split <- as.expression(bquote(phantom(0) <= .(split)))
      else split <- as.expression(bquote(phantom(0) > .(split)))
    }
    grid.rect(gp = gpar(fill = "white", col = 0), width = unit(1,"strwidth", split))
    grid.text(split, just = "center", gp = gpar(fontface="bold", fontsize=12))
  }
}