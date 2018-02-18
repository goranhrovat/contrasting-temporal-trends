node_myPlot2 <- function (mobobj, which = NULL, col = "black", linecol = "red", 
          cex = 0.5, pch = 19, jitter = FALSE, xscale = NULL, yscale = NULL, 
          ylines = 1.5, id = TRUE, labels = FALSE, xlab, ylab, xskala, yskala, addGrowthInfo=T) 
{
  y <- response(mobobj)
  
  y <- y[[1]]
  if (is.factor(y)) y <- as.numeric(y) - 1
  
  surv <- FALSE
  if (is.null(pch)) pch <- 1
  
  y <- as.numeric(y)
  pch <- rep(pch, length.out = length(y))
  if (jitter) y <- jitter(y)
  x <- model.matrix(mobobj@tree$model)
  if (is.null(which)) {
    which <- if (NCOL(x) > 1) 
      2:NCOL(x)
    else 1
  }
  x <- x[, which, drop = FALSE]
  k <- NCOL(x)
  if (is.null(xscale)) 
    xscale <- apply(x, 2, function(xi) range(xi) + c(-0.1, 0.1) * diff(range(xi)))
  else xscale <- matrix(xscale)
  if (is.null(yscale)) 
    yscale <- range(y) + c(-0.1, 0.1) * diff(range(y))
  rval <- function(node) {
    y <- rep.int(y, node$weights)
    yhat <- fitted(node$model)
    if (!surv) 
      yhat <- rep.int(yhat, node$weights)
    pch <- rep.int(pch, node$weights)
    top_vp <- viewport(layout = grid.layout(nrow = 2 * k, 
                                            ncol = 3, 
                                            widths = unit(c(ylines, 1, 1), c("lines", "null", "lines")), 
                                            heights = unit(rep.int(c(1.5, 1), k) - c(1.5, rep.int(0, 2 * k - 1)), 
                                                           rep.int(c("lines", "null"), k))),
                       width = unit(0.87, "npc"),
                       height = unit(0.78, "npc") - unit(1, "lines"),
                       name = paste("node_scatterplot", node$nodeID, sep = ""))
    
    pushViewport(top_vp)
    grid.rect(gp = gpar(fill = "white", col = 0))
    top <- viewport(layout.pos.col = 2, layout.pos.row = 1)
    pushViewport(top)
    mainlab <- paste(ifelse(id, paste("Node", node$nodeID, 
                                      "(n = "), ""), sum(node$weights), ifelse(id, ")", 
                                                                               ""), sep = "")
    #grid.text(mainlab)
    minX <- min(xskala)
    maxX <- max(xskala)
    koef <- node$model$coefficients
    startX <- koef[1] + koef[2]*minX
    endX <- koef[1] + koef[2]*maxX
    porast <- (endX/startX-1)*100
    porast <- abs(round(porast,1))*sign(koef[2])
    
    if (addGrowthInfo) {
      modelTitle <- paste(paste(c("x0 =", "k ="), round(koef,8), collapse = "\n"), paste0("(", porast,"%)"))
      grid.text(modelTitle, just = c("center", "center"), vjust = -0.1, gp=gpar(fontsize=10))
    }
    
    popViewport()
    for (i in 1:k) {
      plot_vpi <- viewport(layout.pos.col = 2, layout.pos.row = 2 * 
                             i, xscale = xscale[, i], yscale = yscale, name = paste("node_scatterplot", 
                                                                                    i, node$nodeID, "plot", sep = ""))
      pushViewport(plot_vpi)
      xi <- rep.int(x[, i], node$weights)
      oi <- order(xi)
      grid.points(xi, y, gp = gpar(col = col, cex = cex), pch = pch)
      grid.lines(xi[oi], yhat[oi], default.units = "native", gp = gpar(col = linecol, lwd=3))
      
      #grid.xaxis(at = c(ceiling(xscale[1, i] * 10), floor(xscale[2, i] * 10))/10)
      grid.xaxis(at = xskala, gp = gpar(fontsize=12.5))
      #grid.yaxis(at = c(ceiling(yscale[1]), floor(yscale[2])))
      grid.yaxis(at = yskala, label = sub("[\\.]", ",", format(yskala, digits=2)), gp = gpar(fontsize=12.5))
      
      if (labels) {
        grid.text(xlab, x = unit(0.5, "npc"), y = unit(-2.2, "lines"), gp = gpar(fontface="bold", fontsize=14))
        grid.text(ylab, y = unit(0.5, "npc"), x = unit(-3.9, "lines"), rot = 90, gp = gpar(fontface="bold", fontsize=14))
      }
      grid.rect(gp = gpar(fill = "transparent"))
      upViewport()
    }
    upViewport()
  }
  return(rval)
}