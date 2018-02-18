node_myInner <- function (ctreeobj, digits = 3, abbreviate = FALSE, fill = "white", 
          pval = TRUE, id = TRUE) 
{
  getLabel1 <- function(x) {
    if (x$terminal) 
      return(rep.int("", 2))
    varlab <- ifelse(abbreviate > 0, abbreviate(x$psplit$variableName, 
                                                as.numeric(abbreviate)), x$psplit$variableName)
    if (pval) {
      pvalue <- 1 - x$criterion$maxcriterion
      plab <- ifelse(pvalue < 10^(-digits), paste("p <", 
                                                  10^(-digits)), paste("p =", round(pvalue, digits = digits)))
    }
    else {
      plab <- ""
    }
    return(c(varlab, plab))
  }
  maxstr <- function(node) {
    lab <- getLabel1(node)
    msl <- ifelse(node$terminal, "", maxstr(node$left))
    msr <- ifelse(node$terminal, "", maxstr(node$right))
    lab <- c(lab, msl, msr)
    return(lab[which.max(nchar(lab))])
  }
  nstr <- maxstr(ctreeobj@tree)
  rval <- function(node) {
    node_vp <- viewport(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                        width = unit(1, "strwidth", nstr) * 1.3,
                        height = unit(3, "lines"),
                        name = paste("node_inner", node$nodeID, sep = ""))
    pushViewport(node_vp)
    xell <- c(seq(0, 0.2, by = 0.01), seq(0.2, 0.8, by = 0.05), seq(0.8, 1, by = 0.01))
    yell <- sqrt(xell * (1 - xell))
    lab <- getLabel1(node)
    fill <- rep(fill, length.out = 2)
    grid.polygon(x = unit(c(xell, rev(xell)), "npc"), 
                 y = unit(c(yell, -yell) + 0.5, "npc"), 
                 gp = gpar(fill = fill[1]))

    grid.text(lab[1], y = unit(1.5 + 0.5 * pval, "lines"), gp = gpar(fontsize=13.5, fontface="bold"))
    if (pval) 
      grid.text(lab[2], y = unit(1, "lines"))
    if (id) {
      nodeIDvp <- viewport(x = unit(0.5, "npc"), y = unit(1, 
                                                          "npc"), width = max(unit(1, "lines"), unit(1.3, 
                                                                                                     "strwidth", as.character(node$nodeID))), height = max(unit(1, 
                                                                                                                                                                "lines"), unit(1.3, "strheight", as.character(node$nodeID))))
      pushViewport(nodeIDvp)
      grid.rect(gp = gpar(fill = fill[2]))
      grid.text(node$nodeID)
      popViewport()
    }
    upViewport()
  }
  return(rval)
}