myplotbetadisper <- function (x, axes = c(1, 2), cex = 0.7, pch = seq_len(ng), col = NULL, 
    lty = "solid", lwd = 1, hull = TRUE, ellipse = FALSE, conf, 
    segments = TRUE, seg.col = "grey", seg.lty = lty, seg.lwd = lwd, 
    label = TRUE, label.cex = 1, ylab, xlab, main, sub, 
    fillrect="white", coltextrect="black", alphaPoints=0.5, 
    labPoints=NULL, poslabPoints=4, ...) 
{
    localAxis <- function(..., col, bg, pch, cex, lty, lwd) axis(...)
    localBox <- function(..., col, bg, pch, cex, lty, lwd) box(...)
    localTitle <- function(..., col, bg, pch, cex, lty, lwd) title(...)
    Ellipse <- function(scrs, centres, conf, col, lty, lwd, ...) {
        mat <- cov.wt(scrs, center = centres)
        if (mat$n.obs == 1) 
            mat$cov[] <- 0
        xy <- if (mat$n.obs > 1) {
            vegan:::veganCovEllipse(mat$cov, mat$center, conf)
        }
        else {
            scrs
        }
        vegan:::ordiArgAbsorber(xy, FUN = lines, col = col, lty = lty, 
            lwd = lwd, ...)
    }
    if (missing(main)) 
        main <- deparse(substitute(x))
    if (missing(sub)) 
        sub <- paste("method = \"", attr(x, "method"), "\"", 
            sep = "")
    if (missing(xlab)) 
        xlab <- paste("PCoA", axes[1])
    if (missing(ylab)) 
        ylab <- paste("PCoA", axes[2])
    t <- if (missing(conf)) {
        1
    }
    else {
        sqrt(qchisq(conf, df = 2))
    }
    g <- scores(x, choices = axes)
    ng <- length(levels(x$group))
    lev <- levels(x$group)
    if (is.null(col)) {
        col <- palette()
    }
    col <- rep_len(col, ng)
    colpts <- apply(col2rgb(col), 2, addAlpha, alpha=alphaPoints)
    seg.col <- rep_len(seg.col, ng)
    plot(g$sites, asp = 1, type = "n", axes = FALSE, ann = FALSE, 
        ...)
    if (is.matrix(g$centroids)) {
        for (i in seq_along(lev)) {
            curlev <- lev[i]
            take <- x$group == curlev
            j <- which(lev == curlev)
            if (segments) {
                segments(g$centroids[j, 1L], g$centroids[j, 2L], 
                  g$sites[take, 1L], g$sites[take, 2L], col = seg.col[i], 
                  lty = seg.lty, lwd = seg.lwd)
            }
            if (hull) {
                ch <- chull(g$sites[take, ])
                ch <- c(ch, ch[1])
                lines(x$vectors[take, axes][ch, ], col = col[i], 
                  lty = lty, lwd = lwd, ...)
            }
            if (ellipse) {
                Ellipse(g$sites[take, , drop = FALSE], centres = g$centroids[j, 
                  ], conf = t, col = col[i], lty = lty, lwd = lwd, 
                  ...)
            }
            points(g$centroids[j, , drop = FALSE], pch = 16, 
                cex = 1, col = col[i], ...)
        }
    }
    else {
        if (segments) {
            segments(g$centroids[, 1L], g$centroids[, 2L], g$sites[, 
                1L], g$sites[, 2L], col = seg.col, lty = seg.lty, 
                ...)
        }
        if (hull) {
            ch <- chull(g$sites)
            ch <- c(ch, ch[1])
            lines(x$vectors[, axes][ch, ], col = col[1L], lty = lty, 
                lwd = lwd, ...)
        }
        if (ellipse) {
            Ellipse(g$sites, centres = g$centroids, conf = t, 
                col = col[1L], lty = lty, lwd = lwd, ...)
        }
        points(g$centroids[, 1L], g$centroids[, 2L], pch = 16, 
            cex = 1, col = col[1L], ...)
    }
    points(g$sites, pch = pch[x$group], cex = cex, col = col[x$group], 
        ...)
    if (!is.null(labPoints)) {
       text(g$sites, labels=labPoints, pos=poslabPoints,
            cex = cex, col = col[x$group])
    }
    if (label) {
        myordilabel(x, display = "centroids", choices = axes, cex = label.cex, fill=fillrect, col=coltextrect)
    }
    localTitle(main = main, xlab = xlab, ylab = ylab, sub = sub, 
        ...)
    localAxis(1, ...)
    localAxis(2, ...)
    localBox(...)
    class(g) <- "ordiplot"
    invisible(g)
}


myordilabel <- function (x, display, labels, choices = c(1, 2), priority, select, 
    cex = 0.8, fill = "white", border = NULL, col = NULL, xpd = TRUE, 
    ...) 
{
    if (missing(display)) 
        display <- "sites"
    x <- scores(x, choices = choices, display = display, ...)
    if (missing(labels)) 
        labels <- rownames(x)
    if (!missing(select)) {
        x <- .checkSelect(select, x)
        labels <- .checkSelect(select, labels)
    }
    if (!missing(priority)) {
        if (!missing(select)) 
            priority <- priority[select]
        ord <- order(priority)
        x <- x[ord, ]
        labels <- labels[ord]
    }
    else {
        ord <- seq_along(labels)
    }
    em <- strwidth("m", cex = cex, ...)
    ex <- strheight("x", cex = cex, ...)
    w <- (strwidth(labels, cex = cex, ...) + em/1.5)/2
    h <- (strheight(labels, cex = cex, ...) + ex/1.5)/2
    if (is.null(col)) 
        if (!is.null(border)) 
            col <- border
        else col <- par("fg")
    col <- rep(col, length = nrow(x))[ord]
    if (!is.null(border)) 
        border <- rep(border, length = nrow(x))[ord]
    fill <- rep(fill, length = nrow(x))[ord]
    for (i in 1:nrow(x)) {
        vegan:::ordiArgAbsorber(x[i, 1] + c(-1, 1, 1, -1) * w[i], x[i, 
            2] + c(-1, -1, 1, 1) * h[i], col = fill[i], border = border[i], 
            xpd = xpd, FUN = polygon, ...)
        vegan:::ordiArgAbsorber(x[i, 1], x[i, 2], labels = labels[i], 
            cex = cex, col = col[i], xpd = xpd, FUN = text, ...)
    }
    invisible(x)
}

addAlpha <- function(col, alpha) {
  alpha <- round(alpha*255)
  rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
}