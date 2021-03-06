###
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
#' Multiplot function
#'
#' This function allow putting multiple ggplots in an array of k columns and plotting them.
#' Requires the grid package.
#' @param plotlist A list of ggplot objects. Defaults to NULL.
#' @param file A file in which to save the multiplot.
#' @param cols The number of columns, defaults to 1. Only used when layout is NULL.
#' @param layout Defaults to NULL. Specifies a
#' @keywords ggplot2, multiplot, multiple
#' @examples
#' multiplot(plotlist = c(aplot, anotherplot, athirdplot), cols = 3)
#' multiplot(plotlist = c(aplot, anotherplot, athirdplot, finalplot), cols = 2)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }

    if (numPlots==1) {
        print(plots[[1]])

    } else {
        # Set up the page
        grid::grid.newpage()
        grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))

        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

            print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}


