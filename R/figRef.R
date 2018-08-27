### Better figure numbering prefixing and captioning for use with RMarkdown and knitr
###
### Using: http://galahad.well.ox.ac.uk/repro/#better-figure-and-table-captions
### options(figcap.prefix = "Figure", figcap.sep = ":", figcap.prefix.highlight = "**")
###
###
### ```{r carDataPlot, fig.cap=figRef("carData", "Car speed and stopping distances from the 1920s.")}
###    plot(cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)", las = 1)
###    lines(lowess(cars$speed, cars$dist, f = 2/3, iter = 3), col = "red")
### ```
#' A Cat Function
#'
#' This function allows redefining figure captions in Rmarkdown/knits
#' It should be used in R chunk headers to hardcode figure captions and numbering.

figRef <- local({
    tag <- numeric()
    created <- logical()
    used <- logical()
    function(label, caption, prefix = options("figcap.prefix"),
             sep = options("figcap.sep"), prefix.highlight = options("figcap.prefix.highlight")) {
        i <- which(names(tag) == label)
        if (length(i) == 0) {
            i <- length(tag) + 1
            tag <<- c(tag, i)
            names(tag)[length(tag)] <<- label
            used <<- c(used, FALSE)
            names(used)[length(used)] <<- label
            created <<- c(created, FALSE)
            names(created)[length(created)] <<- label
        }
        if (!missing(caption)) {
            created[label] <<- TRUE
            paste0(prefix.highlight, prefix, " ", i, sep, prefix.highlight,
                   " ", caption)
        } else {
            used[label] <<- TRUE
            paste(prefix, tag[label])
        }
    }
})


