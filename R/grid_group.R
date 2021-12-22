
draw_grid_group <- function(data, panel_params, coord, grob, debug) {
    if (!is.null(debug)) {
        coords <- coord$transform(data, panel_params)
        debug(data, coords)
    }
    if (is.grob(grob)) {
        grob
    } else {
        if (!is.function(grob))
            stop("Invalid grob in GeomPanel")
        coords <- coord$transform(data, panel_params)
        grob(data, coords)
    }
}

grid_group <- function(grob=nullGrob(),
                       mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", inherit.aes = TRUE,
                       show.legend = FALSE,
                       key_glyph = NULL,
                       debug = NULL, ...) {
    if (!is.grob(grob) && !is.function(grob))
        stop("Invalid 'grob' argument;  must be grob or function")
    if (!is.null(debug) && !is.function(debug))
        stop("Invalid 'debug' argument:  must be NULL or function")
    ## Protection against bug in 'ggplot2' 3.3.5 (fixed in later versions)
    if (is.null(mapping)) {
        required_aes <- NULL
    } else {
        required_aes <- names(mapping)
    }
    if (packageVersion("ggplot2") <= "3.3.5") {
        default_aes <- aes(xmin = -Inf, xmax = -Inf)
    } else {
        default_aes <- aes()
    }
    GeomGridGroup <- ggproto("GeomGridGroup", Geom,
                             required_aes = required_aes,
                             default_aes = default_aes,
                             draw_group = draw_grid_group)
    layer(geom = GeomGridGroup,
          mapping = mapping,
          data = data, 
          stat = stat, 
          position = position,
          inherit.aes = inherit.aes,
          check.aes = FALSE,
          show.legend = show.legend,
          key_glyph = key_glyph,
          params = list(grob = grob, debug = debug, ...))
}

