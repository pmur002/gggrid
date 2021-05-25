
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

GeomGridGroup <- ggproto("GeomGridGroup", Geom,
                         ## No required_aes
                         ## Allow values to be passed in
                         default_aes = aes(x = NULL),
                         ## No draw_key
                         draw_group = draw_grid_group)
	
grid_group <- function(grob=nullGrob(),
                       mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", inherit.aes = TRUE,
                       debug = NULL, ...) {
    if (!is.grob(grob) && !is.function(grob))
        stop("Invalid 'grob' argument;  must be grob or function")
    if (!is.null(debug) && !is.function(debug))
        stop("Invalid 'debug' argument:  must be NULL or function")
    layer(geom = GeomGridGroup,
          mapping = mapping,
          data = data, 
          stat = stat, 
          position = position,
          inherit.aes = inherit.aes,
          show.legend = FALSE, 
          params = list(grob = grob, debug = debug, ...))
}

