

geom_scatterpie_color <- 
  
  function (mapping = NULL, data, cols, pie_scale = 1, sorted_by_radius = FALSE, 
          legend_name = "type", long_format = FALSE, ...) 
{
  if (is.null(mapping)) 
    mapping <- aes_(x = ~x, y = ~y)
    mapping <- modifyList(mapping, aes_(r0 = 0, color = as.formula(paste0("~", 
                                                                        legend_name)), amount = ~value))
  if (!"r" %in% names(mapping)) {
    xvar <- get_aes_var(mapping, "x")
    size <- diff(range(data[, xvar]))/50 * pie_scale
    data$r <- size
    mapping <- modifyList(mapping, aes_(r = size))
  }
  names(mapping)[match(c("x", "y"), names(mapping))] <- c("x0", 
                                                          "y0")
  if (long_format == TRUE) {
    df <- data
    names(df)[which(names(df) == cols)] = legend_name
    cols2 <- enquo(cols)
  }
  else {
    data <- data[rowSums(data[, cols]) > 0, ]
    cols2 <- enquo(cols)
    df <- gather(data, "type", "value", !!cols2)
    df$type <- factor(df$type, levels = cols)
    names(df)[which(names(df) == "type")] = legend_name
  }
  if (!sorted_by_radius) {
    return(geom_arc_bar(mapping, data = df, stat = "pie", 
                        inherit.aes = FALSE, ...))
  }
  lapply(split(df, df$r)[as.character(sort(unique(df$r), decreasing = TRUE))], 
         function(d) {
           geom_arc_bar(mapping, data = d, stat = "pie", inherit.aes = FALSE, 
                        ...)
         })
}