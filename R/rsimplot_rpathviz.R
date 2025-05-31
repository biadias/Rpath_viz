#'Interactive plots for Rsim
#'
#' @description
#'Interactive plots for Rsim plot using plotly:: package and tidyverse::
#'
#'add another paragraph
#'
#' @param Rsim.output An object of class Rsim.
#' @param eco.name Optional name of the ecosystem.
#' @param spname A character vector of species names to plot. If "all", all species will be plotted and rel_bio will use the default FALSE.
#' @param rel_bio Logical value of whether to plot a single group biomass as relative or absolute value.
#' @param palette A character vector of colors to use.
#'
#' @return An interactive plotly object
#'
#' @section Contributors:
#' rsim.plot function from Rpath by Kerim Aydin
#'
#' @examples
#' \dontrun{
#' # create the Rsim object
#' library(Rpath)
#' runObj <- rsim.scenario(rpath(REco.params), REco.params, years= 1:50)
#' Rsim.output <- rsim.run(runObj, years= 1:50)
#' # plot the Rsim object two default color blind friendly palettes
#' rsimplot_rpathviz(Rsim.output, eco.name= "Anchovy Bay", spname = "all", palette= "rsim_pal_dark")
#' rsimplot_rpathviz(Rsim.output, eco.name= "Anchovy Bay", spname = "all", palette= "rsim_pal_light")
#' rsimplot_rpathviz(Rsim.output, eco.name= "Anchovy Bay", spname = "Foragefish2", rel_bio=FALSE, palette= "rsim_pal_light")
#'
#' }
#'
#' @export



rsimplot_rpathviz <- function(Rsim.output,
                                  eco.name = NULL,
                                  spname = "all",
                                  rel_bio = FALSE,
                                  palette = "rsim_pal_dark") {
  rsim_name <- base::deparse(substitute(Rsim.output))
  mrg <- list(
    l = 50,
    r = 50,
    b = 50,
    t = 50,
    pad = 20
  )

  if ("all" %in% spname) {
    spname <- base::colnames(Rsim.output$out_Biomass)[2:ncol(Rsim.output$out_Biomass)]
  }
  # Setting up to calculate relative biomass or absolute biomass in case of only one species is selected
  single_sp <- (base::length(spname) == 1)

  if (single_sp && !rel_bio) {

    spnum   <- base::which(Rsim.output$params$spname == spname)
    mat     <- Rsim.output$out_Biomass[, spnum, drop = FALSE]
    plot.bio <- mat
    val_name <- "Biomass"
  } else {
    # relative biomass for many species or for a single species in rel_bio mode
    mat       <- Rsim.output$out_Biomass[, spname, drop = FALSE]
    start.val <- mat[1, ]
    start.val[start.val == 0] <- 1
    plot.bio  <- base::sweep(mat, 2, start.val, "/")
    val_name  <- "RelativeBiomass"
  }

  # Create a time vector
  time <- base::seq_len(base::nrow(plot.bio))
  df <- base::cbind(time, base::as.data.frame(plot.bio))

  # Here is a little error msg for the user, in case they try to plot a species that is not in the output
  data_cols <- base::setdiff(names(df), "time")
  if (length(data_cols) == 0) {
    stop(
      "Hey, no species columns found to plot.\n",
      "  * Check your `spname` argument: ",
      paste0("'", paste(spname, collapse = "', '"), "'"),
      " not in out_Biomass."
    )
  }

  val_name <- if (rel_bio) "Biomass" else "RelativeBiomass"
  df_long <- tidyr::pivot_longer(
    df,
    cols      = all_of(data_cols),
    names_to  = "Species",
    values_to = val_name
  )

  df_long$Species <- base::factor(df_long$Species, levels = spname)

   n_species <- base::length(spname)

  rsim_pal_dark <-  c("#EC7604", "#EF5703" ,"#D37844" ,"#C07B65" ,"#967F89", "#6C83AE" ,"#4982AF" ,"#2D7D8B", "#117867", "#166158", "#254451", "#35274A")
  rsim_pal_light <- c("#FFAB88", "#FF9D8A", "#FFA57B", "#EFA793", "#C5AEB8", "#9DB3DF", "#82B4E2", "#71B2C1", "#64B1A0", "#68A097", "#738A97", "#807693")

  if (is.character(palette) && length(palette) == 1 &&
      palette %in% c("rsim_pal_dark", "rsim_pal_light")) {
    pal_vec <- get(palette)
    my_colors <- if (length(pal_vec) < n_species) {
      grDevices::colorRampPalette(pal_vec)(n_species)
    } else {
      pal_vec[1:n_species]
    }
  } else if (base::is.character(palette) && base::length(palette) == 1 &&
             exists(palette, mode = "function")) {
    # named base‐R palette function
    # If the palette is pre-determined e.g. "rainbow" or "terrain.colors"
    # https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf
    my_colors <- base::match.fun(palette)(n_species)
  } else if (base::is.character(palette) && base::length(palette) > 1) {
    # If you decide to give a vector of colors
    my_colors <- if (base::length(palette) < n_species) {
      # the function will interpolate if there not enough colors
      grDevices::colorRampPalette(palette)(n_species)
    } else {
      # If enough colors
      palette[1:n_species]
    }
  } else {
    # Default to rsim_pal_dark if palette is not specified
    my_colors <- if (length(rsim_pal_dark) < n_species) {
      grDevices::colorRampPalette(rsim_pal_dark)(n_species)
    } else {
      rsim_pal_dark[1:n_species]
    }
  }


  if (val_name == "Biomass") {
    title_text <- sprintf(
      "Absolute Biomass Over Time for %s in %s",
      spname, eco.name %||% rsim_name
    )
  } else {
    # relative case
    if (single_sp && rel_bio) {
      title_text <- sprintf(
        "Relative Biomass Over Time for %s in %s",
        spname, eco.name %||% rsim_name
      )
    } else {
      title_text <- sprintf(
        "Relative Biomass Over Time in %s",
        eco.name %||% rsim_name
      )
    }
  }

  # Plotly object
  rsim_int_plotly <- plotly::plot_ly(
    data = df_long,
    x = ~ time,
    y = as.formula(paste0("~", val_name)),
    color = ~ Species,
    colors = my_colors,
    type = 'scatter',
    mode = 'lines'
  ) %>%
    plotly::layout(
      title  = title_text,
      xaxis  = list(title = "Months"),
      yaxis  = list(
        title      = if (val_name == "Biomass") "Biomass" else "Relative Biomass"
      ),
      margin = mrg,
      legend = list(traceorder = "normal")
    )

  return(rsim_int_plotly)
}

