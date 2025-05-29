#'Interactive plots for Rsim
#'
#' @description
#'Interactive plots for Rsim plot using plotly:: package and tidyverse::
#'
#'add another paragraph
#'
#' @param Rpath.obj An object of class Rpath \code{rpath()}.
#' @param eco.name Optional name of the ecosystem.  Default is the `eco.name` attribute from the
#'  rpath object created from running \code{rpath()}.
#' @param line.col description.
#' @param h_spacing horizontal spacing multiplier, it spreads nodes horizontally.
#' @param node_size_min from scale_value() Minimum value of the new scale (default is 1).
#' @param node_size_max from scale_value() Maximum value of the new scale (default is 30).
#' @param fleet_color single color for fleet nodes.
#' @param groups_palette color palette for non-fleet groups. Default is a vector of colors.
#' @param text_size size of the text labels.
#'
#' @return Returns a plot visualization of the food web.
#'
#' @importFrom magrittr `%>%`
#'
#' @section Contributors:
#' webplot original function from Rpath by Kerim Aydin
#'
#' @section Plotting:
#' Caution: For large food webs see the examples.
#'
#' @examples
#' \dontrun{
#' # Read in Rpath parameter file, generate and name model object
#' Rpath.obj <- Rpath::rpath(Rpath::AB.params, eco.name = "Anchovy Bay")
#' # Plot food web diagram with all groups labeled, including fleets
#' webplot_rpathviz(Rpath.obj, h_spacing = 3, text_size = 3)
#'
#' # Read in Rpath parameter file, generate and name model object
#' Rpath.obj <- Rpath::rpath(Rpath::Ecosense.EBS, eco.name = "Eastern Bering Sea")
#' # Plot food web diagram with all groups labeled, including fleets. Follow the steps
#' # 1) assign the plot to an object
#' # 2) use ggplot2::ggsave() to save the plot for a fast visualization
#' wp <- webplot_rpathviz(Rpath.obj, h_spacing = 3, text_size = 3)
#' ggplot2::ggsave("figures/EBSfoodwebplot2.png", p , width= 16, height= 10)
#'
#'
#' }
#'
#' @export


webplot_rpathviz <- function(Rpath.obj,
                                 eco.name = attr(Rpath.obj, "eco.name"),
                                 line.col = "grey",
                                 h_spacing = 3,
                                 node_size_min = 1,
                                 node_size_max = 30,
                                 fleet_color = "#B40F20", # single color for fleet nodes
                                 groups_palette = c("#EC7604", "#CB7A5C", "#5785C1", "#0B775E"),
                                 text_size= 3)
{

  #Number of groups check to determine function plot
  if (Rpath.obj$NUM_GROUPS > 20) {
    message("Your food web object has more than 20 functional groups.
    \nPlotting to the RStudio window will take a while, please be patient...
    Refer to the examples for large food webs.")
  }

  colors_net <- grDevices::colorRampPalette(groups_palette)

  # Building the nodes with Rpath object.
  nodes <- tibble::tibble(
    GroupNum = 1:length(Rpath.obj$TL),
    Group    = Rpath.obj$Group,
    type     = Rpath.obj$type,
    TL       = as.numeric(Rpath.obj$TL),
    Biomass  = as.numeric(Rpath.obj$Biomass)
  ) %>%
    dplyr::mutate(id = GroupNum) %>%
    # Always convert Group to character then factor then numeric
    dplyr::mutate(group = as.numeric(as.factor(as.character(Group))))
  # Calculate tot.catch and filter out fleet (type 3) nodes with no tot.catch.
  tot.catch <- Rpath.obj$Landings + Rpath.obj$Discards
  nodes <- nodes %>%
    dplyr::mutate(fleet_tot =
                    dplyr::if_else(type == 3,
                                   purrr::map_dbl((GroupNum - (Rpath.obj$NUM_GROUPS - Rpath.obj$NUM_GEARS)),
                                                  ~sum(tot.catch[, .x])),
                                   NA_real_)) %>%
    dplyr::filter(!(type == 3 & fleet_tot == 0))

  # Compute node size based on Biomass.
  nodes <- nodes %>%
    dplyr::mutate(node_size = scale_value(Biomass, node_size_min = 10, node_size_max = 30))

  # Build the edge list using the original node IDs.
  allowed_ids <- nodes$id
  predators <- dplyr::filter(nodes,!(type %in% c(1, 2)))

  edge_list <- purrr::map_dfr(predators$id, function(i) {
    node_type <- Rpath.obj$type[i]
    if (node_type == 0) {
      prey_indices <- base::which(Rpath.obj$DC[, i] > 0)
    } else if (node_type == 3) {
      gear.num <- i - (Rpath.obj$NUM_GROUPS - Rpath.obj$NUM_GEARS)
      # Pre-compute the sum for this gear
      tot_val <- base::sum(tot.catch[, gear.num])
      if (tot_val == 0)
        return(NULL)
      prey_indices <- base::which(tot.catch[, gear.num] > 0)
    } else {
      prey_indices <- base::integer(0)
    }
    # Only keep prey that exist in allowed_ids.
    prey_indices <- base::intersect(prey_indices, allowed_ids)
    if (length(prey_indices) > 0) {
      tibble::tibble(from = i,
             to = prey_indices,
             width = nodes$Biomass[nodes$id == i] / 10)  # adjust scaling as needed
    } else {
      NULL
    }
  })

  # Create an edge attribute for gradient mapping; here we copy "width"
  edge_list <- edge_list %>% dplyr::mutate(edge_stat = width)

  # Re-index nodes to have sequential IDs
  nodes <- nodes %>%
    dplyr::arrange(id) %>%
    dplyr::mutate(new_id =
                    dplyr::row_number())
  map_ids <- nodes %>%
    dplyr::select(old_id = id, new_id)

  edge_list <- edge_list %>%
    dplyr::inner_join(map_ids, by = c("from" = "old_id")) %>%
    dplyr::rename(from_new = new_id) %>%
    dplyr::inner_join(map_ids, by = c("to" = "old_id")) %>%
    dplyr::rename(to_new = new_id) %>%
    dplyr::mutate(from = from_new, to = to_new) %>%
    dplyr::select(from, to, width, edge_stat)

  nodes <- nodes %>%
    dplyr::mutate(id = new_id)

  # Create the tidygraph object
  graph_obj <- tidygraph::tbl_graph(nodes = nodes,
                         edges = edge_list,
                         directed = TRUE)

  # Compute cluster betweenness using igraph
  graph_ig <- igraph::as.igraph(graph_obj)
  clust <- igraph::cluster_edge_betweenness(graph_ig)
  mem <- igraph::membership(clust)
  # Add cluster membership to nodes
  graph_obj <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    dplyr::mutate(cluster = as.factor(mem)) #FLAG ####
  # Override cluster for fleet nodes: if type==3, assign cluster = "fleet"
  graph_obj <- graph_obj %>% tidygraph::activate(nodes) %>%
    dplyr::mutate(cluster = dplyr::if_else(type == 3, "fleet", as.character(cluster)))

  # Create a layout using KK
  #(Here I set kk, but would be nice to have the user decide between a few layouts kk, fr, etc.)
  lay <- ggraph::create_layout(graph_obj, layout = "kk")
  if (!"TL" %in% colnames(lay)) {
    lay <- dplyr::left_join(lay,
                            tidygraph::as_tibble(graph_obj, what = "nodes"), by = "id")
  }
  lay$x <- lay$x * h_spacing      # Spread nodes horizontally
  lay$y <- base::as.numeric(lay$TL)  # Adjust vertical spacing

  y_min <- base::min(lay$y, na.rm = TRUE)
  y_max <- base::max(lay$y, na.rm = TRUE)

  # Create a color mapping for node clusters
  # Get all unique cluster values
  node_levels <- base::sort(
    base::unique(
    tidygraph::activate(graph_obj, nodes) %>%
      dplyr::pull(cluster)))
  # Separate fleets from non-fleet clusters
  nonfleet_levels <- base::setdiff(node_levels, "fleet")
  # Assign colors to non-fleet clusters using the palette
  nonfleet_colors <- colors_net(length(nonfleet_levels))
  # Combine with a fixed color for fleets
  color_mapping <- c("fleet" = fleet_color,
                     stats::setNames(nonfleet_colors, nonfleet_levels))

  ggraph::set_graph_style(plot_margin = ggplot2::margin(30, 30, 30, 30))
  jitter <- ggplot2::position_jitter(width = 0.1, height = 0.1)

#browser() #for checks
  # Build the ggraph plot
  p <- ggraph::ggraph(lay) +
    ggraph::geom_edge_link(
      ggplot2::aes(edge_width = edge_list$width,
                   color = ggplot2::after_stat(index)),
                   lineend = "round",
                   alpha = 0.30) +
    ggraph::scale_edge_colour_gradient(low = "#ffd06f", high = "#aadce0", guide = "legend") +
    ggraph::geom_edge_loop(ggplot2::aes(edge_width = width, color = ggplot2::after_stat(index)),
                   alpha = 0.85,
                   lineend = "round") +
    ggraph::scale_edge_width(range = c(0.2, 10)) +
    ggraph::geom_node_point(ggplot2::aes(size = node_size), color = "white") +
    ggraph::geom_node_point(ggplot2::aes(
      alpha = 0.8,
      color = cluster,
      size = node_size
    )) +
    ggplot2::scale_size(range = c(1, base::max(nodes$node_size, na.rm = TRUE))) +
    ggplot2::scale_color_manual(values = color_mapping) +
    ggraph::geom_node_text(
      ggplot2::aes(label = Group),
      size = text_size,
      color = "gray15",
      repel = TRUE,
      check_overlap = TRUE,
      point.padding = ggplot2::unit(0.95, "lines"),
      segment.size = 0.25,
      max.overlaps = Inf
    ) +
    ggplot2::labs(y = "Trophic Level", title = eco.name) +
    ggplot2::scale_y_continuous(breaks = base::seq(floor(y_min),
                                                   base::ceiling(y_max), by = 1),
                       expand = ggplot2::expansion(c(0.10, 0.10))) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(c(0.10, 0.10))) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank()
    )

  return(p)
}

#' Function to scale node size based on Biomass
#'
#' @param x Numeric vector of values to be scaled.
#' @param orig_min Minimum value of the original scale (default is minimum of x).
#' @param orig_max Maximum value of the original scale (default is maximum of x).
#' @param node_size_min Minimum value of the new scale (default is 1).
#' @param node_size_max Maximum value of the new scale (default is 30).
#'
#' @noRd

scale_value <- function(x,
                        orig_min = min(x),
                        orig_max = max(x),
                        node_size_min = 1,
                        node_size_max = 30) {
  node_size_min + ((x - orig_min) / (orig_max - orig_min)) * (node_size_max - node_size_min)
}
