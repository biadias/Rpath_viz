#------------------------------------------------------------------------------#
#AUTHORS: Bia Dias
#ORIGINAL AUTHORS: webplot() function from Rpath by Kerim Aydin
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#
# Foodweb Network plot Function ggraph:: igraph:: and tidyverse::
#------------------------------------------------------------------------------#

ggraph_webplot_Rpath <- function(Rpath.obj,
                                 eco.name = attr(Rpath.obj, "eco.name"),
                                 line.col = "grey",
                                 h_spacing = 3,
                                 # horizontal spacing multiplier
                                 fleet_color = "#B40F20", # single color for fleet nodes
                                 text_size= 3)
  {

  library(tidyverse)
  library(tidygraph)
  library(ggraph)
  library(purrr)
  library(igraph)

  # Function to scale node size based on Biomass
  scale_value <- function(x,
                          orig_min = min(x),
                          orig_max = max(x),
                          new_min = 1,
                          new_max = 30) {
    new_min + ((x - orig_min) / (orig_max - orig_min)) * (new_max - new_min)
  }

  # Define a color palette generator for non-fleet clusters
  #colors_net <- colorRampPalette(c("#3A9AB2", "#6FB2C1", "#91BAB6", "#A5C2A3",
  #                                 "#BDC881", "#DCCB4E", "#E3B710", "#E79805",
  #                                 "#EC7A05", "#EF5703"))

  colors_net <- colorRampPalette(c("#EC7604" , "#CB7A5C", "#5785C1", "#0B775E"))

 #Building the nodes with Rpath object.
  nodes <- tibble(
    GroupNum = 1:length(Rpath.obj$TL),
    Group    = Rpath.obj$Group,
    type     = Rpath.obj$type,
    TL       = as.numeric(Rpath.obj$TL),
    Biomass  = as.numeric(Rpath.obj$Biomass)
  ) %>%
    mutate(id = GroupNum) %>%
    # Always convert Group to character then factor then numeric
    mutate(group = as.numeric(as.factor(as.character(Group))))

  # Calculate tot.catch and filter out fleet (type 3) nodes with no tot.catch.
  tot.catch <- Rpath.obj$Landings + Rpath.obj$Discards
  nodes <- nodes %>%
    mutate(fleet_tot = if_else(type == 3, sapply((GroupNum - (
      Rpath.obj$NUM_GROUPS - Rpath.obj$NUM_GEARS
    )), function(j)
      sum(tot.catch[, j])), NA_real_)) %>%
    filter(!(type == 3 & fleet_tot == 0))

  # Compute node size based on Biomass.
  nodes <- nodes %>% mutate(node_size = scale_value(Biomass, new_min = 10, new_max = 50))

  # Build the edge list using the original node IDs.
  allowed_ids <- nodes$id
  predators <- nodes %>% filter(!(type %in% c(1, 2)))

  edge_list <- map_dfr(predators$id, function(i) {
    node_type <- Rpath.obj$type[i]
    if (node_type == 0) {
      prey_indices <- which(Rpath.obj$DC[, i] > 0)
    } else if (node_type == 3) {
      gear.num <- i - (Rpath.obj$NUM_GROUPS - Rpath.obj$NUM_GEARS)
      # Pre-compute the sum for this gear
      tot_val <- sum(tot.catch[, gear.num])
      if (tot_val == 0)
        return(NULL)
      prey_indices <- which(tot.catch[, gear.num] > 0)
    } else {
      prey_indices <- integer(0)
    }
    # Only keep prey that exist in allowed_ids.
    prey_indices <- intersect(prey_indices, allowed_ids)
    if (length(prey_indices) > 0) {
      tibble(from = i,
             to = prey_indices,
             width = nodes$Biomass[nodes$id == i] / 10)  # adjust scaling as needed
    } else {
      NULL
    }
  })

      # Create an edge attribute for gradient mapping; here we copy "width"
      edge_list <- edge_list %>% mutate(edge_stat = width)

      # Re-index nodes to have sequential IDs
      nodes <- nodes %>% arrange(id) %>% mutate(new_id = row_number())
      map_ids <- nodes %>% select(old_id = id, new_id)

      edge_list <- edge_list %>%
        inner_join(map_ids, by = c("from" = "old_id")) %>%
        rename(from_new = new_id) %>%
        inner_join(map_ids, by = c("to" = "old_id")) %>%
        rename(to_new = new_id) %>%
        mutate(from = from_new, to = to_new) %>%
        select(from, to, width, edge_stat)

      nodes <- nodes %>% mutate(id = new_id)

      # Create the tidygraph object
      graph_obj <- tbl_graph(nodes = nodes,
                             edges = edge_list,
                             directed = TRUE)

      # Compute cluster betweenness using igraph
      graph_ig <- as.igraph(graph_obj)
      clust <- cluster_edge_betweenness(graph_ig)
      mem <- membership(clust)
      # Add cluster membership to nodes
      graph_obj <- graph_obj %>% activate(nodes) %>% mutate(cluster = as.factor(mem))
      # Override cluster for fleet nodes: if type==3, assign cluster = "fleet"
      graph_obj <- graph_obj %>% activate(nodes) %>%
        mutate(cluster = if_else(type == 3, "fleet", as.character(cluster)))

      # Create a layout using KK
      lay <- create_layout(graph_obj, layout = "kk")
      if (!"TL" %in% colnames(lay)) {
        lay <- left_join(lay, as_tibble(graph_obj, what = "nodes"), by = "id")
      }
      lay$x <- lay$x * h_spacing      # Spread nodes horizontally
      lay$y <- as.numeric(lay$TL)  # Adjust vertical spacing

      y_min <- min(lay$y, na.rm = TRUE)
      y_max <- max(lay$y, na.rm = TRUE)

      # Create a color mapping for node clusters
      # Get all unique cluster values
      node_levels <- sort(unique(activate(graph_obj, nodes) %>% pull(cluster)))
      # Separate fleets from non-fleet clusters
      nonfleet_levels <- setdiff(node_levels, "fleet")
      # Assign colors to non-fleet clusters using the palette
      nonfleet_colors <- colors_net(length(nonfleet_levels))
      # Combine with a fixed color for fleets
      color_mapping <- c("fleet" = fleet_color, setNames(nonfleet_colors, nonfleet_levels))

      set_graph_style(plot_margin = margin(30, 30, 30, 30))
      jitter <- position_jitter(width = 0.1, height = 0.1)

      # Build the ggraph plot
      p <- ggraph(lay) +
        geom_edge_link(aes(edge_width = width, color = after_stat(index)),
                       lineend = "round",
                       alpha = 0.30) +
        scale_edge_colour_gradient(low = "#ffd06f", high = "#aadce0") +
        geom_edge_loop(aes(edge_width = width, color = after_stat(index)),
                       alpha = 0.85,
                       lineend = "round") +
        scale_edge_width(range = c(0.2, 10)) +
        geom_node_point(aes(size = node_size), color = "white") +
        geom_node_point(aes(
          alpha = 0.8,
          color = cluster,
          size = node_size
        )) +
        scale_size(range = c(1, max(nodes$node_size, na.rm = TRUE))) +
        scale_color_manual(values = color_mapping) +
        geom_node_text(
          aes(label = Group),
          size = text_size,
          color = "gray15",
          repel = TRUE,
          check_overlap = TRUE,
          point.padding = unit(0.95, "lines"),
          segment.size = 0.25,
          max.overlaps = Inf
        ) +
        labs(y = "Trophic Level", title = eco.name) +
        scale_y_continuous(breaks = seq(floor(y_min), ceiling(y_max), by = 1),
                           expand = expansion(c(0.10, 0.10))) +
        scale_x_continuous(expand = expansion(c(0.10, 0.10))) +
        theme_classic() +
        theme(
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank()
        )

      return(p)
}
