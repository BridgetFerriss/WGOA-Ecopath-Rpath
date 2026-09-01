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
                                 det_color = "darkgray",
                                 node_size_min = 1,
                                 node_size_max = 30,
                                 text_size = 3,
                                 highlighted_groups = NULL, # character vector of group names to bold
                                 highlighted_color = "gray15", # text color for highlighted labels
                                 dim_alpha = 0.15, # alpha for groups not interacting with highlighted species
                                 low_tl_spread = 1, # extra horizontal spread multiplier for low trophic-level nodes
                                 max.overlaps = Inf) # passed to ggrepel; reduce to declutter labels
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
  
  # Compute node size based on log1p(Biomass) — log-transform prevents detritus
  # from dominating the scale and collapsing all other species to the minimum.
  nodes <- nodes %>% mutate(node_size = scale_value(log1p(Biomass), 
                                                    new_min = node_size_min, 
                                                    new_max = node_size_max))
  
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
             width = nodes$node_size[nodes$id == i])  # use scaled node size for consistent sizing
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
  
  # Identify groups that directly interact with any highlighted species
  if (!is.null(highlighted_groups)) {
    highlighted_ids  <- nodes$id[nodes$Group %in% highlighted_groups]
    interacting_ids  <- unique(c(
      highlighted_ids,
      edge_list$to[edge_list$from %in% highlighted_ids],
      edge_list$from[edge_list$to %in% highlighted_ids]
    ))
    interacting_groups <- nodes$Group[nodes$id %in% interacting_ids]
  } else {
    highlighted_ids    <- nodes$id       # no dimming when nothing is highlighted
    interacting_ids    <- nodes$id
    interacting_groups <- nodes$Group
  }
  
  # Color only edges that directly touch a highlighted species;
  # everything else is drawn as a gray segment
  edge_list <- edge_list %>%
    mutate(
      edge_active     = from %in% highlighted_ids | to %in% highlighted_ids,
      edge_alpha_link = if_else(edge_active, 0.30, dim_alpha),
      edge_alpha_loop = if_else(edge_active, 0.85, dim_alpha)
    )
  
  # Global width range — shared by both scale_edge_width and scale_linewidth
  # so active (gradient) and inactive (gray) edges use the same size reference.
  width_range <- range(edge_list$width, na.rm = TRUE)
  
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
    mutate(cluster = if_else(type == 3, "fleet", as.character(cluster))) %>%
    mutate(cluster = if_else(type==2, "det", as.character(cluster)))
  
  
  # Create a layout using KK
  lay <- create_layout(graph_obj, layout = "kk")
  if (!"TL" %in% colnames(lay)) {
    lay <- left_join(lay, as_tibble(graph_obj, what = "nodes"), by = "id")
  }
  lay$x <- lay$x * h_spacing      # Spread nodes horizontally
  
  # Within-band spreading for low-TL nodes.
  # Nodes within each rounded-TL band are re-spaced by rank,
  # guaranteeing a minimum separation. The spread is largest at the lowest
  # TL and tapers to 1 (no extra spread) at the highest TL.
  # NOTE: lay$x is modified directly to preserve the layout_tbl_graph class;
  # piping lay through dplyr would strip that class and break ggraph.
  if (low_tl_spread > 1) {
    tl_min_val <- min(lay$TL, na.rm = TRUE)
    tl_max_val <- max(lay$TL, na.rm = TRUE)
    tl_bands   <- round(lay$TL)
    new_x      <- lay$x
    
    for (band in unique(tl_bands)) {
      idx <- which(tl_bands == band)
      nn  <- length(idx)
      if (nn > 1) {
        tl_norm     <- (band - tl_min_val) / (tl_max_val - tl_min_val)
        band_spread <- low_tl_spread * (1 - tl_norm) + 1 * tl_norm
        r           <- rank(lay$x[idx], ties.method = "first")
        x_c         <- mean(lay$x[idx])
        new_x[idx]  <- x_c + (r - (nn + 1) / 2) * band_spread
      }
    }
    lay$x <- new_x
  }
  
  lay$y <- as.numeric(lay$TL)  # Adjust vertical spacing
  lay$fontface     <- if_else(lay$Group %in% highlighted_groups, "bold", "plain")
  lay$highlighted  <- lay$Group %in% highlighted_groups
  lay$node_alpha   <- if_else(lay$Group %in% interacting_groups, 0.8, dim_alpha)
  
  y_min <- min(lay$y, na.rm = TRUE)
  y_max <- max(lay$y, na.rm = TRUE)
  
  # Store final node positions for re-use
  node_x <- lay$x
  node_y <- lay$y
  
  # Dimmed edge coordinates for geom_segment rendering
  dim_edge_coords <- edge_list %>%
    filter(!edge_active) %>%
    transmute(
      x     = node_x[from],
      y     = node_y[from],
      xend  = node_x[to],
      yend  = node_y[to],
      width = width
    )
  
  # Active-edge-only graph with the same node positions (manual layout)
  # This lets geom_edge_link draw only active edges with gradient + fixed alpha
  graph_active <- graph_obj %>% activate(edges) %>% filter(edge_active)
  lay_active   <- create_layout(graph_active, layout = "manual",
                                x = node_x, y = node_y)
  lay_active$fontface    <- lay$fontface
  lay_active$highlighted <- lay$highlighted
  lay_active$node_alpha  <- lay$node_alpha
  
  # Create a color mapping for node clusters
  # Get all unique cluster values
  node_levels <- sort(unique(activate(graph_obj, nodes) %>% pull(cluster)))
  # Separate fleets from non-fleet clusters
  nonfleet_levels <- setdiff(node_levels, c("fleet", "det"))
  # Assign colors to non-fleet clusters using the palette
  nonfleet_colors <- colors_net(length(nonfleet_levels))
  # Combine with a fixed color for fleets
  color_mapping <- c("fleet" = fleet_color, "det"= det_color, setNames(nonfleet_colors, nonfleet_levels))
  
  set_graph_style(plot_margin = margin(30, 30, 30, 30))
  jitter <- position_jitter(width = 0.1, height = 0.1)
  
  # Build the ggraph plot
  p <- ggraph(lay_active) +
    # Dimmed edges drawn first (behind active edges) as plain segments
    geom_segment(data = dim_edge_coords,
                 aes(x = x, y = y, xend = xend, yend = yend, linewidth = width),
                 color = line.col, alpha = 0.20) +
    scale_linewidth(range = c(0.2, 10), limits = width_range, guide = "none") +
    # Active edges with gradient colour and fixed alpha
    geom_edge_link(aes(edge_width = width, color = after_stat(index)),
                   lineend = "round", alpha = 0.30) +
    scale_edge_colour_gradient(low = "#ffd06f", high = "#aadce0") +
    geom_edge_loop(aes(edge_width = width, color = after_stat(index)),
                   alpha = 0.85, lineend = "round") +
    scale_edge_width(range = c(0.2, 10), limits = width_range) +
    geom_node_point(aes(size = node_size, alpha = node_alpha), color = "white") +
    geom_node_point(aes(
      alpha = node_alpha,
      color = cluster,
      size = node_size
    )) +
    scale_size(range = c(1, max(nodes$node_size, na.rm = TRUE))) +
    scale_color_manual(values = color_mapping) +
    scale_alpha_identity() +
    geom_node_text(
      aes(label = if_else(highlighted, NA_character_, as.character(Group)),
          fontface = fontface,
          alpha = node_alpha),
      size = text_size,
      color = "gray15",
      repel = TRUE,
      check_overlap = TRUE,
      point.padding = unit(0.95, "lines"),
      segment.size = 0.25,
      max.overlaps = max.overlaps,
      na.rm = TRUE
    ) +
    geom_node_text(
      aes(label = if_else(highlighted, as.character(Group), NA_character_),
          fontface = fontface,
          alpha = node_alpha),
      size = text_size,
      color = highlighted_color,
      repel = TRUE,
      check_overlap = TRUE,
      point.padding = unit(0.95, "lines"),
      segment.size = 0.25,
      max.overlaps = max.overlaps,
      na.rm = TRUE
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
