#' @title Convert a tree into branching times
#' @description Convert a tree into branching times. Differently from the ape's
#'  function, it will keep the multiple events. Since the units are million
#'  years, a precision of 8 means that the approximation goes up to the 8-th
#'  digits. With such approximation we consider events happening within an
#'  interval of 4 days (1 million years / 10^8 = 1 year / 100) as simultaneous.
#' @inheritParams default_params_doc
#' @return the branching times
#' @author Giovanni Laudanno
convert_tree2brts <- function(tree, precision = 8) {

  brts0 <- ape::branching.times(tree)
  brts <- DDD::roundn(brts0, digits = precision)

  brts
}

#' @title Site models
#' @description Site models
#' @inheritParams default_params_doc
#' @return the site models
#' @author Giovanni Laudanno
get_site_models <- function() {
  beautier::get_site_model_names()
}

#' @title Clock models
#' @description Clock models
#' @inheritParams default_params_doc
#' @return the clock models
#' @author Giovanni Laudanno
get_clock_models <- function() {
  beautier::get_clock_model_names()
}

#' @title Twin models
#' @description Twin models
#' @inheritParams default_params_doc
#' @return the twin models
#' @author Giovanni Laudanno
get_twin_models <- function() {
  c("bd", "yule")
}

#' @title Convert bd phylo to L table
#' @description Convert bd phylo to L table. Don't use for mbd.
#' @inheritParams default_params_doc
#' @return the L table
#' @author Xu Liang, Giovanni Laudanno
#' @export
bd_phylo_2_l_table <- function(
  phylo
) {
  # get L names
  l_names <- c("birth_time", "parent", "id", "death_time")

  # compute the relative branching times
  brt <- convert_tree2brts(phylo) # nolint pirouette function

  if (min(brt) < 0) {
    brt <- brt + abs(min(brt))
  }
  # number of species including extinct species.
  num_species <- phylo$Nnode + 1
  brt_pre_l <- c(brt[phylo$edge[, 1] - length(phylo$tip.label)]) # nolint
  # check if the relative branching times are equal to the real branching times.
  # if not correct it to the real branching times.
  if (min(brt_pre_l) == 0) {
    correction <- max(phylo$edge.length[which(brt_pre_l == 0)]) # nolint
    brt_pre_l <- brt_pre_l + correction
  }
  # preliminary l_table table
  pre_l_table <- cbind(
    brt_pre_l,
    phylo$edge,
    phylo$edge.length, # nolint
    brt_pre_l - phylo$edge.length # nolint
  )
  # identify the extant species and the extinct species
  extantspecies_index <- pre_l_table[which(pre_l_table[, 5] <= 1e-10), 3]
  tipsindex <- c(1:num_species)
  extinct_index3 <- subset(tipsindex, !(tipsindex %in% extantspecies_index))
  # assigen the extinct species with extinct times;
  # the extant species with -1 and the internal nodes with 0.
  eeindicator <- matrix(0, length(phylo$edge.length), 1) # nolint
  eeindicator[match(extantspecies_index, pre_l_table[, 3])] <- -1
  ext_pos <- match(extinct_index3, pre_l_table[, 3])
  eeindicator[ext_pos] <- pre_l_table[ext_pos, 5]
  pre_l_table <- cbind(pre_l_table, eeindicator)

  sort_l_table <- pre_l_table[order(pre_l_table[, 1], decreasing = TRUE), ]
  nodesindex <- unique(phylo$edge[, 1])
  l_table <- sort_l_table
  real_l <- NULL
  do <- 0
  while (do == 0) {
    j <- which.min(l_table[, 3])
    daughter <- l_table[j, 3]
    parent <- l_table[j, 2]
    if (parent %in% nodesindex) {
      l_table[which(l_table[, 2] == parent), 2] <- daughter
      if (length(which(l_table[, 3] == parent)) == 0) {
        real_l <- rbind(real_l, l_table[j, ], row.names = NULL)
        l_table <- l_table[-j,
               , drop = FALSE]
      } else {
        l_table[which(l_table[, 3] == parent), 6] <- l_table[j, 6]
        l_table[which(l_table[, 3] == parent), 3] <- daughter
        l_table <- l_table[-j,
               , drop = FALSE]
      }
    } else {
      real_l <- rbind(real_l, l_table[j, ], row.names = NULL)
      l_table <- l_table[-j,
             , drop = FALSE]
    }

    if (nrow(l_table) == 0) {
      do <- 1
    }
  }
  real_l <- real_l[order(real_l[, 1], decreasing = T), ]
  l_table <- real_l[, c(1, 2, 3, 6)]

  daughter_index <- l_table[, 3]
  daughter_realindex <- c(1:nrow(l_table))
  parent_index <- l_table[, 2]
  parent_realindex <- match(parent_index, daughter_index)

  l_table[, 2] <- parent_realindex
  l_table[, 3] <- daughter_realindex
  l_table[1, 2] <- 0
  l_table[1, 3] <- -1
  l_table[2, 2] <- -1
  for (i in c(2:nrow(l_table))) {
    if (l_table[i - 1, 3] < 0) {
      mrows <- which(l_table[, 2] == abs(l_table[i - 1, 3]))
      l_table[mrows, 2] <- l_table[i - 1, 3]
      l_table[mrows, 3] <- -1 * l_table[mrows, 3]
    }
  }
  dimnames(l_table) <- NULL
  colnames(l_table) <- l_names
  return(l_table)
}
