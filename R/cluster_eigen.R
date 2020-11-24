#' @title This function implements a vector partition algorithm with global
#' initialization that maximizes the modilarity measure and provide membership
#' for community assignment. The idea is that the uncontrained solution of
#' community assignment is the eigenvectors of the modualrity matrix.
#' We project graph distance matrix to the eigenvectos in order to get a
#' constrained solution and furture tune current assignment with one-iteration
#' of k-means clustering or set it as an initialization of a full iteration
#' of k-means clustering.
#'
#' @param g The input unweigheted and undirected graph.
#' @param kopt The specified number of clusters.
#' @param tune Methods selected to tune community assignment by one-iteration
#' of k-means clustering with "fast" tune or full iteration of k-means
#' clustering with "fine" tune. Defaut is "fine" tune.
#' @param verbose output message
#' @return
#' Returns a list with entries:
#' \describe{
#'   \item{k:}{ The number of communities detected in current
#'            community assignment.}
#'   \item{modularity:}{ The calculated modularity for current
 #'        community assignment showed by "label" in cluster list.}
#'   \item{cluster:}{ A list of membership, normalized membership and label for
#'        current and updated community assignment after tunning.}
#'   \item{k_up:}{ The number of communities detected in updated
#'        community assignment.}
#'   \item{modularity_up:}{ The calculated modularity for updated community
#'         assignment showed by "label_up" in cluster list.}
#' }
#' @examples
#' \donttest{
#' library(igraph)
#' g <- make_full_graph(5) %du% make_full_graph(5) %du% make_full_graph(5)
#' g <- add_edges(g, c(1,6, 1,11, 6, 11))
#' res <- cluster_eigen(g)
#' plot(g, vertex.color = res$cluster[[1]]$label_up)
#' }
#' @import tidyverse
#' @import foreach
#' @import igraph
#' @import doParallel
#' @import parallel
#' @importFrom purrr map map_dbl
#' @importFrom tibble as_tibble add_column
#' @importFrom lsa cosine
#' @importFrom dplyr group_by group_map mutate arrange desc select
#' @importFrom bcp bcp
#' @importFrom DMwR SoftMax
#' @importFrom here here
#' @export

cluster_eigen <- function(g,
                          kopt = 2,
                          tune = c("fast", "fine"),
                          verbose = FALSE) {           # set your path
    # my_path      <- gsub("eigencluster/", "", here("dimension/R/"))
    # source_files <- list.files(my_path, "*.R$")
    # map(paste0(my_path, source_files), source)
    n        <- vcount(g)
    eclidean <- diag(1, n) - as.matrix(as_adjacency_matrix(g))
    mod      <- modularity_matrix(g, seq_len(length(V(g))))
    # dim_time <- system.time(
    #     results <- dimension(mod,
    #                          components = min(n, 50),
    #                          decomposition =  "eigen",
    #                          method = "kmeans"))
    # lambda   <- results$subspace$sigma_a
    # u        <- results$subspace$u
    w <- eigen(mod)
    lambda   <- w$values
    u        <- w$vectors
    
    # function to calcualte modualrity
    mod_cal <- function(mem) {
        df <- tibble(mem = mem, u = u) %>%
              group_by(mem) %>%
              group_map(~ tibble(w = sum(c(colSums(.)^2) * lambda)))
        Reduce(`+`, df)$w / (2 * gsize(g))
    }
    registerDoParallel(detectCores())
    mem_cal <- function(gd, kopt = 2) {
        foreach(s = switch(2 - is.null(kopt),
                           seq_len(ncol(gd)),
                           kopt)) %dopar% {
            # max cosine similarity
            mem <- gd[, seq_len(s)] %>% apply(1, max)
            label <-  gd[, seq_len(s)] %>%
                      apply(1, function(x) which(x %in% mem)[1])
            tibble(mem = mem, mem_norm = SoftMax(mem), label = label)
        }
    }

    if (missing(kopt)) {
        kopt <- NULL
        dim  <- min(results$dimension + 1, sum(lambda > 0))
    } else if (max(kopt) <= sum(lambda > 0)) {
        dim  <- max(kopt)
    } else {
        res  <- tibble(k = kopt, modularity_up = NA)
        res$cluster <- list(tibble(mem_up = NA,
                                   mem_norm_up = NA,
                                   label_up = NA))
        stop("argument k is larger than the maximum cluster number.\n")
    }
    if (missing(tune)) {
        tune <- "fine"
    }

    if (dim == 1) {
        label       <- rep(1, n)
        modularity  <- mod_cal(mem = label)
        mem_time    <- tune_time <- 0
        res         <- tibble(k = 1, modularity_up = modularity)
        res$cluster <- list(tibble(mem_up = 1,
                                   mem_norm_up = 1,
                                   label_up = label))
    } else if (dim == 2) {
      label <- (1 + sign(u[, 1])) / 2
      modularity <- mod_cal(mem = label)
      mem_time    <- tune_time <- 0
      res <- tibble(k = 2, modularity_up = modularity)
      res$cluster <- list(tibble(mem_up = u[, 1],
                                 mem_norm_up = SoftMax(u[, 1]),
                                 label_up = label))
    } else {
        r <- u[, seq_len(dim)] %*% diag(sqrt(lambda[seq_len(dim)]))
        cosine_time <- system.time(suppressMessages(
        gd <- as_tibble(
              foreach(i = seq_len(nrow(eclidean)), .combine = rbind) %dopar%
                apply(u[, seq_len(dim)], 2, function(x)
                  cosine(eclidean[i, ], x)
                ),
              .name_repair = "unique")))
        res <- tibble(k = switch(2 - is.null(kopt),
                                 seq_len(ncol(gd)),
                                 kopt),
                      modularity = 0)
        mem_time  <- system.time(mem <- mem_cal(gd, kopt = kopt))
        tune_time <- system.time(
            res$cluster <- switch(tune,
                fast = {
                    Map(function(x, y) x %>% add_column(y),
                        mem, mem %>% lapply(`[[`, 3) %>% lapply(fast_tune, r))
                    },
                fine = {
                    Map(function(x, y) x %>% add_column(y),
                        mem, mem %>% lapply(`[[`, 3) %>% lapply(fine_tune, r))
                    },
                stop("Invalid tuning input")
            )
        )
        # output result
        res <- res %>%
               mutate(modularity    = map_dbl(cluster,
                                              ~ mod_cal(.x$label)),
                      k_up          = map_dbl(cluster,
                                              ~ length(unique(.x$label_up))),
                      modularity_up = map_dbl(cluster,
                                              ~ mod_cal(.x$label_up))
                      ) %>%
               arrange(desc(modularity_up))
        # if (verbose) {
        #     message("dim_time", dim_time[[3]],
        #             "cosine_time", cosine_time[[3]],
        #             "mem_time", mem_time[[3]],
        #             "tune_time", tune_time[[3]], "\n")
        # }
    }
    return(res)
}
