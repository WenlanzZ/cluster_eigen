#' @title Return an upated communtiy assignments and
#' memberships by one iteration of k-means clustering.
#'
#' @param label Current labels of community assignments.
#' @param r A p-dimensional vector of eigenvector times
#' square root of non-negative eigenvalues, where p equals
#' the number of non-negative eigenvalues.
#' @return
#' Returns a data frame with entries:
#' \describe{
#'   \item{mem_up:}{ An updated measure of the strenth with
#'   which vertices assigned to their new communities. }
#'   \item{mem_norm_up:}{ a softMax normalized "mem_up".}
#'   \item{label_up:}{ Updated labels of community assignments.}
#' }
#' @examples
#' \donttest{
#' fast_tune(label, r)
#' }
#' @importFrom  tibble tibble
#' @importFrom  stats aggregate
#' @importFrom  DMwR SoftMax
#' @export

fast_tune <- function(label, r) {
        # one iteration k means
        center <- aggregate(. ~ label,
                            data = tibble(label = label, r),
                            FUN = sum) %>%
                  select(-1)
        group <- as_tibble(foreach(s = seq_len(nrow(center)),
                                   .combine = cbind) %do%
                apply(r, 1, function(x) x %*% t(center[s, ])))
        # max center angle
        mem_up <- group %>% apply(1, max)
        label_up <- group %>% apply(1, function(x) which(x %in% mem_up)[1])
        tibble(mem_up = mem_up,
               mem_norm_up = SoftMax(mem_up),
               label_up = label_up)
    }
