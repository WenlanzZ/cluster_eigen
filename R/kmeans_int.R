#' @title Return an upated communtiy assignments and
#' memberships by k-means clustering with initialization.
#'
#' @param label Current labels of community assignments.
#' @param r A p-dimensional vector of eigenvector times
#' square root of non-negative eigenvalues, where p equals
#' the number of non-negative eigenvalues.
#' @return
#' Returns a data frame with entries:
#' \describe{
#'   \item{label_up:}{ Updated labels of community assignments.}
#' }
#' @examples
#' \donttest{
#' kmeans_int(label, r)
#' }
#' @importFrom  tibble tibble
#' @importFrom  stats aggregate
#' @export

kmeans_int <- function(label, r) {
        # initialize k means
        start <- aggregate(. ~ label,
                            data = tibble(label = label, r),
                            FUN = mean) %>%
                  select(-1)  
        tibble(label_up = kmeans(r, start)$cluster)
    }
