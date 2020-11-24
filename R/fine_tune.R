#' @title Return an upated communtiy assignments and
#' memberships by k-means clustering.
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
#' fine_tune(label, r)
#' }
#' @importFrom  tibble tibble
#' @importFrom  stats aggregate
#' @importFrom  DMwR SoftMax
#' @export

fine_tune <- function(label, r) {
        prev_center     <- data.frame(0)
        update_center   <- data.frame(1)
        it <- 1
        while (all(max(apply(update_center - prev_center,
                             1,
                             function(x) sqrt(sum(x^2)))) > 0.0001)) {
            if (it == 1) {
                center <- aggregate(. ~ label,
                                    data = tibble(label = label, r),
                                    FUN = sum) %>%
                          select(-1)
            }
            group <- as_tibble(foreach(s = seq_len(nrow(center)),
                                       .combine = cbind) %do%
                    apply(r, 1, function(x) x %*% t(center[s, ])))
            # max center angle
            mem_up   <- group %>% apply(1, max)
            label_up <- group %>% apply(1, function(x) which(x %in% mem_up)[1])
            # update center
            prev_center   <- center
            center        <- aggregate(. ~ label,
                                       data = tibble(label = label_up, r),
                                       FUN = sum) %>%
                             select(-1)
            update_center <- data.frame(matrix(0,
                                               nrow = ncol(group),
                                               ncol = ncol(r)))
            update_center[seq_len(nrow(center)), ] <- center
            it <- it + 1
        }
        tibble(mem_up = mem_up,
               mem_norm_up = SoftMax(mem_up),
               label_up = label_up)
}
