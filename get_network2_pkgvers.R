#'get_network2
#'
#'An adjusted version of the get_network function from R package asnipe.
#'
#'@param association_data data used to generate adjacency matrix (normally a GBI)
#'@param data_format type of data. Defaults to GBI
#'@param association_index Defaults to SRI
#'
#'@details for further parameter information see asnipe documentation
#'
#'@return the adjacency matrix
#'
#'@export

##Adapted function
get_network2<-function (association_data, data_format = "GBI", association_index = "SRI",
                        identities = NULL, which_identities = NULL, times = NULL,
                        occurrences = NULL, locations = NULL, which_locations = NULL,
                        start_time = NULL, end_time = NULL, classes = NULL, which_classes = NULL,
                        enter_time = NULL, exit_time = NULL)
{
  if (is.null(association_data)) {
    stop("No association_data data!")
  }
  if (length(dim(association_data)) != 2 & data_format == "GBI") {
    stop("Invalid dimensions for association_data")
  }
  if (length(dim(association_data)) != 3 & data_format == "SP") {
    stop("Invalid dimensions for association_data")
  }
  if ((length(identities) != ncol(association_data) & !is.null(identities)) ==
      TRUE) {
    stop("Length of identities does not match number of individuals")
  }
  if ((length(times) != nrow(association_data) & !is.null(times)) ==
      TRUE) {
    stop("Length of times does not match number of groups")
  }
  if ((length(occurrences[1, ]) != nrow(association_data) &
       !is.null(occurrences)) == TRUE) {
    stop("Number of occurrence periods does not match number of sampling periods")
  }
  if ((length(occurrences[, 1]) != ncol(association_data) &
       !is.null(occurrences)) == TRUE) {
    stop("Number of individuals in occurrences does not match number of individuals in sampling periods")
  }
  if ((length(locations) != nrow(association_data) & !is.null(locations)) ==
      TRUE) {
    stop("Length of locations does not match number of groups")
  }
  if ((length(classes) != ncol(association_data) & !is.null(classes)) ==
      TRUE) {
    stop("Length of classes does not match number of individuals")
  }
  if ((!is.null(which_identities) & is.null(identities)) ==
      TRUE) {
    stop("Cannot apply which_identities without identities data")
  }
  if ((!is.null(which_locations) & is.null(locations)) == TRUE) {
    stop("Cannot apply which_locations without locations data")
  }
  if ((!is.null(start_time) & is.null(times)) == TRUE) {
    stop("Cannot apply start_time without times data")
  }
  if ((!is.null(end_time) & is.null(times)) == TRUE) {
    stop("Cannot apply end_time without times data")
  }
  if ((!is.null(which_classes) & is.null(classes)) == TRUE) {
    stop("Cannot apply which_class without classes data")
  }
  if ((!is.null(enter_time) & is.null(times)) == TRUE) {
    stop("Cannot control for overlapping time without observation times")
  }
  if ((!is.null(exit_time) & is.null(times)) == TRUE) {
    stop("Cannot control for overlapping time without observation times")
  }
  if (!is.null(colnames(association_data)) & !all(colnames(association_data) ==
                                                  identities)) {
    stop("Identities is not in the same order as columns in association_data")
  }
  if (!any(association_index %in% c("SRI", "HWI"))) {
    stop("Unknown association_index")
  }
  if (data_format == "GBI") {
    association_data <- as.matrix(association_data)
  }
  else {
    association_data <- as.array(association_data)
  }
  if (!is.null(which_identities)) {
    if (data_format == "GBI")
      association_data <- association_data[, which(identities %in%
                                                     which_identities)]
    if (data_format == "SP")
      association_data <- association_data[, which(identities %in%
                                                     which_identities), which(identities %in% which_identities)]
    identities <- identities[which(identities %in% which_identities)]
  }
  if (!is.null(start_time) & is.null(end_time)) {
    end_time <- max(times)
  }
  if (!is.null(end_time) & is.null(start_time)) {
    start_time <- min(times)
  }
  if (!is.null(start_time) & !is.null(end_time)) {
    subs <- which(times >= start_time & times <= end_time)
    if (data_format == "GBI")
      association_data <- association_data[subs, ]
    if (data_format == "SP")
      association_data <- association_data[subs, , ]
    locations <- locations[subs]
    times <- times[subs]
  }
  if (!is.null(which_locations)) {
    subs <- which(locations %in% which_locations)
    if (data_format == "GBI")
      association_data <- association_data[subs, ]
    if (data_format == "SP")
      association_data <- association_data[subs, , ]
    locations <- locations[subs]
    times <- times[subs]
  }
  if (!is.null(which_classes)) {
    if (data_format == "GBI")
      association_data <- association_data[, which(classes %in%
                                                     which_classes)]
    if (data_format == "SP")
      association_data <- association_data[, which(classes %in%
                                                     which_classes), which(classes %in% which_classes)]
    identities <- identities[which(classes %in% which_classes)]
  }
  do.SR <- function(GroupBy, input, association_index, present) {
    jumps <- c(seq(0, ncol(input), 50))
    if (max(jumps) < ncol(input)) {
      jumps <- c(jumps, ncol(input))
    }
    out <- matrix(nrow = 0, ncol = 1)
    for (i in 1:(length(jumps) - 1)) {
      tmp <- input[, GroupBy] + input[, (jumps[i] + 1):jumps[i +
                                                               1]]
      if (length(tmp) > nrow(input)) {
        x <- colSums(tmp == 2)
      }
      else {
        x <- sum(tmp == 2)
      }
      if (length(tmp) > nrow(input)) {
        yab <- colSums(tmp == 1)
      }
      else {
        yab <- sum(tmp == 1)
      }
      if (association_index == "SRI") {
        out <- c(out, x/(x + yab))
      }
      else if (association_index == "HWI") {
        out <- c(out, x/(x + 0.5 * yab))
      }
    }
    out
  }
  do.SR.time <- function(GroupBy, input, association_index,
                         times, present) {
    jumps <- c(seq(0, ncol(input), 50))
    if (max(jumps) < ncol(input)) {
      jumps <- c(jumps, ncol(input))
    }
    out <- matrix(nrow = 0, ncol = 1)
    for (i in 1:(length(jumps) - 1)) {
      tmp <- input[, GroupBy] + input[, (jumps[i] + 1):jumps[i +
                                                               1], drop = FALSE]
      if (!is.null(enter_time) | !is.null(exit_time)) {
        tmp2 <- present[, GroupBy] + present[, (jumps[i] +
                                                  1):jumps[i + 1], drop = FALSE]
        tmp[which(tmp2 < 2, arr.ind = T)] <- 0
      }
      if (length(tmp) > nrow(input)) {
        x <- colSums(tmp == 2)
        yab <- apply(tmp, 2, function(x) {
          sum(table(times[x == 1]) == 2)
        })
        y <- colSums(tmp == 1) - (2 * yab)
      }
      else {
        x <- sum(tmp == 2)
        yab <- sum(table(times[tmp == 1]) == 2)
        y <- sum(tmp == 1) - (2 * yab)
      }
      if (association_index == "SRI") {
        out <- c(out, x/(x + y + yab))
      }
      else if (association_index == "HWI") {
        out <- c(out, x/(x + y + 0.5 * yab))
      }
    }
    out
  }
  do.SR2 <- function(i, a, association_index) {
    x <- apply(a[, i, ], 2, sum)
    n <- apply(a, 1, rowSums)
    n[n > 0] <- 1
    seen <- t(apply(n, 1, function(x) x - n[i, ]))
    ya <- rowSums(seen < 0)
    yb <- rowSums(seen > 0)
    seen <- t(apply(n, 1, function(x) x + n[i, ]))
    yab <- rowSums(seen > 1) - x
    if (association_index == "SRI") {
      out <- x/(x + ya + yb + yab)
    }
    else if (association_index == "HWI") {
      out <- x/(x + ya + yb + 0.5 * yab)
    }
    return(out)
  }
  do.SR2.occurrences <- function(i, a, association_index, occurrences) {
    x <- apply(a[, i, ], 2, sum)
    seen <- sweep(occurrences, 2, occurrences[i, ], "+")
    yab <- rowSums(seen == 2) - x
    ya_b <- rowSums(seen == 1)
    if (association_index == "SRI") {
      out <- x/(x + ya_b + yab)
    }
    else if (association_index == "HWI") {
      out <- x/(x + ya_b + 0.5 * yab)
    }
    return(out)
  }
  if (!is.null(enter_time) | !is.null(exit_time)) {
    present <- matrix(1, nrow(association_data), ncol(association_data))
  }
  else {
    present <- NA
  }
  if (!is.null(enter_time)) {
    for (i in 1:ncol(present)) {
      present[which(times < enter_time[i]), i] <- 0
    }
  }
  if (!is.null(exit_time)) {
    for (i in 1:ncol(present)) {
      present[which(times > exit_time[i]), i] <- 0
    }
  }
  if (data_format == "GBI" & is.null(times))
    fradj_sorted <- do.call("rbind", lapply(seq(1,
                                                ncol(association_data), 1), FUN = do.SR, input = association_data,
                                            association_index))
  if (data_format == "GBI" & !is.null(times))
    fradj_sorted <- do.call("rbind", lapply(seq(1,
                                                ncol(association_data), 1), FUN = do.SR.time, input = association_data,
                                            association_index, times, present))
  if (data_format == "SP" & is.null(occurrences))
    fradj_sorted <- do.call("rbind", lapply(seq(1,
                                                ncol(association_data), 1), FUN = do.SR2, a = association_data,
                                            association_index))
  if (data_format == "SP" & !is.null(occurrences))
    fradj_sorted <- do.call("rbind", lapply(seq(1,
                                                ncol(association_data), 1), FUN = do.SR2.occurrences,
                                            a = association_data, association_index, occurrences))
  fradj_sorted[is.nan(fradj_sorted)] <- 0
  diag(fradj_sorted) <- 0
  if (!is.null(identities)) {
    colnames(fradj_sorted) <- identities
    rownames(fradj_sorted) <- identities
  }
  else if (!is.null(colnames(association_data))) {
    colnames(fradj_sorted) <- colnames(association_data)
    rownames(fradj_sorted) <- colnames(association_data)
  }
  return(fradj_sorted)
}
