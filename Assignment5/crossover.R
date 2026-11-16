# crossover.R | Crossover detector | BDA400 A5 | Iqra
# "Up" when arr1 breaks above arr2, "Down" when it breaks below, "None"
# otherwise. Element 1 is "None" because it has no previous point.

crossover <- function(arr1, arr2) {
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  crossover_signals <- rep("None", length(arr1))
  for (i in seq_along(arr1)[-1]) {
    up <- arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]
    down <- arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]
    if (up) {
      crossover_signals[i] <- "Up"
    } else if (down) {
      crossover_signals[i] <- "Down"
    }
  }
  crossover_signals
}

# Demo:
# fast <- c(11, 13, 16, 21, 19, 23, 26, 25, 22)
# slow <- c(19, 21, 23, 19, 16, 13, 11, 12, 14)
# crossover(fast, slow)
