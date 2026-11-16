# crossunder.R | Crossunder detector | BDA400 A5 | Iqra
# "True" when arr1 drops from at-or-above arr2 to strictly below it, "False"
# otherwise, "None" for the first element; string labels per the pseudocode.

crossunder <- function(arr1, arr2) {
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  crossunder_signals <- rep("False", length(arr1))
  crossunder_signals[1] <- "None"
  for (i in seq_along(arr1)[-1]) {
    if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
      crossunder_signals[i] <- "True"
    }
  }
  crossunder_signals
}

# Demo:
# fast <- c(11, 13, 16, 21, 19, 23, 26, 25, 22)
# slow <- c(19, 21, 23, 19, 16, 13, 11, 12, 14)
# crossunder(fast, slow)
