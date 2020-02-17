
shift <- function(x, k = 1) {
  n <- length(x)
  if (k > 0) c(rep(x[[1]], k), x)[1:n]
  else if (k < 0) c(x, rep(x[[n]], -k))[(-k + 1):(-k + n)]
  else x
}

dif <- function(x) {
  as.numeric(shift(x, -1) - shift(x, 1)) / 2
}


subtract_mean <- function(x, na.rm = TRUE) {
  x - mean(x, na.rm = na.rm)
}

divide_by_sd <- function(x, na.rm = TRUE) {
  x / sd(x, na.rm = na.rm)
}

standardize <- function(x, na.rm = TRUE) {
  x %>% 
    subtract_mean(na.rm = na.rm) %>% 
    divide_by_sd(na.rm = na.rm)
}


soften <- function(x, k = 10, l = 0.1 ^ (2 / k), 
                   method = c("round", "forward")) {
  t <- rep(0, length(x))
  
  if (missing(method) || method == "round") {
    int <- -floor((k - 1) / 2):ceiling((k - 1) / 2)
  } else if (method == "forward") {
    int <- 0:(k - 1)
    l <- sqrt(l)
  }
  
  for (i in int) t <- t + shift(x, -i) * l ^ abs(i)
  t / sum(l ^ abs(int))
}

plus <- function(x) {
  x[x < 0] <- 0
  x
}


find_events <- function(x, k = 240, delta = 0.12) {
  x <- x %>% dif %>% `-` (delta) %>% plus %>% `*` (100) %>% 
    `+` (1) %>%  log %>% soften(k, method = "forward")
  
  w <- x %>% as.logical %>% rle %>% unclass %>% as_tibble
  w %<>% mutate(sums = cumsum(lengths) - lengths)
  w %<>% filter(values)
  w <- map2_dbl(w$sums, w$lengths, function(a, b) a + which.max(x[a:(a + b)]))
  
  r <- rep_along(x, FALSE)
  r[w] <- TRUE
  r
}


find_dist <- function(x, y) {
  d2 <- rowSums(x ^ 2) %*% matrix(rep(1, nrow(y)), nrow = 1) + 
    matrix(rep(1, nrow(x))) %*% rowSums(y ^ 2) - 
    2 * x %*% t(y)
  sqrt(d2)
}

classify_nearest <- function(x, y) {
  d <- find_dist(x, y)
  apply(d, 1, function(x) which(x == min(x))[[1]])
}
