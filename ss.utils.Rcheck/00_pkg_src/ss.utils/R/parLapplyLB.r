##  This is taken from page 20 of Parallel R by McCallum and Weston

parLapplyLB <- function(cl, x, fun, ...) {
  clusterCall(cl, LB.init, fun, ...)
  r <- clusterApplyLB(cl, x, LB.worker)
  clusterEvalQ(cl, rm('.LB.fun', '.LB.args', pos=globalenv()))
  r
}

LB.init <- function(fun, ...) {
  assign('.LB.fun', fun, pos=globalenv())
  assign('.LB.args', list(...), pos=globalenv())
  NULL
}
LB.worker <- function(x) {
  do.call('.LB.fun', c(list(x), .LB.args))
}