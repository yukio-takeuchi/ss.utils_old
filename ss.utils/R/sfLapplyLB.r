## Lord balancing version of sfLapply to use parLapplyLB

sfLapplyLB<-function (x, fun, ...){
    sfCheck()
    checkFunction(fun)
    if (sfParallel())
        return(parLapplyLB(sfGetCluster(), x, fun, ...))
    else return(lapply(x, fun, ...))
}