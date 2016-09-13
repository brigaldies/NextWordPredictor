require(doParallel)

# Generic function for parallelizing any task (when possible)
parallelizeTask <- function(task, ...) {
    # Calculate the number of cores
    ncores <- detectCores() - 1
    # Initiate cluster
    cl <- makeCluster(ncores)
    registerDoParallel(cl)
    #print("Starting task")
    r <- task(...)
    #print("Task done")
    stopCluster(cl)
    r
}