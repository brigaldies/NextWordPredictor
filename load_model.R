loadModel <- function(directory, sample_rate) {
    modelFile = paste0(directory, '/model.rds')
    message(paste0('Loading model from "', modelFile, '" ...'))
    execTime = system.time({
        model = readRDS(file = modelFile)
    })
    message(paste('Model loaded in', round(execTime['elapsed'], 2), 'secs'))
    message(paste('Model size: ', round(object.size(model)/1024/1024, 2), 'MB'))
    
    # Return the model
    model    
}