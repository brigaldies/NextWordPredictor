# Load the model
loadModel0 <- function(directory, sample_rate) {
    model = list()
    
    execTime = system.time({
        # Load the unigrams
        # message(paste('Loading the unigrams...'))
        # model$unigrams = readRDS(file = paste0(directory, '\\unigrams.rds'))
        # message(paste('Loaded', format(dim(model$unigrams)[1], big.mark = ',', scientific = FALSE), 'unigrams.'))
        
        # Load the bigrams
        message(paste('Loading the bigrams...'))
        model$bigrams = readRDS(file = paste0(directory, '/bigrams.rds'))
        message(paste('Loaded', format(dim(model$bigrams)[1], big.mark = ',', scientific = FALSE), 'bigrams.'))
        setkey(model$bigrams, lowergram)
        
        # Load the trigrams
        message(paste('Loading the trigrams...'))
        model$trigrams = readRDS(file = paste0(directory, '/trigrams.rds'))
        message(paste('Loaded', format(dim(model$trigrams)[1], big.mark = ',', scientific = FALSE), 'trigrams.'))
        setkey(model$trigrams, lowergram)
        
        # Load the quadgrams
        message(paste('Loading the quadgrams...'))
        model$quadgrams = readRDS(file = paste0(directory, '/quadgrams.rds'))
        message(paste('Loaded', format(dim(model$quadgrams)[1], big.mark = ',', scientific = FALSE), 'quadgrams.'))
        setkey(model$quadgrams, lowergram)
    })
    message(paste('Model loaded in', round(execTime['elapsed'], 2), 'secs'))
    message(paste('Model size: ', round(object.size(model)/1024/1024, 2), 'MB'))
    
    # Return the model
    model    
}

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