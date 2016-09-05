# Create a data sample
sampleCorpusFile <- function(directory, file_name, sample_rate) {
    execTime <- system.time({
        set.seed(10)    
        filePath = paste0(directory, '\\', file_name)
        cnx = file(filePath, open = "r")    
        samplePath = paste0(filePath, '.sample_', sprintf('%.1f', sample_rate * 100))
        message(paste('Creating sample', samplePath))
        sampleFile = file(samplePath, open = "wt")
        #lines = readLines(cnx, n = 10)    
        #writeLines(lines, sampleFile)
        linesChunkSize = 100000
        totalLinesCount = 0
        sampleSize = 0
        while (TRUE) {
            lines = readLines(cnx, n = linesChunkSize, encoding = 'UTF-8')  
            linesCount = length(lines)
            totalLinesCount = totalLinesCount + linesCount
            if (linesCount == 0 ) {
                message("Reached EOF")
                break
            }    
            message(paste(linesCount, 'lines read'))
            keepLines <- rbinom(n = linesCount, size = 1, prob = sample_rate)
            sampleLines = unlist(lapply(seq_along(keepLines), function(i) { if (keepLines[i] == 1) lines[[i]] }))
            sampleCount = length(sampleLines)
            message(paste(sampleCount, 'sampled lines'))
            sampleSize = sampleSize + sampleCount
            writeLines(sampleLines, sampleFile)    
        }
    })
    message(paste(sampleSize, 'lines sampled out of', totalLinesCount, 'in', round(execTime["elapsed"], 2), "secs"))
    close(sampleFile)
    close(cnx)
    TRUE
}
