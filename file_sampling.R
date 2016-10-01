# -----------------------------------------------------------------------------
# Function: sampleCorpusFile
#
# Description: Create a sample data file with randomly selected lines. The
# randomization is executed with the rbinom function.
#
# Arguments:
# directory  : Directory where the file to sample resides.
# file_name  : Name of the file to sample.
# sample_rate: Sampling rate as a decimal number (e.g., 0.1 for 10%)
#
# Result: The sampled lines are written to a file named '<file_name>.sample<sample_rate>'
# in the same directory as the file to sample.
#
# Example usage:
# sampleCorpusFile('c:\coursera\datascience\capstone\corpus, 'en_US.twitter.txt', 0.1)
# -----------------------------------------------------------------------------
require(stringi)
sampleCorpusFile <- function(directory, file_name, sample_rate) {
    execTime <- system.time({
        set.seed(10)    
        filePath = paste0(directory, '\\', file_name)
        message(paste('Sampling', filePath, 'with rate', sample_rate))        
        cnx = file(filePath, open = "r")    
        samplePath = paste0(filePath, '.sample_', sprintf('%.1f', sample_rate * 100))
        message(paste('Creating sample', samplePath))
        sampleFile = file(samplePath, open = "wt", encoding = "UTF-8")        
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
            
            # Convert non-ASCII characters
            sampleLines <- stringi::stri_trans_general(sampleLines, "latin-ascii")
            
            # Write the sampled lines to the sample file
            if (sampleCount > 0) {
                writeLines(sampleLines, sampleFile)    
            }            
        }
    })
    message(paste(sampleSize, 'lines sampled out of', totalLinesCount, 'in', round(execTime["elapsed"], 2), "secs"))
    close(sampleFile)
    close(cnx)
    TRUE
}

# -----------------------------------------------------------------------------
# Function: partitionCorpusFile
#
# Description: Same random selection of sampled lines, but with the added step
# of writing out the non-sampled files to another file, hence creating a 
# partition of the input file.
#
# Arguments:
# directory  : Directory where the file to sample resides.
# file_name  : Name of the file to sample.
# sample_rate: Sampling rate as a decimal number (e.g., 0.1 for 10%)
# 
# Result: The sampled and non-sampled lines are written to files named '<file_name>.sample<sample_rate>'
# and '<file_name>.sample<1 - sample_rate>' respectively and in the same directory as the input file to sample.
#
# Example usage:
# partitionCorpusFile('c:\coursera\datascience\capstone\corpus, 'en_US.twitter.txt', 0.1)
# -----------------------------------------------------------------------------
partitionCorpusFile <- function(directory, file_name, sample_rate) {
    execTime <- system.time({
        set.seed(10)    
        filePath = paste0(directory, '\\', file_name)
        cnx = file(filePath, open = "rb") # Read in binary mode
        samplePath = paste0(filePath, '.partition_', sprintf('%.1f', sample_rate * 100))
        nonSamplePath = paste0(filePath, '.partition_', sprintf('%.1f', (1 - sample_rate) * 100))        
        message(paste('Creating samples', samplePath, 'and', nonSamplePath))
        sampleFile = file(samplePath, open = "wt")
        nonSampleFile = file(nonSamplePath, open = "wt")
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
            # Convert non-ASCII characters
            sampleLines <- stringi::stri_trans_general(sampleLines, "latin-ascii")
            nonSampleLines = unlist(lapply(seq_along(keepLines), function(i) { if (keepLines[i] == 0) lines[[i]] }))
            # Convert non-ASCII characters
            nonSampleLines <- stringi::stri_trans_general(nonSampleLines, "latin-ascii")
            sampleCount = length(sampleLines)
            nonSampleCount = length(nonSampleLines)
            message(paste(sampleCount, 'sampled lines, and', nonSampleCount, 'non-sampled lines'))
            sampleSize = sampleSize + sampleCount
            writeLines(sampleLines, sampleFile)    
            writeLines(nonSampleLines, nonSampleFile)    
        }
    })
    message(paste(sampleSize, 'lines sampled out of', totalLinesCount, 'in', round(execTime["elapsed"], 2), "secs"))
    close(sampleFile)
    close(nonSampleFile)
    close(cnx)
    TRUE
}