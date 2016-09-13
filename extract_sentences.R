# Extract sentences
extractSentences <- function(singleLine) {    
    
    line <- singleLine
    
    # Convert to UTF-8
    #line <- stri_enc_toutf8(line)
    
    # Trim the line
    line <- str_trim(line)
    
    # Terminate the line
    lineLen = nchar(line)
    lineEndChar = substr(line, lineLen, lineLen)
    endOfSentenceChars = c('.', '?', ':', '!')
    if (!(lineEndChar %in% endOfSentenceChars)) {
        line = paste0(line, '.')
    }
    
    # Substitute numbers with place holder <NUMBER>
    line <- gsub('[[:digit:]]+([\\.:-]*[[:digit:]])*', '<NUMBER>', line)    
    
    # Extract the sentences:
    regexSentence = '[^.!?;:]*[.!?;:]'
    sentences = str_trim(unlist(regmatches(line, gregexpr(regexSentence, line))))
    sentences
}

# Test lines
s1 <- extractSentences("  this is a sentence.And another sentence with 1.25 grams of something   ")
S2 <- extractSentences("  The cow jumps over the moon; From a famous children's book!  ")