The English Corpus was produced by executing the following **reproducible** steps:

1. Randomly (seed-based for reproducibility) select 10% (our "sampling rate") of the lines in the raw English files (news, blogs, twitter);
2. Persist the selected lines into separate "training" files, for later viewing if necessary;
3. Load of the training files into an English Corpus using the R `TM` package function `Corpus`;
4. Clean/pre-process the loaded lines with the `TM` package function `tm_map` with the *transformers* listed below:
    + Convert to lower-case (`tm_map(corpus, content_transformer(tolower))`)
    + Remove punctuation (`tm_map(corpus, removePunctuation)`)
    + Remove numbers (`tm_map(corpus, removeNumbers)`)
    + Remove profanity (`tm_map(corpus, removeWords, enProfanityWords[,1])`). We used [this](https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en) list of profane words.
    + Remove white spaces (`tm_map(corpus, stripWhitespace)`)
