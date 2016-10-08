## Set of N-Grams

Our predictive model is composed of the following five N-grams R `data.table` tables:

1. A **unigrams** (N=1) data table (e.g., "the", "to");
2. A **bigrams** (N=2) data table (e.g., "i am", "to be");
3. A **trigrams** (N=3) data table (e.g., "one of the", "a lot of"); 
4. A **quadgrams** (N=4) data table (e.g., "the end of the", "thanks for the");
5. A **pentagrams** (N=5) data table (e.g., "at the end of the", "in the middle of the").

## N-Gram Data Structure

Each N-Gram data table contains the following columns:

Column | Description
------------- | -------------
**gram** | The gram as a string of one (N=1) or more (N>=2) words.
**count** | The number of times the gram is used in the Corpus training data set.
**lowergram** | The preceding lower-order gram (e.g., for gram "in the world", the lowergram is "in the"). This is applicable to N>=2.
**last_word_in_gram** | The last word in the gram (e.g., for gram "in the world", the last word is "world").
**logprob** | Logarithm of the gram's **Maximum Likelihood Estimation** (MLE). see definition below.
**logpercent** | Logarithm of the gram's **Percent Use** (PU) based on the gram's count. see definition below.

### Percent Use

Both the MLE and Percent Use probabilities are calculated according to the definitions specified in reference [N-Grams](https://web.stanford.edu/~jurafsky/slp3/4.pdf).

For a given N in an N-gram, a gram's **Percent Use** is calculated as the gram's usage count in the training corpus divided by the total number of grams in the N-gram, as shown by the formula below:

$$P_{PU}(w_1 \\ldots w_n) = \\frac{Count(w_1 \\ldots w_n)}{Count(N-Grams)}$$

### Maximum Likelihood Estimation

For a given N in an N-gram, a gram's **Maximum Likelihood Estimation** is calculated as the gram's usage count in the training corpus divided by the preceding lower-order gram's usage count in the training corpus, as shown by the formula below:

$$P_{MLE}(w_1 \\ldots w_n) = P(w_n|w_1 \\ldots w_{n-1}) = \\frac{Count(w_1 \\ldots w_n)}{Count(w_1 \\ldots w_{n-1})}$$

## N-Gram Construction

Using the training English Corpus built with `TM::corpus` from the 10%-sampled news, blogs, and twitter files, each n-gram is constructed as follows:

1. An N-order tokenizer is built with `RWeka::NGramTokenizer`;
1. A Document Term Matrix (`tdm`) is built with `TM::DocumentTermMatrix` with the RWeka tokenizer;
1. The `dtm` data structure is converted to an R matrix: `dtmAsMatrix <- as.matrix(dtm)`
1. The following vectors are created from the DTM matrix:
    + The n-grams themselves:
        + `ngramsString <- names(dtmAsMatrix)`
    + The n-grams' numbers of appearances, or counts, in the Corpus (Note: The n-grams with a count of one are dropped):
        + `ngramsCounts <- colSum(dtmAsMatrix)`
    + The n-grams' lower-order (n-1)-grams:
        + `ngramsLowerOrderString <- sapply(ngramsString, function(gram) { getFirstNTokens(gram, n-1)})`
    + The n-grams' lower-order (n-1)-grams' counts, as captured in the (n-1)-grams data table:
        + `ngramsLowerOrderCount <- base::merge(x = data.frame(gram = ngramsLowerOrderString), y = data.frame(gram = lowerOrderNGram$gram, count = lowerOrderNGram$count), all.x = TRUE)$count`
    + The n-grams' last word in the gram:
        + `ngramsLastWord <- sapply(ngramsString, function(gram) { getLastToken(gram)})`
    + The n-grams' logarithm of the Maximum Likelihood Estimate (MLE):
        + `ngramsLogProb = log(ngramsCount/ngramsLowerOrderCount)`
    + The n-grams' logarithm of the percent use (PU):
        + `ngramsLogPercent = log(ngramsCount/ngramsTotalCount)`
1. A `data.table` table is built with a column for each vector built above:
    + `ngramsDat = data.table(gram = ngramsString, count = ngramsCount, lowergram = ngramsLowerOrderString, lowercount =  ngramsLowerOrderCount, last_word_in_gram = ngramsLastWord, logprob = ngramsLogProb, logpercent = ngramsLogPercent, key = 'gram')`
