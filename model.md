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
**lowergram** | The preceding lowe-order gram (e.g., for gram "in the world", the lowergram is "in the"). This is applicable to N>=2.
**last_word_in_gram** | The last word in the gram (e.g., for gram "in the world", the last word is "world").
**logprob** | Logarithm of the gram's **Maximum Likelihood Estimation** (MLE). see definition below.
**logpercent** | Logarithm of the gram's **Percent Use** (PU) based on the gram's count. see definition below.

### Percent Use

Both the MLE and Percent Use probabilities are calculated according to the definitions specified in reference [N-Grams](https://web.stanford.edu/~jurafsky/slp3/4.pdf).

For a given N in an N-gram, a gram's **Percent Use** is calculated as the gram's usage count in the training corpus divided by the total number of grams in the N-gram, as shown by the formula below:

$$P_{PU}(w_n \\ldots w_1) = \\frac{Count(w_n \\ldots w_1)}{Total Number of N-Grams}$$

### Maximum Likelihood Estimation

For a given N in an N-gram, a gram's **Maximum Likelihood Estimation** is calculated as the gram's usage count in the training corpus divided by the preceding lower-order gram's usage count in the training corpus, as shown by the formula below:

$$P_{MLE}(w_n \\ldots w_1) = P(w_n|w_{n-1} \\ldots w_1) = \\frac{Count(w_n \\ldots w_1)}{Count(w_{n-1} \\ldots w_1)}$$

## N-Gram Construction

*** explain how an N-gram is constructed ***

## Model Test

*** Explain how the model is tested ***
