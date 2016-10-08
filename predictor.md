We implemented a variation of the [Stupid Backoff Algorithm](https://web.stanford.edu/~jurafsky/slp3/4.pdf) as follows:

1. Lookup the top 10 matches in the highest order N-gram table in the model, where lowergram = last (N-1) words in the input sentence;
1. If less than 10 matches were found, **back off** and look up for additional matches in the (N-1)-gram table;
1. At each backoff, *discount* the matches' MLE probabilities by applying a $\lambda$ weight. (e.g., $\lambda=0.4$): $P_{MLE(weighted)}(w_{1} \ldots w_{N-1}w_{match}) = \lambda^{(4 - (N-1))} * P_{MLE}(w_{1} \ldots w_{N-1}w_{match})$
1. Repeat the previous step until 10 matches are found, using the unigrams table as the last lookup step.
1. Return the top 10 matches (or predictions), sorted by $\lambda\$-weighted MLE descending.

The algorithm's flow chart is shown below.
