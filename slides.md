<style>
.reveal h1, .reveal h2, .reveal h3 {
  word-wrap: normal;
  -moz-hyphens: none;
}
.small-code pre code {
  font-size: 1em;
}
</style>

Next Word Predictor
========================================================
class: small-code
author: Coursera Data Science Capstone Project, Bertrand Rigaldies, October 8, 2016
autosize: true
font-family: 'Helvetica'
transition: rotate
width: 1440
height: 900



* An N-grams-based probabilistic [model](#/model) to predict the next most likely words in an english sentence (See example of R output below);
* Using Maximum Likelihood Estimates (MLE) probabilities, and [Stupid Backoff Algorithm](#/predictor)-based prediction;
* Achieving about 55% prediction [success rate](#/performance);
* Deployed on [Shinyapps.io](#/shinyapp).




```r
predictWithStupidBackoff(model = model, sentence = 'as far as I can', matches_count_max = 5)
```

```
                gram   logprob last_word_in_gram      sprob
1: as i can remember -1.953815          remember 0.14173228
2:     as i can tell -2.446292              tell 0.08661417
3:        as i can i -2.541602                 i 0.07874016
4:  as i can because -2.764746           because 0.06299213
5:      as i can and -3.234749               and 0.03937008
```

The Probabilistic Model
========================================================
id: model
class: small-code

* English Corpus from [HC Corpora](www.corpora.heliohost.org).
* 10% of the English Corpus used as training dataset.
* 5 N-grams tables built from the training dataset, N = 1 to 5.
* Each N-gram table is structured as follows:

Column | Description
------------- | -------------
**gram** | The gram as a string of one ($N=1$) or more ($N\gt2$) words.
**count** | The number of times the gram is used in the Corpus training dataset.
**lowergram** | The preceding lower-order gram when $N\gt2$, or "lowergram" (e.g., "in the" in gram "in the world").
**last_word_in_gram** | The last word in the gram (e.g., "world" in gram "in the world").
**logprob** | Logarithm of the gram's **Maximum Likelihood Estimation** (MLE).
**logpercent** | Logarithm of the gram's **Percent Use** (PU) based on the gram's count.

**Maximum Likelihood Estimation**: $P_{MLE}(w_1 \ldots w_n) = P(w_n|w_1 \ldots w_{n-1}) = \frac{Count(w_1 \ldots w_n)}{Count(w_1 \ldots w_{n-1})}$

**Percent Use**: $P_{PU}(w_1 \ldots w_n) = \frac{Count(w_1 \ldots w_n)}{Count(n-Grams)}$

The Stupid Backoff Algorithm
========================================================
id: predictor

Reference: [Speech and Language Processing, Daniel Jurafsky & James H. martin, January 9, 2015, Chapter 4: N-Grams, section 4.5.1 The Web and Stupid Backoff](https://web.stanford.edu/~jurafsky/slp3/4.pdf)

The basic flow is:

1. Lookup the top 10 matches in the highest order N-gram table in the model, where lowergram = last (N-1) words in the input sentence;
1. If less than 10 matches were found, **back off** and look up for additional matches in the (N-1)-gram table;
1. At each backoff, *discount* the matches' MLE probabilities by applying a $\lambda$ weight. (e.g., $\lambda=0.4$): $P_{MLE(weighted)}(w_{1} \ldots w_{N-1}w_{match}) = \lambda^{(4 - (N-1))} * P_{MLE}(w_{1} \ldots w_{N-1}w_{match})$
1. Repeat the previous step until 10 matches are found, using the unigrams table as the last lookup step;
1. Return the top 10 matches (or predictions), sorted by $\lambda\$-weighted MLE descending.

Quantitative Predictive Performance: ~55%
========================================================
id: performance
class: small-code

* Sample 10% of the non-training data in the English Corpus to create a test dataset;
* Build a bi, tri, quad, and pentagrams from the test dataset;
* For each N-gram, build a `data.table` with the following columns: a) `gram`: The gram; b) `lowergram`: The gram's immediate lower-order gram; c) `word_to_predict`: the gram's last word;
* Sample 25,000 grams from each N-gram (N=2 to 5), and `rbind` the resulting samples into a 100,000-row table (Samples of the resulting test data shown below):

```
                    gram   lowergram word_to_predict
1:        are we to make   are we to            make
2: make better decisions make better       decisions
3:          its too much     its too            much
```
* **Success Criteria**: For each `gram` in the test `data.table`, run the Stupid Backoff prediction on the `lowergram`, and if the resulting 10 predictions contain the gram's `word_to_predict`, then the test is a PASS, else the test is a FAIL (Test samples shown below):
<table border=1>
<tr> <th>  </th> <th> Test Gram </th> <th> Word to Predict </th> <th> Top 10 Predicted Words </th> <th> Test Result </th>  </tr>
  <tr> <td> 1 </td> <td> i feel like being </td> <td> being </td> <td> i, im, a, the, it, ive, its, my, such, this </td> <td> FAIL </td> </tr>
  <tr> <td> 2 </td> <td> that matters to </td> <td> to </td> <td> is, to, its, what, of, worse, most, and, the, in </td> <td> PASS </td> </tr>
  <tr> <td> 3 </td> <td> to the vast </td> <td> vast </td> <td> next, point, public, new, us, top, world, city, hospital, state </td> <td> FAIL </td> </tr>
   </table>

My Shinyapps.io Application
========================================================
id: shinyapp

[![Next Word Predictor 1.0](shinyappio.jpg)](https://brigaldies.shinyapps.io/NextWordPredictor/)
