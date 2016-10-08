## Model Test

Various experimentations and iterations of the predictive model were tested as follows:

1. Sample 10% of the **non**-training data in the English Corpus to create a test dataset;
1. Build a bi, tri, quad, and pentagrams from the test dataset using the same techniques as when building the model (i.e., RWeka tokenizer, Document Term Matrix, and operations on the latter);
1. For each N-gram, build a `data.table` with the following columns:
    + `gram`: The gram;
    + `lowergram`: The gram's immediate lower-order gram;
    + `word_to_predict`: the gram's last word;
* Sample 25,000 grams from each N-gram (N=2 to 5), and `rbind` the resulting samples into a 100,000-row table;
* **Success Criteria**: For each `gram` in the test `data.table`, run the Stupid Backoff prediction on the `lowergram`, and if the resulting 10 predictions contain the gram's `word_to_predict`, then the test is a PASS, else the test is a FAIL.

Using the above test methodology, our model achieved a **55% success rate**.
