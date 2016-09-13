freqTop = head(freq, n = 100)
wordcloud(names(freqTop), freqTop, scale=c(5, .1), colors=brewer.pal(6, "Set1"))
