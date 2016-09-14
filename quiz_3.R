# -----------------------------------------------------------------------------
# QUiz #3: 4/10!
# -----------------------------------------------------------------------------
execTime = system.time({
    sentenceTest = "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
    choices = c('give', 'eat', 'die', 'sleep')
    answerQuiz(sentenceTest, choices) # give WRONG
})
message(paste('Prediction run in', round(execTime["elapsed"], 2), 'secs'))

execTime = system.time({
    sentenceTest = "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
    choices = c('financial', 'marital', 'horticultural', 'spiritual')
    answerQuiz(sentenceTest, choices) # Financial WRONG
})
message(paste('Prediction run in', round(execTime["elapsed"], 2), 'secs'))

execTime = system.time({
    sentenceTest = "I'd give anything to see arctic monkeys this"
    choices = c('month', 'morning', 'weekend', 'decade')
    answerQuiz(sentenceTest, choices) # morning WRONG
}) 
message(paste('Prediction run in', round(execTime["elapsed"], 2), 'secs'))

execTime = system.time({
    sentenceTest = "Talking to your mom has the same effect as a hug and helps reduce your"
    choices = c('sleepiness', 'happiness', 'stress', 'hunger')
    answerQuiz(sentenceTest, choices) # stress RIGHT
})
message(paste('Prediction run in', round(execTime["elapsed"], 2), 'secs'))

execTime = system.time({
    sentenceTest = "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
    choices = c('picture', 'minute', 'walk', 'look')
    answerQuiz(sentenceTest, choices) # look WRONG
})
message(paste('Prediction run in', round(execTime["elapsed"], 2), 'secs'))

execTime = system.time({
    sentenceTest = "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
    choices = c('case', 'mater', 'account', 'incident')
    answerQuiz(sentenceTest, choices) # case WRONG
})
message(paste('Prediction run in', round(execTime["elapsed"], 2), 'secs'))

execTime = system.time({
    sentenceTest = "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
    choices = c('finger', 'arm', 'hand', 'toe')
    answerQuiz(sentenceTest, choices) # hand RIGHT
})
message(paste('Prediction run in', round(execTime["elapsed"], 2), 'secs'))

execTime = system.time({
    sentenceTest = "Every inch of you is perfect from the bottom to the"
    choices = c('top', 'side', 'center', 'middle')
    answerQuiz(sentenceTest, choices) # top RIGHT
})
message(paste('Prediction run in', round(execTime["elapsed"], 2), 'secs'))

execTime = system.time({
    sentenceTest = "I'm thankful my childhood was filled with imagination and bruises from playing"
    choices = c('inside', 'outside', 'weekly', 'daily')
    answerQuiz(sentenceTest, choices) # outside RIGHT
})
message(paste('Prediction run in', round(execTime["elapsed"], 2), 'secs'))

execTime = system.time({
    sentenceTest = "I like how the same people are in almost all of Adam Sandler's"
    choices = c('stories', 'novels', 'movies', 'pictures')
    answerQuiz(sentenceTest, choices) # NULL
})
message(paste('Prediction run in', round(execTime["elapsed"], 2), 'secs'))

unigramsDat[grepl('^stories$|^novels$|^movies$|^pictures$', gram)][order(logprob, decreasing = TRUE)] # Stories is most probable