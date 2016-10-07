require(dplyr)
require(data.table)

source('config.R')
source('predictor.R')

partitionRate = 10
sampleRate = 5

# Test bigrams
testBigramsDTPathName = paste0(g_corpus_directory_en, '/test_bigrams_DT_partition_', sprintf('%.1f', 100-partitionRate), '_sample_', sprintf('%.1f', sampleRate), '.rds')
testBigramsDT = readRDS(file = testBigramsDTPathName)

# Test trigrams
testTrigramsDTPathName = paste0(g_corpus_directory_en, '/test_trigrams_DT_partition_', sprintf('%.1f', 100-partitionRate), '_sample_', sprintf('%.1f', sampleRate), '.rds')
testTrigramsDT = readRDS(file = testTrigramsDTPathName)

# Test quadgrams
testQuadgramsDTPathName = paste0(g_corpus_directory_en, '/test_quadgrams_DT_partition_', sprintf('%.1f', 100-partitionRate), '_sample_', sprintf('%.1f', sampleRate), '.rds')
testQuadgramsDT = readRDS(file = testQuadgramsDTPathName)

# Test pentagrams
testPentagramsDTPathName = paste0(g_corpus_directory_en, '/test_pentagrams_DT_partition_', sprintf('%.1f', 100-partitionRate), '_sample_', sprintf('%.1f', sampleRate), '.rds')
testPentagramsDT = readRDS(file = testPentagramsDTPathName)

set.seed(10)
testNGramSize = 25000
testData = rbind(
    sample_n(testBigramsDT, testNGramSize),
    sample_n(testTrigramsDT, testNGramSize),
    sample_n(testQuadgramsDT, testNGramSize),
    sample_n(testPentagramsDT, testNGramSize)
)

sample_n(testData, size = 10)
