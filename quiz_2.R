# Quiz #2 with the 3-gram: 5/10
trigramsDat2[grepl('^case of(\\s+)', gram)][grepl('(\\s)pretzels$|(\\s)cheese$|(\\s)beer$|(\\s)soda$', gram)][order(logprob, decreasing = TRUE)] # No match
trigramsDat2[grepl('^mean the(\\s+)', gram)][grepl('(\\s)most$|(\\s)universe$|(\\s)best$|(\\s)world$', gram)][order(logprob, decreasing = TRUE)] # Match
trigramsDat2[grepl('^me the(\\s+)', gram)][grepl('(\\s)happiest$|(\\s)bluest$|(\\s)saddest$|(\\s)smelliest$', gram)][order(logprob, decreasing = TRUE)] # Match
trigramsDat2[grepl('^but the(\\s+)', gram)][grepl('(\\s)referees$|(\\s)crowd$|(\\s)defense$|(\\s)players$', gram)][order(logprob, decreasing = TRUE)] # No match
trigramsDat2[grepl('^at the(\\s+)', gram)][grepl('(\\s)movies$|(\\s)mall$|(\\s)beach$|(\\s)grocery$', gram)][order(logprob, decreasing = TRUE)] # Match
trigramsDat2[grepl('^on my(\\s+)', gram)][grepl('(\\s)way$|(\\s)phone$|(\\s)motorcyle$|(\\s)horse$', gram)][order(logprob, decreasing = TRUE)] # Match
trigramsDat2[grepl('^quite some(\\s+)', gram)][grepl('(\\s)time$|(\\s)years$|(\\s)thing$|(\\s)weeks$', gram)][order(logprob, decreasing = TRUE)] # Match
trigramsDat2[grepl('^his little(\\s+)', gram)][grepl('eyes|fingers|toes|ears', gram)][order(logprob, decreasing = TRUE)] # No match
trigramsDat2[grepl('^during the(\\s+)', gram)][grepl('(\\s)hard|(\\s)bad|(\\s)sad|(\\s)worse', gram)][order(logprob, decreasing = TRUE)] # No match
trigramsDat2[grepl('^must be(\\s+)', gram)][grepl('(\\s)asleep$|(\\s)insane$|(\\s)insensitive$|(\\s)callous$', gram)][order(logprob, decreasing = TRUE)] # No match

# Quiz #2 with the 4-gram: 6/10
quadgramsDat3 = quadgramsDat2
setkey(quadgramsDat3, 'lowergram')
key(quadgramsDat3)
quadgramsDat3['a case of'][grepl('(\\s)pretzels$|(\\s)cheese$|(\\s)beer$|(\\s)soda$', gram)][order(logprob, decreasing = TRUE)] # Match

quadgramsDat3['would mean the'][grepl('(\\s)most$|(\\s)universe$|(\\s)best$|(\\s)world$', gram)][order(logprob, decreasing = TRUE)] # Match

quadgramsDat3['make me the'][grepl('(\\s)happiest$|(\\s)bluest$|(\\s)saddest$|(\\s)smelliest$', gram)][order(logprob, decreasing = TRUE)] # Match

quadgramsDat3['struggling but the'][grepl('(\\s)referees$|(\\s)crowd$|(\\s)defense$|(\\s)players$', gram)][order(logprob, decreasing = TRUE)] # No match
trigramsDat3['but the'][grepl('(\\s)referees$|(\\s)crowd$|(\\s)defense$|(\\s)players$', gram)][order(logprob, decreasing = TRUE)] # Matched crowd

quadgramsDat3['date at the'][grepl('(\\s)movies$|(\\s)mall$|(\\s)beach$|(\\s)grocery$', gram)][order(logprob, decreasing = TRUE)] # Matched grocery
trigramsDat3['at the'][grepl('(\\s)movies$|(\\s)mall$|(\\s)beach$|(\\s)grocery$', gram)][order(logprob, decreasing = TRUE)] # Match beach

quadgramsDat3['be on my'][grepl('(\\s)way$|(\\s)phone$|(\\s)motorcyle$|(\\s)horse$', gram)][order(logprob, decreasing = TRUE)] # Match

quadgramsDat3['in quite some'][grepl('(\\s)time$|(\\s)years$|(\\s)thing$|(\\s)weeks$', gram)][order(logprob, decreasing = TRUE)] # Match

quadgramsDat3['with his little'][grepl('(\\s)eyes$|(\\s)fingers$|(\\s)toes$|(\\s)ears$', gram)][order(logprob, decreasing = TRUE)] # No match
trigramsDat3['his little'][grepl('(\\s)eyes$|(\\s)fingers$|(\\s)toes$|(\\s)ears$', gram)][order(logprob, decreasing = TRUE)] # No match
bigramsDat2[grepl('^little(\\s+)', gram)][grepl('(\\s)eyes$|(\\s)fingers$|(\\s)toes$|(\\s)ears$', gram)][order(logprob, decreasing = TRUE)] # Match with 'ears'

quadgramsDat3['faith during the'][grepl('(\\s)hard$|(\\s)bad$|(\\s)sad$|(\\s)worse$', gram)][order(logprob, decreasing = TRUE)] # No match
trigramsDat3['during the'][grepl('(\\s)hard$|(\\s)bad$|(\\s)sad$|(\\s)worse$', gram)][order(logprob, decreasing = TRUE)] # No match
bigramsDat2[grepl('^the(\\s+)', gram)][grepl('(\\s)hard$|(\\s)bad$|(\\s)sad$|(\\s)worse$', gram)][order(logprob, decreasing = TRUE)] # Match

quadgramsDat3['you must be'][grepl('(\\s)asleep$|(\\s)insane$|(\\s)insensitive$|(\\s)callous$', gram)][order(logprob, decreasing = TRUE)] # No match
trigramsDat3['must be'][grepl('(\\s)asleep$|(\\s)insane$|(\\s)insensitive$|(\\s)callous$', gram)][order(logprob, decreasing = TRUE)] # No match
bigramsDat2[grepl('^be(\\s+)', gram)][grepl('(\\s)asleep$|(\\s)insane$|(\\s)insensitive$|(\\s)callous$', gram)][order(logprob, decreasing = TRUE)] # Match 'asleep' as first choice.