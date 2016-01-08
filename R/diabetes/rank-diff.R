###### rank diff #########

################################# rank diff with diabetes corpus #########################


words <- readLines("words-high-cited.txt")
timestamps <- as.numeric(readLines("timestamps-high-cited.txt"))
cites <- as.numeric(readLines("citationstamps-high-cited.txt"))

wordstream <- words
wordyears <- timestamps

tf <- sort(table(words), decreasing=T)
yrs <- as.numeric(names(table(wordyears)))

tf.high <- tf
 
 
words <- readLines("diabetes-words-cleaned.txt")
timestamps <- as.numeric(readLines("diabetes-times-cleaned.txt"))
 
wordstream <- words
wordyears <- timestamps

tf <- sort(table(words), decreasing=T)
yrs <- as.numeric(names(table(wordyears)))

tf.diabetes <- tf



joint.id.diabetes <- which(names(tf.diabetes) %in% names(tf.high))
joint.id.high <- which(names(tf.high) %in% names(tf.diabetes))

lex.diabetes <- tf.diabetes[joint.id.diabetes]
lex.high <- tf.high[joint.id.high]

rank.high <- c()
f.high <- as.numeric(lex.high)/sum(as.numeric(lex.high))
f.diabetes <- as.numeric(lex.diabetes)/sum(as.numeric(lex.diabetes))
f.diabetes.high <- c()

for (w in names(lex.diabetes)) {

    id <- which(names(lex.high) == w)
    rank.high <- c(rank.high,id)
    f.diabetes.high <- c(f.diabetes.high,f.high[id])
    
    

}

rank.diff <- c(1:length(lex.diabetes)) - rank.high

score <- rank.diff*f.diabetes*f.diabetes.high


###################################################### rank diff wrt years ##############################

start <- 1950
yr1 <- 1952
yr2 <- 1953

tf.yr1 <- sort(table(words[which(wordyears == yr1)]), decreasing=T)
#tf.yr1 <- sort(table(words[intersect(which(wordyears >= start),which(wordyears < yr1))]), decreasing=T)
tf.yr2 <- sort(table(words[which(wordyears == yr2)]), decreasing=T)
#tf.yr2 <- sort(table(words[intersect(which(wordyears >= start),which(wordyears < yr2))]), decreasing=T)


joint.id.yr1 <- which(names(tf.yr1) %in% names(tf.yr2))
joint.id.yr2 <- which(names(tf.yr2) %in% names(tf.yr1))

lex.yr1 <- tf.yr1[joint.id.yr1]
lex.yr2 <- tf.yr2[joint.id.yr2]

rank.yr2 <- c()
f.yr2 <- as.numeric(lex.yr2)/sum(as.numeric(lex.yr2))
f.yr1 <- as.numeric(lex.yr1)/sum(as.numeric(lex.yr1))
f.yr1.yr2 <- c()

for (w in names(lex.yr1)) {

    id <- which(names(lex.yr2) == w)
    rank.yr2 <- c(rank.yr2,id)
    f.yr1.yr2 <- c(f.yr1.yr2,f.yr2[id])
    
    

}

rank.diff <- c(1:length(lex.yr1)) - rank.yr2

score <- rank.diff*f.yr1*f.yr1.yr2

######################### which top names in next year have highest rank jump? ###############################

n <- 500
id <- which(names(lex.yr1) %in% names(lex.yr2[1:n]))
rank.id <- sort.int(rank.diff[id],decreasing=F,index.return=T)$ix
score.id <- sort.int(score[id],decreasing=F,index.return=T)$ix

ntop <- 100

#names(lex.yr1)[id[rank.id]][1:100]

#names(lex.yr1)[id[score.id]][1:100]

v1 <- intersect(names(lex.yr1)[id[rank.id]][1:ntop],names(lex.yr1)[id[score.id]][1:ntop])


v2 <- intersect(names(lex.yr1)[id[score.id]][1:ntop],names(lex.yr1)[id[rank.id]][1:ntop])

intersect(v2,v1)
