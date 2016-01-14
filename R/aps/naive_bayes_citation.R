#################### naive bayes citation classifier ######################

words <- readLines("words.txt")

dois <- readLines("dois.txt")

docs <- readLines("aps-title-citations.txt")

citations <- as.numeric(readLines("aps-cite-citations.txt"))

treshold <- round(mean(citations))

class <- rep(0, length(citations))

for (i in c(1:length(citations))) {

    if (citations[i] > treshold) {
        class[i] <- 1
    }
    cat(i,"\n")

}


##### split data into train and test set ######

#id.trainset <- sample(c(1:round(length(docs))),round(length(docs)/2) )

id.trainset <- c(1:20000)

train.docs <- docs[id.trainset]
train.class <- class[id.trainset]

#test.docs <- docs[-id.trainset]
#test.class <- class[-id.trainset]

id.testset <- c(20001:40000)
test.docs <- docs[id.testset]
test.class <- class[id.testset]


train.docs.cited <- train.docs[which(train.class==1)]
train.docs.notcited <- train.docs[which(train.class==0)]

cited.words <- c()
for (i in c(1:length(train.docs.cited))) {

    cited.words <- c(cited.words, strsplit(train.docs.cited[i]," ")[[1]])
    cat(i,"\n")   
}

notcited.words <- c()
for (i in c(1:length(train.docs.notcited))) {

    notcited.words <- c(notcited.words, strsplit(train.docs.notcited[i]," ")[[1]])
    cat(i,"\n")   
}



################# vocabulary and class word freqs ############ 

all.words <- c(cited.words,notcited.words)

vocabulary <- sort(table(all.words),decreasing=T)

v <- length(vocabulary)

cited.freq <- sort(table(cited.words),decreasing=T)
notcited.freq <- sort(table(notcited.words),decreasing=T)

n.cited <- sum(cited.freq)
n.notcited <- sum(notcited.freq)



#write.table(cbind(names(vocabulary),as.numeric(vocabulary)), "nb-vocabulary-1.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)
#write.table(cbind(names(cited.freq),as.numeric(cited.freq)), "nb-cited-1.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)
#write.table(cbind(names(notcited.freq),as.numeric(notcited.freq)), "nb-notcited-1.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)


##### class priors #############

cited.prior <- length(train.docs.cited)/length(train.docs)
notcited.prior <- 1 - cited.prior



#### classify #######

classify <- function(w) {

alpha = 1

### class a ###

p.word.a <- c()

for (word in w) {

    freq <- as.numeric(cited.freq[which(word==names(cited.freq))])

    if ( length(freq) > 0 ) {

        n.w.a <- freq

        p.w.a <- (n.w.a + 1*alpha)/(n.cited + v*alpha)

    } else {

        p.w.a <- 1/(n.cited + v + 1)

    }
    
    p.word.a <- c(p.word.a,p.w.a)
    
}

#c.a <- p.a*prod(p.word.a)

c.a <- log(cited.prior) + sum(log(p.word.a))

### class b ###


p.word.b <- c()

for (word in w) {

    freq <- as.numeric(notcited.freq[which(word==names(notcited.freq))])

    if ( length(freq) > 0 ) {

        n.w.b <- freq

        p.w.b <- (n.w.b + 1*alpha)/(n.notcited + v*alpha)

    } else {

        p.w.b <- 1/(n.notcited + v + 1)

    }
    
    p.word.b <- c(p.word.b,p.w.b)
    
}

#c.b <- p.b*prod(p.word.b)

c.b <- log(notcited.prior) + sum(log(p.word.b))



c.class <- c(c.a,c.b)

#return(j.class[which(c.class==max(c.class))])

return(which(c.class==max(c.class)))

}


####  classify titles from train set #####

n.class <- c()
sample.id <- sample(c(1:length(train.docs)),500)
for (i in sample.id) {

    w <- strsplit(train.docs[i], " ")[[1]]
    n.class <- c(n.class,classify(w))
    cat(i,"\n")


}

#### precision and recall with training set ######

correct.class <- train.class[sample.id]

classified <- abs(n.class-2)

#length(which(correct.class==1))

#length(which(classified==1))

tp <- length(which(correct.class+classified==2))

tn <- length(which(correct.class+classified==0))

fn <- length(which(correct.class-classified==1))

fp <- length(which(correct.class-classified==-1))

accu <- (tp+tn)/(tp+tn+fp+fn)

prec <- tp/(tp+fp)

rec <- tp/(tp+fn)

f1 <- 2*prec*rec/(prec+rec)


#### precision and recall with test set #####


n.class <- c()
sample.id <- sample(c(1:length(test.docs)),500)
for (i in sample.id) {

    w <- strsplit(test.docs[i], " ")[[1]]
    n.class <- c(n.class,classify(w))
    cat(i,"\n")


}


correct.class <- test.class[sample.id]

classified <- abs(n.class-2)

#length(which(correct.class==1))

#length(which(classified==1))

tp <- length(which(correct.class+classified==2))

tn <- length(which(correct.class+classified==0))

fn <- length(which(correct.class-classified==1))

fp <- length(which(correct.class-classified==-1))

accu <- (tp+tn)/(tp+tn+fp+fn)

prec <- tp/(tp+fp)

rec <- tp/(tp+fn)

f1 <- 2*prec*rec/(prec+rec)

