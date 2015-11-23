library(barcode)

words <- readLines("diabetes-words-cleaned.txt")
timestamps <- as.numeric(readLines("diabetes-times-cleaned.txt"))

wordstream <- words
wordyears <- timestamps

tf <- sort(table(words), decreasing=T)
yrs <- as.numeric(names(table(wordyears)))



######################################### C measure, P. Carpena et al., PHYSICAL REVIEW E 79, 035102R 2009 ###################


ids <- intersect(which(wordyears >= 2010),which(wordyears < 2015))

yr <- "2015"



#text <- words[which(wordyears == yr)]

#text <- words[ids]

text <- words

tf.temp <- sort(table(text), decreasing=T)

tokens <- names(tf.temp)

n <- as.numeric(tf.temp)

#tokens <- tokens[which(n > 0.0005*length(text))]

#n <- n[which(n > 0.0005*length(text))]

tokens <- tokens[1:100]

n <- n[1:100]

c <- c()

sigma.norm <- c()

for (k in c(1:length(tokens))) {

    
    w <- tokens[k]

    e <- which(text == w)

    d <- diff(e)

    mean.d <- mean(d)

    norm.d <- d/mean.d

    sd.d <- sd(norm.d)

    p <- 1/mean.d

    norm.sd <- sd.d/sqrt(1-p)

    av.norm.sd <- (2*n[k]-1)/(2*n[k]+2)

    sd.av.norm.sd <- 1/(sqrt(n[k])*(1+2.8*n[k]^(-.865)))
    
    sigma.norm <- c(sigma.norm, norm.sd)

    c <- c(c,(norm.sd - av.norm.sd)/sd.av.norm.sd)


}

c <- c[!is.na(c)]

ctokens <- tokens[1:length(c)]

id <- sort.int(c,decreasing=T, index.return=T)$ix

keywords.c <- ctokens[id]

id <- sort.int(sigma.norm,decreasing=T, index.return=T)$ix

keywords.s <- ctokens[id]

top <- intersect(keywords.s[1:100],tokens[1:100])


################ plot example of words levels in given timespan ##########################



ids <- intersect(which(wordyears >= 1950),which(wordyears < 2015))

text <- words[ids]

elow <- which(text == "alloxan-diabetic")
ehigh <- which(text == "β-cell")

x <- list("β-cell"=ehigh,"low"=elow)

barcode(x,outerbox =F, xlim=c(0,1.4*10^6))

#################################### plot levels of few top frequent words in a sequence ###################


text <- words[(3000000):(3002000)]

tf.tmp <- sort(table(text),decreasing=T)[1:10]

nm <- names(tf.tmp)

n <- as.numeric(tf.tmp)

el <- c()

for (k in c(1:length(nm))) {

    e <- which(text == nm[k])
    
    el <- c(el, list(e))
    
}

x <- setNames(as.list(el),nm)

barcode(x,outerbox =F, xlim=c(0,2000))

