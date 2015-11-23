library(edgeR)


words <- readLines("diabetes-words-cleaned.txt")
timestamps <- as.numeric(readLines("diabetes-times-cleaned.txt"))

wordstream <- words
wordyears <- timestamps

tf <- sort(table(words), decreasing=T)
yrs <- as.numeric(names(table(wordyears)))


x <- as.numeric(tf)

freq <- goodTuring(x)

###### test: total words #######
sum(freq$n*freq$count)
length(words)

######################### total probability of observing frequencies ################################


sum(freq$proportion*freq$n)+freq$P0

####### probability of observing words with freq = f ############################


f = 50
id <- which(freq$count == f)
freq$proportion[id]


####################################### good-turing prediction & comparison with data ######################################


subset.yrs <- yrs[89:154]

gt <- c()
p.exp <- c() 

for (i in c(2:(length(subset.yrs)))){

    ids <- intersect(which(wordyears >= subset.yrs[1]),which(wordyears < subset.yrs[i]))

    #ids <- which(wordyears == subset.yrs[i-1])

    text <- words[ids]

    x.lex <- sort(table(text), decreasing=T)
    
    x <- as.numeric(x.lex)
    
    nr <- as.numeric(sort(table(x),decreasing=T))
    
    gt <- c(gt, nr[1]/sum(x))
    
    text1 <- words[which(wordyears == subset.yrs[i])]
    
    x1.lex <- sort(table(text1), decreasing=T)
    
    ids <- setdiff(names(x1.lex),names(x.lex))
    
    p.exp <- c(p.exp, sum(as.numeric(x1.lex[ids]))/sum(as.numeric(x1.lex)))
    
    #p.exp <- c(p.exp ,1 - length(intersect(names(x1.lex),names(x.lex)))/length(x1.lex))
    
    cat(i,"\n")
    
}

###################################### plot ###########################################

x <- subset.yrs[2:length(subset.yrs)]

lw1 <- loess(gt ~ x, span=0.25)
ft1 <- predict(lw1)

lw2 <- loess(p.exp ~ x)
ft2 <- predict(lw2)

plot(subset.yrs[2:length(subset.yrs)],p.exp, xlab="year", ylab="Good-Turing p0, prob. of new words")
points(x,gt, pch=8)
lines(lw1$x,ft1)
lines(lw2$x,ft2)

#########################################################################################