library(igraph)

words <- readLines("/Users/dean/code/percipio/models/aps/words.txt")

#words <- readLines("/Users/dean/code/percipio/models/tolstoy/words.txt")

words <- words[1:500000]

write.table(words, "/Users/dean/code/c++/correlations/seg-words.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)

tf <- sort(table(words),decreasing=T)

system("time /Users/dean/code/c++/correlations/main /Users/dean/code/c++/correlations/seg-words.txt > /Users/dean/code/c++/correlations/seg-corr.txt")

corr <- read.csv("/Users/dean/code/c++/correlations/seg-corr.txt", header=F, stringsAsFactors=F)

g <- graph.data.frame(corr, directed=F)

c <- 2*ecount(g)/(vcount(g)*(vcount(g)-1))

rwords <- sample(words, length(words))

d <- words

x <- ceiling(10^(seq(0,log(length(words),10),by=log(length(words),10)/100)))

y <- c()

for (nwords in x) {

s <- split(d, ceiling(seq_along(d)/nwords))

shuffle <- sample(c(1:length(s)),length(s))

rwords <- as.character(unlist(s[shuffle]))

write.table(rwords, "/Users/dean/code/c++/correlations/seg-words-r.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)

system("time /Users/dean/code/c++/correlations/main /Users/dean/code/c++/correlations/seg-words-r.txt > /Users/dean/code/c++/correlations/seg-corr-r.txt")

corr <- read.csv("/Users/dean/code/c++/correlations/seg-corr-r.txt", header=F, stringsAsFactors=F)

g1 <- graph.data.frame(corr, directed=F)

int <- graph.intersection(g,g1)
ec <- E(int)$V3_1
ec1 <- E(int)$V3_2
p <- sum(abs(ec-ec1)/(ec+ec1))/ecount(int)

cint <- 2*ecount(int)/(vcount(int)*(vcount(int)-1))
    
y <- c(y,p*cint/c)

cat("p: ",p," cint/ct:",cint/c," nwords:",nwords,"\n")

}
