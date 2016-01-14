#################### naive bayes classifier ######################

words <- readLines("words.txt")

dois <- readLines("dois.txt")


j.class <- c("PhysRevA","PhysRevB","PhysRevC","PhysRevD","PhysRevE")

##### local vocabs and global vocab

id.a <- grep("PhysRevA",dois)
tf.a <- sort(table(words[id.a]),decreasing=T)
n.a <- sum(tf.a)

id.b <- grep("PhysRevB",dois)
tf.b <- sort(table(words[id.b]),decreasing=T)
n.b <- sum(tf.b)

id.c <- grep("PhysRevC",dois)
tf.c <- sort(table(words[id.c]),decreasing=T)
n.c <- sum(tf.c)

id.d <- grep("PhysRevD",dois)
tf.d <- sort(table(words[id.d]),decreasing=T)
n.d <- sum(tf.d)

id.e <- grep("PhysRevE",dois)
tf.e <- sort(table(words[id.e]),decreasing=T)
n.e <- sum(tf.e)

id <- c(id.a,id.b,id.c,id.d,id.e)
tf <- sort(table(words[id]),decreasing=T)

v <- length(tf)

alpha = 1

##### doc priors

docs <- readLines("aps-title-citations.txt")

class <- readLines("aps-doi-citations.txt")

id.a <- grep("PhysRevA",class)
id.b <- grep("PhysRevB",class)
id.c <- grep("PhysRevC",class)
id.d <- grep("PhysRevD",class)
id.e <- grep("PhysRevE",class)
id <- c(id.a,id.b,id.c,id.d,id.e)

p.a <- length(id.a)/length(id)
p.b <- length(id.b)/length(id)
p.c <- length(id.c)/length(id)
p.d <- length(id.d)/length(id)
p.e <- length(id.e)/length(id)

p.class <- c(p.a,p.b,p.c,p.d,p.e)




classify <- function(w) {

### class a ###

p.word.a <- c()

for (word in w) {

    freq <- as.numeric(tf.a[which(word==names(tf.a))])

    if ( length(freq) > 0 ) {

        n.w.a <- freq

        #n.a <- length(tf.a)


        p.w.a <- (n.w.a + alpha)/(n.a + alpha*v)

    } else {

        p.w.a <- 1/(n.a + v + 1)

    }
    
    p.word.a <- c(p.word.a,p.w.a)
    
}

#c.a <- p.a*prod(p.word.a)

c.a <- log(p.a) + sum(log(p.word.a))

### class b ###


p.word.b <- c()

for (word in w) {

    freq <- as.numeric(tf.b[which(word==names(tf.b))])

    if ( length(freq) > 0 ) {

        n.w.b <- freq

        #n.b <- length(tf.b)

        

        p.w.b <- (n.w.b + alpha)/(n.b + alpha*v)

    } else {

        p.w.b <- 1/(n.b + v + 1)

    }
    
    p.word.b <- c(p.word.b,p.w.b)
    
}

#c.b <- p.b*prod(p.word.b)

c.b <- log(p.b) + sum(log(p.word.b))

### class c

p.word.c <- c()

for (word in w) {

    freq <- as.numeric(tf.c[which(word==names(tf.c))])

    if ( length(freq) > 0 ) {

        n.w.c <- freq

        #n.c <- length(tf.c)


        p.w.c <- (n.w.c + alpha)/(n.c + alpha*v)

    } else {

        p.w.c <- 1/(n.c + v + 1)

    }
    
    p.word.c <- c(p.word.c,p.w.c)
    
}

#c.c <- p.c*prod(p.word.c)

c.c <- log(p.c) + sum(log(p.word.c))

##### class d

p.word.d <- c()

for (word in w) {

    freq <- as.numeric(tf.d[which(word==names(tf.d))])

    if ( length(freq) > 0 ) {

        n.w.d <- freq

        #n.d <- length(tf.d)


        p.w.d <- (n.w.d + alpha)/(n.d + alpha*v)

    } else {

        p.w.d <- 1/(n.d + v + 1)

    }
    
    p.word.d <- c(p.word.d,p.w.d)
    
}

#c.d <- p.d*prod(p.word.d)

c.d <- log(p.d) + sum(log(p.word.d))

### class e

p.word.e <- c()

for (word in w) {

    freq <- as.numeric(tf.e[which(word==names(tf.e))])

    if ( length(freq) > 0 ) {

        n.w.e <- freq

        #n.e <- length(tf.e)


        p.w.e <- (n.w.e + alpha)/(n.e + alpha*v)

    } else {

        p.w.e <- 1/(n.e + v + 1)

    }
    
    p.word.e <- c(p.word.e,p.w.e)
    
}

#c.e <- p.e*prod(p.word.e)

c.e <- log(p.e) + sum(log(p.word.e))

##########

c.class <- c(c.a,c.b,c.c,c.d,c.e)

#return(j.class[which(c.class==max(c.class))])

return(which(c.class==max(c.class)))

}


####  compute confusion matrix ##### 

n.class <- c()
sample.id <- sample(id.a,500)
for (i in sample.id) {

    w <- strsplit(docs[i], " ")[[1]]
    n.class <- c(n.class,classify(w))
    cat(i,"\n")


}
classified.a <- as.numeric(table(n.class))

n.class <- c()
sample.id <- sample(id.b,500)
for (i in sample.id) {

    w <- strsplit(docs[i], " ")[[1]]
    n.class <- c(n.class,classify(w))
    cat(i,"\n")


}
classified.b <- as.numeric(table(n.class))

n.class <- c()
sample.id <- sample(id.c,500)
for (i in sample.id) {

    w <- strsplit(docs[i], " ")[[1]]
    n.class <- c(n.class,classify(w))
    cat(i,"\n")


}
classified.c <- as.numeric(table(n.class))

n.class <- c()
sample.id <- sample(id.d,500)
for (i in sample.id) {

    w <- strsplit(docs[i], " ")[[1]]
    n.class <- c(n.class,classify(w))
    cat(i,"\n")


}
classified.d <- as.numeric(table(n.class))


n.class <- c()
sample.id <- sample(id.e,500)
for (i in sample.id) {

    w <- strsplit(docs[i], " ")[[1]]
    n.class <- c(n.class,classify(w))
    cat(i,"\n")


}
classified.e <- as.numeric(table(n.class))



confusion.matrix <- rbind(classified.a,classified.b,classified.c,classified.d,classified.e)
rownames(confusion.matrix) <- NULL

#length(which(n.class == 5))


accuracy <- sum(diag(confusion.matrix))/sum(confusion.matrix)

recall <- unlist(lapply(c(1:5), function(i) confusion.matrix[i,i]/sum(confusion.matrix[i,])))

precision <- unlist(lapply(c(1:5), function(i) confusion.matrix[i,i]/sum(confusion.matrix[,i])))




