library(jsonlite)

library(coreNLP)

initCoreNLP()

fn <- list.files(path="./json", full.names =T)
#t1 <- Sys.time()

i=1
for (f in fn) {

    data <- fromJSON(f)
    if (data$articleType == "article") {
        #output = annotateString(data$title$value)
        #title = paste(tolower(getToken(output)$lemma[union(grep("^N",getToken(output)$POS),grep("^J",getToken(output)$POS))]), collapse=" ")
        #title = paste(tolower(getToken(output)$lemma[union(which(getToken(output)$POS=="NN"),which(getToken(output)$POS=="JJ"))]), collapse=" ")
        title <- gsub("<.*?>", "", data$title$value)
        #print(title)
        write.table(title, "aps-ti.txt", quote = FALSE, row.names = FALSE, col.names = FALSE, append=TRUE)
        write.table(data$date, "aps-date.txt", quote = FALSE, row.names = FALSE, col.names = FALSE, append=TRUE)
        write.table(data$id, "aps-doi.txt", quote = FALSE, row.names = FALSE, col.names = FALSE, append=TRUE)
    }
    cat(i,"\n")
    i=i+1
}

#t2 <- Sys.time()