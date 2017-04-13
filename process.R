library(tm)

# data[["content"]] should be the input parameter
getCorpus <- function(contentList) {
	set <- character()
	for (content in contentList) {
		set[length(set) + 1] <- as.character(content)
	}
	vsList <- VectorSource(set)
	res <- Corpus(vsList)
	res <- tm_map(res, stripWhitespace)
	res <- tm_map(res, tolower)
	res <- tm_map(res, removeNumbers)
	res <- tm_map(res, removePunctuation)
	res <- tm_map(res, removeWords, stopwords("english"))
	stem <- tm_map(res, stemDocument)
	return(stem)
}

getBagOfWords <- function(corpus) {
	ans <- list()
	mat <- DocumentTermMatrix(corpus)
	totDoc <- dim(mat)[1]
	for (i in 1:totDoc) {
		now <- list()
		nowMat <- mat[i,]
		j <- 1
		while (length(findFreqTerms(nowMat, j)) > 0) {
			words <- findFreqTerms(nowMat, j, j)
			if (length(words) > 0)
				for (word in words)
					now[word] <- j
			j <- j+1
		}
		ans[i] <- list(now)
	}
	return(ans)
}

getAllBagOfWords <- function(data) {
	contentList <- data[["content"]]
	corpus <- getCorpus(contentList)
	print(corpus)
	bag <- getBagOfWords(corpus)
	return(bag)
}

drawWordCloud <- function(corpus, toPdf = 0, minLimit = 100) {
	mat <- DocumentTermMatrix(corpus)
	j <- minLimit
	words <- character()
	times <- numeric()
	while (length(findFreqTerms(mat, j)) > 0) {
		tmp <- findFreqTerms(mat, j, j)
		for (word in tmp) {
			words <- append(words, word)
			times <- append(times, j)
		}
		j <- j+1
	}
	words <- rev(words)
	times <- rev(times)
	if (toPdf != 0) {
		pdf(file = toPdf)
	}
	wordcloud(words, times)
	if (toPdf != 0) dev.off()
	return(data.frame(word=words, freq=times))
}

drawWordLength <- function(corpus, toPdf = 0, minLimit = 0) {
	mat <- DocumentTermMatrix(corpus)
	words <- findFreqTerms(mat, minLimit)
	len <- numeric()
	for (word in words) {
		len <- append(len, nchar(word))
	}
	if (toPdf != 0) {
		pdf(file = toPdf)
	} else if (length(dev.list()) == 0) {
		x11()
	}
	qplot(sort(wordLength), bins=max(wordLength)-min(wordLength)+1)
	if (toPdf != 0) dev.off()
	return(len)
}

drawCategories <- function(data, toPdf = 0) {
	categories <- character()
	set <- lapply(as.character(data[["categories"]]), strsplit, "[|]")
	for (item in set)
	for (iitem in item) 
		for (i in iitem) {
			if ((!is.na(i)) && (nchar(i) > 0)) {
				categories <- append(categories, i)
			}
		}
	categories <- unique(categories)
	dataf <- as.data.frame(data)
	freqList <- rep(0, times=length(categories))
	freqList <- data.frame(categories = categories, freq = freqList)
	for (i in 1:dim(dataf)[1]) {
		splits <- strsplit(as.character(dataf[[i,"categories"]]), "[|]")
		for (splits.s in splits) 
		for (str in splits.s){
			if ((!is.na(str)) && (nchar(str) > 0))
				freqList$freq[which(freqList$categories==str)] <- 
					freqList$freq[which(freqList$categories==str)] + 1
		}
	}
	freqList <- freqList[sort(freqList $freq, decreasing=TRUE, index.return=TRUE)$ix,]
	freqList$categories <- ordered(freqList$categories, levels=as.character(freqList$categories))
	if (toPdf != 0) {
		pdf(file = toPdf)
	} else if (length(dev.list()) == 0) {
		x11()
	}
	qplot(freqList$categories, freqList$freq, geom="h", xlab="Categories", ylab="Frequency")
	if (toPdf != 0) dev.off()
	return(freqList)
}

drawTimeLine <- function(data, toPdf = 0) {
	time <- lapply(as.character(data[["correction_data"]]), substr, 1, 6)
	time <- unique(time)
	ans <- data.frame(time=as.numeric(time), freq=rep(0, length(time)))
	dataf <- as.data.frame(data)
	for (item in data$correction_data) {
		nowtime <- as.numeric(substr(item, 1, 6))
		ans$freq[which(ans$time==nowtime)] <-
			ans$freq[which(ans$time==nowtime)] + 1
	}
	set <- numeric()
	for (i in 1:dim(ans)[1]) {
		set <- append(set, rep(ans$time[i] %/% 100 * 12 + ans$time[i] %% 100, times=ans$freq[i]))
	}
	set <- set - min(set)
	if (toPdf != 0) {
		pdf(file = toPdf)
	} else if (length(dev.list()) == 0) {
		x11()
	}
	qplot(ans, geom="histogram", xlab="Month", ylab="Frequency", bins=max(ans)-min(ans)+1)
	if (toPdf != 0) dev.off()
	return(set)
}

getWordMatrix <- function(bag) {
	names <- character()
	for (row in bag) {
		names <- append(names, names(row))
	}
	names <- unique(names)
	ans <- matrix(rep(0, length(bag)*length(names)), nrow = length(bag), ncol = length(names))
	i <- 0
	for (row in bag) {
		i <- i+1
		for (word in names(bag[[i]])) 
			ans[i, which(names==word)] <- row[[word]]
	}
	return(ans)
}

getDocumentCategoryMap <- function(data) {
	docIds <- numeric()
	categories <- character()
	for (i in 1:dim(data)[1]) {
		nowArr <- strsplit(as.character(data[["categories"]][[i]]), "[|]")
		#print(nowArr)
		for (j in nowArr)
		for (cate in j) {
			if ((!is.na(cate)) && (nchar(cate) > 0)) {
				docIds <- append(docIds, i)
				categories <- append(categories, cate)
			}
		}
	}
	return(data.frame(id=docIds, category=categories))
}

getSimilarityMat <- function(matrix) {
	n <- dim(matrix)[1]
	ans <- matrix(rep(0, n*n), n, n)
	for (i in 1:(n-1))
		for (j in (i+1):n) {
			print(i)
			print(j)
			ans[i,j] <- sum(matrix[i,] * matrix[j,]) / (dis(matrix[i,]) * dis(matrix[j,]))
			print(ans[i,j])
		}
	for (i in 1:n) ans[i,i] <- 1
	for (i in 2:n)
		for (j in 1:(i-1)) 
			ans[i,j] <- ans[j,i]
	return(ans)
}

dis <- function(vec) {
	ans <- 0
	for (x in vec) ans <- ans + x * x
	ans <- sqrt(ans)
	return(ans)
}