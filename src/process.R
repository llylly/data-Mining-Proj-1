library(tm)
library(NLP)
library(ggplot2)
# It reads the content set and constructs a Corpus object for it
# @paremeter docData[["content"]]
# @return docCorpus(type: Corpus)
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

# Draw word cloud of words which appears at least "minLimit" times
# @paramter docCorpus(type: Corpus)
# @parameter path
# @parameter numeric
# @return wordFrequencyMap(type: data.frame)
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

# Calculate word length from Corpus
# @parameter docCorpus(type: Corpus)
# @parameter path
# @parameter numeric
# @return wordLengthSamples(type: numeric)
# draw: qplot(len, bins=max(len)-min(len)+1)
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
	qplot(len, bins=max(len)-min(len)+1)
	if (toPdf != 0) dev.off()
	return(len)
}

# Calculate the frequency of each category
# @parameter docData(type: data.frame)
# @paramter path
# @return categoryFrequencyList(type: data.framem[categories, freq])
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
	df=data.frame(categories = freqList$categories, freq = freqList$freq)
#	if (toPdf != 0) {
#		pdf(file = toPdf)
#	} else if (length(dev.list()) == 0) {
#		x11()
#	}
	pic = ggplot(data = df, mapping = aes(x = categories, y = freq)) + geom_bar(stat= 'identity')+ theme(axis.text.x = element_text( angle = 45))
	if (toPdf != 0) ggsave(pic, file=toPdf)
#	qplot(freqList$categories, freqList$freq, xlab="Categories", ylab="Frequency")
#	if (toPdf != 0) dev.off()
	return(df)
}

# Calculate the publication time samples of documents
# @parameter docData(type: data.frame)
# @paramter path
# @return timeSamples(type: numeric)
# timeSamples is in month units and relatives to the minimum month
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
#	set <- numeric()
	first = ans$time[1]%/% 100 * 12 + ans$time[1] %% 100
	for (i in 1:dim(ans)[1]) {
#		set <- append(set, rep(ans$time[i] %/% 100 * 12 + ans$time[i] %% 100, times=ans$freq[i]))
		ans$time[i] = ans$time[i] %/% 100 * 12 + ans$time[i] %% 100 - first
	}
#	print(min(set))
#	set <- set - min(set)
#	if (toPdf != 0) {
#		pdf(file = toPdf)
#	} else if (length(dev.list()) == 0) {
#		x11()
#	}
#	pic = ggplot(ans, geom="histogram", xlab="Month", ylab="Frequency")
	pic = ggplot(data=ans, mapping = aes(x=time, y=freq))+ geom_bar(stat= 'identity') + xlab("month")
	if (toPdf != 0) ggsave(pic, file=toPdf)
#	if (toPdf != 0) dev.off()
	return(ans)
}

# Calculate the publication time samples of documents
# @parameter docData(type: data.frame)
# @paramter path
# @return timeSamples(type: numeric)
# timeSamples is in month units and relatives to the minimum month
drawTimeLineYear <- function(data, toPdf = 0) {
	time <- lapply(as.character(data[["correction_data"]]), substr, 1, 4)
	time <- unique(time)
	ans <- data.frame(time=as.numeric(time), freq=rep(0, length(time)))
	dataf <- as.data.frame(data)
	for (item in data$correction_data) {
		nowtime <- as.numeric(substr(item, 1, 4))
		ans$freq[which(ans$time==nowtime)] <-
			ans$freq[which(ans$time==nowtime)] + 1
	}
#	set <- numeric()
	first = ans$time[1]%/% 100 
	for (i in 1:dim(ans)[1]) {
#		set <- append(set, rep(ans$time[i] %/% 100 * 12 + ans$time[i] %% 100, times=ans$freq[i]))
		#ans$time[i] = ans$time[i] %/% 100 - first
	}
#	print(min(set))
#	set <- set - min(set)
#	if (toPdf != 0) {
#		pdf(file = toPdf)
#	} else if (length(dev.list()) == 0) {
#		x11()
#	}
#	pic = ggplot(ans, geom="histogram", xlab="Month", ylab="Frequency")
	pic = ggplot(data=ans, mapping = aes(x=time, y=freq))+ geom_bar(stat= 'identity') + xlab("year")
	if (toPdf != 0) ggsave(pic, file=toPdf)
#	if (toPdf != 0) dev.off()
	return(ans)
}


# Calculate word matrix
# @parameter docCorpus(type: Corpus)
# @return wordMatrix(type: matrix)
getWordMatrix <- function(corpus) {
	return(as.matrix(DocumentTermMatrix(corpus)))
}

# Calculate document-category map from docData
# @parameter docData(type: data.frame)
# @paramter documentCategoryMap(type: data.frame[id, category])
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

# Calculate similarityMatrix from wordMatrix (getWordMatrix)
# @parameter wordMatrix(type: matrix)
# @return similarityMatrix(type: matrix)
# REQUIRES A LOT OF TIME!!!
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
	for (i in 1:n)
		for (j in 1:n)
			if (is.nan(matrix[i,j]))
				matrix[i,j] <- 0
	return(ans)
}

# Calculate Euclid length of a vector
# @paramter vector
# @return numeric
dis <- function(vec) {
	ans <- 0
	for (x in vec) ans <- ans + x * x
	ans <- sqrt(ans)
	return(ans)
}

# Calculate the average similarity for category pair
# @parameter docData(type: data.frame)
# @parameter similarityMatrix(type: matrix)
# @return relativityMatrix(type: matrix)
getCrossDistance <- function(data, matrix) {
	categoryMap <- getDocumentCategoryMap(data)
	categoryArr <- unique(categoryMap$category)
	n <- length(unique(categoryArr))
	ansMat <- matrix(nrow = n, ncol = n)
	for (i in 1:n)
		for (j in 1:n) {
			cate1 <- categoryArr[i]
			cate2 <- categoryArr[j]
			now1 <- categoryMap$id[which(categoryMap$category==cate1)]
			now2 <- categoryMap$id[which(categoryMap$category==cate2)]
			cnt <- 0
			ans <- 0
			for (ii in now1)
				for (jj in now2) 
					if ((i != j) || (ii != jj))  {
						ans <- ans + matrix[ii,jj]
						cnt <- cnt + 1
					}
			if (cnt == 0) {
				cnt <- 1
				ans <- 1
			}
			ansMat[i,j] <- ans / cnt
		}
	return(data.frame(category = categoryArr, ansMat))
}

# Query the relativity between two categories
# @parameter relativityMatrix(type: matrix)
# @parameter character(name of category 1)
# @parameter character(name of category 2)
# @return numeric(relativity value for legal category names, and -1 for illegal category names)
queryDistance <- function(crossMat, type1, type2) {
	c1 <- which(crossMat$category == type1)
	c2 <- which(crossMat$category == type2)
	crossMat <- crossMat[,(dim(crossMat)[2] - dim(crossMat)[1] + 1):(dim(crossMat)[2])]
	if (length(c1) * length(c2) > 0) {
		return(crossMat[c1,c2 + 1])
	} else {
		return(-1)
	}
}

drawInnerDistance <- function(csv_data, toPdf){
	data = read.csv(csv_data)
	df = data.frame(categories=data$category,avg_dis=data$dis)
	df = df[order(df$avg_dis,decreasing=TRUE),]
	df$categories = factor(df$categories,levels=df$categories)
	pic = ggplot(data = df, mapping = aes(x = categories, y = avg_dis)) + geom_bar(stat= 'identity')+ theme(axis.text.x = element_text( angle = 45))
	if (toPdf != 0) ggsave(pic, file=toPdf)
}

drawSVDrepr <- function(corpus, doc2Pdf, word2Pdf, word_cnt){
	mat <- DocumentTermMatrix(corpus,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
	res = svd(mat)
	# 文档语义相关矩阵
	datau <- data.frame(res$u[,1:2])
	# 词项语义相关矩阵
	datav <- data.frame(res$v[,1:2])
	
	l = length(mat$dimnames$Terms)
	inte = round(l/word_cnt)
	i = 100
	s = c(0,0)
	w = c(0)
	while(i<l){
		s = rbind(s,datav[i,])
		w = rbind(w,mat$dimnames$Terms[i])
		i = i + inte
	}
	print(s)
	print(w)
	
	pic1 <- ggplot()+
	  geom_point(data=datau,aes(X1,X2))+
	  geom_text(data=datau,aes(X1,X2),
	            label=1:500,vjust=2)
	pic2 <- ggplot()+
	  geom_point(data=s,aes(X1,X2))+
	  geom_text(data=s,aes(X1,X2),
	            label=w,vjust=2)
	if (doc2Pdf != 0) ggsave(pic1, file= doc2Pdf)
	if (word2Pdf != 0) ggsave(pic2, file= word2Pdf)
}


















