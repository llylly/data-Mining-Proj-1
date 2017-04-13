library(XML)
library(stringr)

readDoc <- function(path) {
	categories <- character()
	locations <- character()
	day_of_month <- NA
	month <- NA
	year <- NA
	
	# parse XML
	rootNode <- xmlRoot(xmlParse(path))
	headNode <- rootNode[["head"]]
		titleNode <- headNode[["title"]]
		docdataNode <- headNode[["docdata"]]
			docidNode <- docdataNode[["doc-id"]]
				docidAttr <- xmlGetAttr(docidNode, "id-string")
			identifiedContentNode <- docdataNode[["identified-content"]]
				for (i in 1:length(xmlChildren(identifiedContentNode))) {
					nowNode <- identifiedContentNode[[i]]
					#if (xmlName(nowNode) == "classifier") {
					#	if (xmlGetAttr(nowNode, "type") == "descriptor")
					#		categories[length(categories) + 1] <- xmlValue(nowNode)
					#}
					if (xmlName(nowNode) == "location") 
						locations[length(locations) + 1] <- xmlValue(nowNode)
				}
		for (i in 1:length(xmlChildren(headNode))) {
			nowNode <- headNode[[i]]
			if (xmlName(nowNode) == "meta") {
				if (xmlGetAttr(nowNode, "name") == "publication_day_of_month")
					day_of_month <- as.numeric(xmlGetAttr(nowNode, "content"))
				if (xmlGetAttr(nowNode, "name") == "publication_month")
					month <- as.numeric(xmlGetAttr(nowNode, "content"))
				if (xmlGetAttr(nowNode, "name") == "publication_year")
					year <- as.numeric(xmlGetAttr(nowNode, "content"))
				if (xmlGetAttr(nowNode, "name") == "online_sections")
					categories[length(categories) + 1] <- as.character(xmlGetAttr(nowNode, "content"))
			}
		}
		pubdataNode <- headNode[["pubdata"]]
		correction_data <- xmlGetAttr(pubdataNode, "date.publication")
	bodyNode <- rootNode[["body"]]
		body.contentNode <- bodyNode[["body.content"]]
		content <- ""
			if (length(xmlChildren(body.contentNode)) > 0)
			for (i in 1:length(xmlChildren(body.contentNode))) {
				nowNode <- body.contentNode[[i]]
				if (!is.null(nowNode)) {
					if (xmlGetAttr(nowNode, "class") == "full_text") {
						for (i in xmlChildren(nowNode)) {
							if (!grepl("LEAD:", xmlValue(i)))
								content <- paste(content, xmlValue(i), sep=" \n ")
						}
					}
				}
			}
	
	# extract info from nodes
	title <- xmlValue(titleNode)
	docid <- as.numeric(docidAttr)
	
	# normalize categories & locations like "|a|b|c", allow duplicate
	categories <- tolower(categories)
	categories <- gsub("[(]", ", ", categories)
	categories <- gsub("[)]", ", ", categories)
	categories <- strsplit(categories, ",|and|; ")
	categoriesStr <- ""
	for (i in categories)
		for (j in i)
			categoriesStr <- paste(categoriesStr, str_trim(j), sep="|")
	locationsStr <- ""
	for (i in locations)
		locationsStr <- paste(locationsStr, i, sep="|")
	
	# may not exist, set NA instead
	if (categoriesStr == "") categoriesStr = NA
	if (locationsStr == "") locationsStr = NA
	
	# return data.frame
	ans <- data.frame(title=title, docid=docid, categories=categoriesStr, 
		day_of_month=day_of_month, month=month, year=year, correction_data=correction_data, locations= locationsStr,
		content=content)
	return(ans)
}

readAll <- function(dirPath) {
	first = TRUE
	data <- data.frame()
	fileList <- list.files(path=dirPath, pattern="*.xml")
	i <- 0
	for (file in fileList) {
		i <- i + 1
		nowData <- readDoc(paste(dirPath, file, sep=""))
		print(paste(as.character(i), paste(dirPath, file, sep="")))
		if (first) {
			first = FALSE
			data <- as.data.frame(nowData)
		} else 
			# concatenate
			data <- rbind(data, nowData)
	}
	return(data)
}

extractAll <- function(dirPath, saveFile) {
	data <- readAll(dirPath)
	
	# save to csc format
	write.csv(data, file=saveFile)
}