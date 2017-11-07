# load the libraries needed
install.packages("flexclust")
library(flexclust)
library(caTools)

# download and read data
fileUrl <- "https://gist.githubusercontent.com/gaoxx643/c3f3cd60af04f940ee2111f4e197f63e/raw/492e54299e4fe882bc5834dc056e118f8e9df5e2/vanguard.tsv"
download.file(fileUrl, destfile = "vanguard-etf.tsv", method = "curl")
data <- read.csv2("vanguard-etf.tsv", header = TRUE, sep = '\t', stringsAsFactors = F)

# inspect the data
head(data)
str(data)

# convert the percentage string columns into numeric values
data[,3] <- as.numeric(sub("%", "", data[,3]))
data[,4] <- as.numeric(sub('$', '', as.character(data[,4]), fixed = TRUE))
data[,5] <- as.numeric(sub('$', '', as.character(data[,5]), fixed = TRUE))
data[,6] <- as.numeric(sub("%", "", data[,6]))
data[,7] <- as.numeric(sub("%", "", data[,7]))
data[,8] <- as.numeric(sub("%", "", data[,8]))
data[,9] <- as.numeric(sub("%", "", data[,9]))
data[,10] <- as.numeric(sub("%", "", data[,10]))
data[,11] <- as.numeric(sub("%", "", data[,11]))

# replace all NA with 0
data[is.na(data)] <- 0

# set a random seed for reproducibility 
set.seed(123)

# split the data to a train and test set
newdata <- sample.split(data$Ticker, SplitRatio = 0.75)
train <- subset(data, sample = TRUE)
test <- subset(data, sample = FALSE)

# run k-means clustering on the data
fit <- kmeans(train[, 8:11], 5, nstart = 20)
fit$cluster
train$group <- fit$cluster

# sort the funds by their assigned group
train[order(train$group), ]

# lable the clusters with names -- genalize the commonalities of clustering results
centroids <- as.data.frame(fit$cluster)
centroids$group <- 1:nrow(centroids)
centroids$lable <- c('International', 'StockBigGain', 'Stock', 'Bond', 'SmallMidLargeCap')
centroids

# set assigned lable on training data
train$label <- centroids$lable[train$group]

# export the fike t a csv file
write.csv(train[order(train$group), ], file = 'clustered.csv')

# predict on new data
fit2 <- as.kcca(fit, data = train[, 8:11])

# predict the assigned color by mapping the color to a cluster
test$group <- predict(fit2, newdata = test[, 8:11])

# Assign the label of the clusters
test$label <- sapply(1:nrow(test), function(row) {
        centroids[centroids$group == test[row, 'group'], ]$label
})

# export the result to a csv file
write.csv(test[order(test$group), ], file = 'clustered2.csv')




