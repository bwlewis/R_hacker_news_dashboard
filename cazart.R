message("Interactive stand-alone experiments for a Hacker News dashboard\n
# Wordcloud of the most recent new stories
# Rate of new story postings
# Positive/negative main comment sentiment of top stories
# Overall comment mood of top stories
# Topic clustering of top and new stories

# Try this code out with, for example:

source('cazart.R')
load('example.rdata')
titlecloud(new)
clust(c(new, top))

# These are just some experiments. See the shiny app for a full implementation.")

# number of top and new stories to download. Small N is faster but much
# less interesting :(.
N <- 100

# Startup: load N new and top stories, add a progress bar?  The HN API can be
# very slow sometimes. However once loaded, we can just update the list adding
# new stories and ejecting old ones. Should be faster once in that phase...
#new <- Map(function(x) item(x), newstories()[1:N])
#top <- Map(function(x) item(x), topstories()[1:N])
load("example.rdata")


library(jsonlite)
library(wordcloud)
library(tm)
library(Matrix)
library(irlba)
library(deldir)
load("words.rdata")

# Basic Hacker News API functions
# Get the ID of the newest post (all posts, comments, stories, etc.)
latest <- function() 
  tryCatch(fromJSON("https://hacker-news.firebaseio.com/v0/maxitem.json"), error=function(e) Inf)
# Get a list of most recent or top-rated news story IDs (stories only)
newstories <- function() 
  tryCatch(fromJSON("https://hacker-news.firebaseio.com/v0/newstories.json"), error=function(e) c())
topstories <- function() 
  tryCatch(fromJSON("https://hacker-news.firebaseio.com/v0/topstories.json"), error=function(e) c())

# a utility function
null2c <- function(x) ifelse(is.null(x), "", x)

# Process the character value x to remove numbers, punctuation, repeated spaces and stopwords.
clean <- function(x)
{
  x <- unlist(strsplit(tolower(gsub("[[:punct:][:digit:]]", "", x)), " "))
  gsub("^ *|(?<= ) | *$", "", Reduce(paste, x[is.na(match(x, stopwords("SMART")))]), perl=TRUE)
}

# Retrieve an item by ID and lightly process the title to remove
# numbers, punctuation and stopwords.
item <- function (id)
{
  if(is.null(id)) return(NULL)
  url <- sprintf("https://hacker-news.firebaseio.com/v0/item/%.0f.json", id)
  ans <- tryCatch(fromJSON(url), error=function(e) list())
  ans$title <- clean(ans$title)
  s <- sentiment(ans)
  ans$sentiment <- s$score
  ans$comments <- s$comments
  ans
}

# collect main comments for a story x and return a really basic sentiment score.
# Follows the simple approach outlined in:
# https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon
# and
# http://www.slideshare.net/jeffreybreen/r-by-example-mining-twitter-for/12-Load_sentiment_word_lists1_Download
# NOTE: kind of slow!! Thus the limiting to at most top 10 comments.
sentiment <- function(x)
{
  if(is.null(x$kids)) return(list(score=0, comments=""))
  kids <- head(x$kids, 10)
  comments <- clean(Reduce(paste, Map(function(y) {z <- item(y); paste(z$text, z$title)}, kids)))
  v <- unlist(strsplit(comments, " "))
  score <- sum(match(v, words$positive, 0) > 0) - sum(match(v, words$negative, 0) > 0)
  list(score=score, comments=comments)
}


# prepare a wordcloud from the titles in the list of stories in x
titlecloud <- function(stories)
{
  # produce a vector of words in the story titles, removing punctuation, digits,
  # and stop words.
  title_words <- unlist(
    Map(function(x) 
      {
        unlist(strsplit(clean(x$title), " "))
      } , stories))
  # collect their counts by word and plot the word cloud
  counts <- as.data.frame(table(title_words))
  wordcloud(counts$title_words, counts$Freq,
            scale=c(4,.05), min.freq=1, max.words=100, colors=brewer.pal(9, "BuGn"))
}

# Compute the rate of stories per second from the time data in the
# supplied list of stories
rate <- function(x)
{
  length(x) / diff(Reduce(range, Map(function(x) x$time, new)))
}

# plot clusters of story title concepts from the supplied news item list
# "latent semantic analysis lite"
# Returns the kmeans cluster output
clust <- function(x, centers=3)
{
  titles <- Map(function(y) y$title, x)
  y <- DocumentTermMatrix(Corpus(VectorSource(titles)))
  y <- sparseMatrix(i=y$i, j=y$j, x=y$v)  # convert to a numeric sparse matrix
  s <- irlba(y, nu=3, nv=3, center=colMeans(y)) # principal components
  # pick the best 2d clustering we get from the three dimensions
  k <- list(kmeans(s$u[, 1:2], centers=centers, nstart=100))
  k <- c(k, list(kmeans(s$u[, c(1,3)], centers=3, nstart=100)))
  d <- which.max(unlist(Map(function(x) x$betweenss / x$totss, k)))[1]
  k <- k[[d]]
   
  # establish dominant small phrases or words in each cluster
  titles <- unlist(titles)
  topics <- Map(function(i)
  {
    t <- table(unlist(
          Map(function(x) paste(x, collapse=" "),
            ngrams(Reduce(c, Map(function(y) unlist(strsplit(y, " ")), titles[which(k$cluster == i)])), 2))))
    names(t)[t == max(t)][[1]]
  }, seq(1, centers))
  # Flag a really big cluster
  p <- k$size / length(k$cluster)
  if(max(p) > 0.66) topics[which.max(p)] <- sprintf("%s\nand other stuff", topics[which.max(p)])

  p <- par(mar=c(0, 0, 0, 0))
  # colors for negative, neutral, positive sentiment
  # Map the sentiment values into the integer interval [9,89] to index colors,
  # (smaller values = more negative sentiment).
  idx <- floor(unlist(Map(function(y) 1 / (1 + exp(-y$sentiment)), x)) * 80) + 9
  col <- sprintf("%sAA", colorRampPalette(brewer.pal(10, "RdBu"), space="Lab")(100))[idx]
  k$xlim <- 1.2 * range(s$u[, 1])
  k$ylim <- 1.2 * range(s$u[, d + 1])
  plot(s$u[, 1], s$u[, d + 1], col=0, pch=1, xaxt="n", yaxt="n", xlab="", ylab="", xlim=k$xlim, ylim=k$ylim)
  voroni(k)
  points(s$u[, 1], s$u[, d + 1], col=col, cex=4, pch=19) # fill
  points(s$u[, 1], s$u[, d + 1], pch=1, cex=4, col="#00000032") # stroke
  text(k$centers[,1], k$centers[,2], labels=topics, cex=3, col="#000000DD")
# manually plot Voronoi tesselation using v[[2]] coordinates, also shrink text coordinates
# to the Voronoi center...xxx
  par(p)
  k
}


# A custom Voronoi plot based on the deldir package
voroni <- function(k)
{
  v <- deldir(k$centers[,1], k$centers[, 2])[[2]]
  m <- (v$y2 - v$y1) / (v$x2 - v$x1)     # slopes
  b <- v$y1 - m * v$x1                   # intercepts
  x <- c(v$x1, v$x2)
  xcenter <- x[which.max(tabulate(match(x, unique(x))))]
  V <- as.matrix(v[, 1:4])
# Extend the deldir line segment coordinates beyond the plot edge
  i <- (V[,1] == xcenter) * 2 + 1
  j <- (V[,1] != xcenter) * 2 + 1
  lim <- apply(cbind(abs(V[matrix(c(1:3, i), nrow=3)] - min(k$xlim)), abs(V[matrix(c(1:3, i), nrow=3)] - max(k$xlim))), 1, which.min)
  V[matrix(c(1:3, i), nrow=3)] <- 10 * k$xlim[lim]
  V[matrix(c(1:3, i + 1), nrow=3)] <- m * 10 * k$xlim[lim] + b
# Fill polygons
  col <- rainbow(nrow(V), alpha=0.1)
  for(m in 1:nrow(V))
  {
    n <- m + 1
    if(n > nrow(V)) n <- 1
    idx <- c(j[m], i[m], i[n], j[n])
    x <- V[matrix(c(rep(m, 2), rep(n, 2), idx), 4)]
    y <- V[matrix(c(rep(m, 2), rep(n, 2), idx + 1), 4)]
    polygon(x, y, col=col[m], lty=2)
  }
}
