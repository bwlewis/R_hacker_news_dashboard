library(shiny)
library(dygraphs)
library(jsonlite)
library(tm)
library(Matrix)
library(irlba)
library(deldir)

# words: list of positive/negatve words from https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
# example: ready-to go populated HN stories example for testing
# can either use local repository files or the indicated url
load(url("http://illposed.net/words.rdata"))
load(url("http://illposed.net/example.rdata"))

# Basic Hacker News API functions
# Get the ID of the newest post (all posts, comments, stories, etc.)
latest <- function() 
  tryCatch(fromJSON("https://hacker-news.firebaseio.com/v0/maxitem.json"), error=function(e) Inf)
topstories <- function() 
  tryCatch(fromJSON("https://hacker-news.firebaseio.com/v0/topstories.json"), error=function(e) c())

# number of top and new stories to download. Small N is faster but
# less interesting :( Warning! The HN API can bog down sometimes.
N <- 100
# Because the API is a bit slow, we limit the number of top comments used
# to compute sentiment
COMMENT_LIMIT <- 20
# Initialize
# Sentiment scale
mood <- c("aggro", "cheesed off", "cranky", "chill", "amped", "stoked", "woot")
state <- reactiveValues(stories=top, # loaded from example.rdata to get started
                        mood=4,
                        mood_raw=0,
                        latest=latest(),
                        rate=NA,
                        rate_history=data.frame(dt=0, rate=0),
                        xy=data.frame(x=0, y=0),
                        time=Sys.time())

# Support functions follow...
# Process the character value x to remove numbers, punctuation, repeated spaces and stopwords.
clean <- function(x)
{
  x <- unlist(strsplit(tolower(gsub("[[:punct:][:digit:]]", "", x)), " "))
  gsub("^ *|(?<= ) | *$", "", Reduce(paste, x[is.na(match(x, stopwords("SMART")))]), perl=TRUE)
}

# Retrieve an item by ID and lightly process the title to remove
# numbers, punctuation and stopwords.
item <- function (id, recursive=TRUE)
{
  if(is.null(id)) return(NULL)
  url <- sprintf("https://hacker-news.firebaseio.com/v0/item/%.0f.json", id)
  ans <- tryCatch(fromJSON(url), error=function(e) list())
  if(is.null(ans$url)) ans$url <- "https://news.ycombinator.com/"
  if(is.null(ans$title)) ans$title <- "no title"
  ans$raw_title <- ans$title
  ans$title <- clean(ans$title)
  s <- list(score=0, comments="")
  if(recursive) s <- sentiment(ans)
  ans$sentiment <- s$score
  ans$comments <- s$comments
  ans
}

# collect main comments for a story x and return a really basic sentiment score.
# Follows the simple approach outlined in:
# https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon
# and
# http://www.slideshare.net/jeffreybreen/r-by-example-mining-twitter-for/12-Load_sentiment_word_lists1_Download
sentiment <- function(x)
{
  if(is.null(x$kids)) return(list(score=0, comments=""))
  kids <- head(x$kids, COMMENT_LIMIT)
  comments <- clean(Reduce(paste, Map(function(y) {z <- item(y, FALSE); paste(z$text, z$title)}, kids)))
  v <- unlist(strsplit(comments, " "))
  if(length(v) == 0) return(list(score=0, comments=comments))
  score <- sum(match(v, words$positive, 0) > 0) - sum(match(v, words$negative, 0) > 0)
  if(is.null(score)) score <- 0
  if(is.na(score)) score <- 0
  list(score=score, comments=comments)
}

# plot clusters of story title concepts from the supplied news item list
# "latent semantic analysis lite"
clust <- function(x, centers=3)
{
  titles <- Map(function(y) y$title, x)
  y <- DocumentTermMatrix(Corpus(VectorSource(titles)))
  y <- sparseMatrix(i=y$i, j=y$j, x=y$v)  # convert to a numeric sparse matrix
  s <- irlba(y, nu=3, nv=3, center=Matrix::colMeans(y)) # principal components
  # pick a good 2d clustering out of the three dimensions
  k <- list(kmeans(s$u[, 1:2], centers=centers, nstart=500))
  k <- c(k, list(kmeans(s$u[, c(1,3)], centers=3, nstart=100)))
  d <- which.max(unlist(Map(function(x) x$betweenss / x$totss, k)))[1]
  k <- k[[d]]
   
  # establish dominant small phrases or words in each cluster
  titles <- unlist(titles)
  topics <- Map(function(i)
  {
    t <- table(unlist(
          Map(function(x) paste(x, collapse=" "),
            ngrams(Reduce(c, Map(function(y) unlist(strsplit(y, " ")),
              titles[which(k$cluster == i)])), 2))))
    names(t)[t == max(t)][[1]]
  }, seq(1, centers))
  # Flag a really big cluster
  p <- k$size / length(k$cluster)
  if(max(p) > 0.66) topics[which.max(p)] <- sprintf("%s\nand other stuff", topics[which.max(p)])

  p <- par(mar=c(0, 0, 0, 0))
  # Sentiment colors: Map the (unbounded) sentiment values into the integer
  # interval [1,10] to index colors
  snmt <- unlist(Map(function(y) y$sentiment, x))
  idx <- floor((1 / (1 + exp(- snmt))) * 9) + 1
  col <- sprintf("%s95",
                 colorRampPalette(c("red", "gray", "blue"), alpha=FALSE)(10))[idx]
  k$xlim <- 1.2 * range(s$u[, 1])
  k$ylim <- 1.2 * range(s$u[, d + 1])
  v <- deldir(k$centers[,1], k$centers[, 2], rw=2 * c(k$xlim, k$ylim))
  tilecol <- rainbow(length(k$size), alpha=0.1)
  plot(s$u[, 1], s$u[, d + 1], col=0, pch=1,
       xaxt="n", yaxt="n", xlab="", ylab="", xlim=k$xlim, ylim=k$ylim)
  plot(tile.list(v), fillcol=tilecol, showpoints=FALSE, asp=NA, add=TRUE)
  isolate({state$xy <- data.frame(x=s$u[, 1], y=s$u[, d + 1])})
  points(s$u[, 1], s$u[, d + 1], col=col, cex=4, pch=19) # fill
  points(s$u[, 1], s$u[, d + 1], pch=1, cex=4, col="#00000055") # stroke
  text(k$centers[,1], k$centers[,2], labels=topics, cex=2.5)
  par(p)
}


ui <- pageWithSidebar(
        headerPanel("Hacker News Top Stories Topic Clusters and Sentiment"),
        sidebarPanel(
          uiOutput("ui_rate"),
          uiOutput("ui_mood"),
          dygraphOutput("dygraph"),
          sliderInput("nclust", "Number of clusters:", min=3, max=7, value=5),
          uiOutput("links"),
          uiOutput("info")
        ),
        mainPanel(
          plotOutput("plot1", height=800, click="plot_click"),
          h3("Points are color-coded by sentiment from red (negative) to grayish (neutral) to blue (positive). Click on points to see article titles and links. Data refresh gradually about every 30s.")
        ))

server <- function(input, output)
{
  obs <- observe({
    isolate({
      # update the current posting rate
      time <- Sys.time()
      id   <- latest()
      dt   <- as.numeric(difftime(time, state$time, units="secs"))
      # wait a while to begin the (sometimes slow) update process
      update <- nrow(state$rate_history) > 1
      state$rate <- 60 * (id - state$latest) / dt
      state$time <- time
      state$rate_history$dt <- state$rate_history$dt - dt / 60  # (in approx minutes)
      state$rate_history <- rbind(state$rate_history, data.frame(dt=0, rate=state$rate))
      if(nrow(state$rate_history) > 100) state$rate_history <- tail(state$rate_history, 100)
      # update the top news stories
      if(update)
      {
        id <- topstories()[1:N]
        # identify indices of new top stories that will replace older ones
        i <- is.na(match(id, unlist(Map(function(x) x$id, state$stories))))
        if(any(i))
        {
          # replace the old stories
           t0 <- proc.time()
           k <- 0
           withProgress({
             for(j in head(which(i), 15))  # limit updates due to slow API
             {
               state$stories[[j]] <- item(id[j])
               incProgress(1)
               k <- k + 1
             }
           }, min=0, max=sum(i), value=0, message = "Updating news stories...")
           dt <- (proc.time() - t0)[3]
           output$info <- renderUI({sprintf("Updated %d out of %d old stories in %.3f seconds.", k, sum(i), dt)})
        }
      }
      # update the current overall mood
      state$mood_raw <- Reduce(sum, Map(function(x) x$sentiment, state$stories))
      if(state$mood_raw < 0) state$mood_raw <- state$mood_raw / 10
      state$mood <- floor(6/(1 + exp(- state$mood_raw))) + 1  
    })
    invalidateLater(30000)   # update in 30 seconds or so
  })

  output$dygraph <- renderDygraph({dygraph(state$rate_history)})

  output$links <- renderUI({
    if(!is.null(input$plot_click))
    {
      i <- which.min((state$xy$x - input$plot_click$x)^2 + (state$xy$y - input$plot_click$y)^2)
      snmt <- state$stories[[i]]$sentiment
      if(is.null(snmt)) snmt <- 0
      HTML(sprintf("<h2>%s</h2><h3><a target='_blank' href='%s'>%s</a></h3><h3>Top ranked comments sentiment: %d</h3>",
                   state$stories[[i]]$raw_title,
                   state$stories[[i]]$url, state$stories[[i]]$url, snmt)
      )
    }
  })

  output$plot1 <- renderPlot({
    clust(state$stories, input$nclust)
  })

# Simulate shiny dashboard 'valueBox'
output$ui_rate <- renderUI({
  HTML(sprintf("<div style='font-weight: bold; text-align: center; color: white; background-color: blue; font-size: 32px;'>%0.3f total posts / minute</div>", state$rate))
})
output$ui_mood <- renderUI({
  HTML(sprintf("<div style='font-weight: bold; text-align: center; color: white; background-color: #AA9900; font-size: 32px;'>mood: %d (%s)</div>", state$mood_raw, mood[state$mood]))
})

}

print(shinyApp(ui, server))
