library(scales)

source("preprocess.R")

Go <- function() {
  voynichPath <- "manuscript/eva/voynich.txt"
  dictDir <- "dict/winedt"

  manuscript <<- PrepareVoynichManuscript(voynichPath)
  voynichDict <<- CreateVoynichDict(manuscript)

  dicts <<- list( )
  dicts[["voynich"]] <<- voynichDict
  dictInfo <- PrepareDictInfo()

  # load all dicts
  for (i in 1:nrow(dictInfo)) {
    row <- dictInfo[i,]
    dicts[[row$language]] <<- LoadLanguageDict(row$path, row$encoding)
  }
  ncharDicts <<- sapply(dicts, nchar)
  maxNChars <<- max(sapply(ncharDicts, max))
}

CalculateDictMeans <- function(hDict) {
  # calculate dict means
  means <- sapply(hDict, mean)
  # order means ascending
  means[order(means)]
}

DrawBoxplots <- function() {
  # order boxplot by mean
  dictMeans <- CalculateDictMeans(ncharDicts)
  # generate colors
  colors <- rainbow(length(dictMeans))

  for (i in 1:length(dictMeans)) {
    langName <- names(dictMeans[i])
    dict <- ncharDicts[[langName]]

    if (i == 1) {
      boxplot(dict, at = 1, xlim = c(1, length(dictMeans)),
              ylim = c(0, maxNChars), col = colors[i],
              main = "Word length for languages",
              xlab = "Language",
              ylab = "Word length")
    } else {
      boxplot(dict, at = i, add = TRUE, col = colors[i])
    }
  }
  axis(1, at=1:length(dictMeans), labels=names(dictMeans), cex.axis = 0.65)
  minor.tick(ny=10, tick.ratio=0.5)
  #legend(15.5, 35, names(dictMeans), fill = colors, cex = 0.75, horiz = FALSE, y.intersp = 0.6)
}

DrawHists <- function() {
  par(mfrow = c(3, 5))
  dictMeans <- CalculateDictMeans(ncharDicts)
  # generate colors
  colors <- rainbow(length(dictMeans))

  for (i in 1:length(dictMeans)) {
    dictName <- names(dictMeans[i])
    dict <- ncharDicts[[dictName]]
    h <- hist(dict, main = dictName, breaks=length(unique(dict)),
              xlim = c(0, 27), col ="red", xlab = "Word length")
    DrawDensity(h, dict, "blue")
  }
}

DrawDensity <- function(hist, data, color) {
  xfit <- seq(min(data), max(data), length = 40)
  yfit <- dnorm(xfit, mean = mean(data), sd = sd(data))
  yfit <- yfit * diff(hist$mids[1:2]) * length(data)
  lines(xfit, yfit, col = color, lwd = 2, lty = 1)
}

PrepareDictInfo <- function() {
  language <- c("catalan",    "croatian",   "czech",   "danish",
                "english_uk", "french",     "german",
                "italian",    "latin",      "polish",  "portuguese",
                "romanian",   "spanish",    "swedish")
  encoding <- c("latin1",   "cp1250",   "cp1250",   "cp1250",
                "cp1250",   "cp1250",   "cp1250",
                "cp1250",   "latin1",   "UTF-16LE", "UTF-16LE",
                "UTF-16LE", "UTF-16LE", "UTF-16LE")
  path <- c("dict/winedt/catalan.dic",  "dict/winedt/croatian.dic",
            "dict/winedt/czech.dic",    "dict/winedt/danish.dic",
            "dict/winedt/english_uk.dic",
            "dict/winedt/french.dic",   "dict/winedt/german.dic",
            "dict/winedt/italian.dic",  "dict/winedt/latin.dic",
            "dict/winedt/polish.dic",   "dict/winedt/portuguese.dic",
            "dict/winedt/romanian.dic",
            "dict/winedt/spanish.dic",  "dict/winedt/swedish.dic")
  data.frame(language, encoding, path, stringsAsFactors = FALSE)
}
