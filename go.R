source("analysis.R")

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

DrawBoxplots <- function() {
  # order boxplot by mean
  # ---------------------
  # calculate dict means
  dictMeans <- sapply(ncharDicts, mean)
  # order means ascending
  dictMeans <- dictMeans[order(dictMeans)]
  # generate colors
  colors <- rainbow(length(dictMeans))

  for (i in 1:length(dictMeans)) {
    langName <- names(dictMeans[i])
    dict <- ncharDicts[[langName]]

    if (i == 1) {
      boxplot(dict, at = 1, xlim = c(1, length(dictMeans) + 3), ylim = c(0, maxNChars), col = colors[i])
    } else {
      boxplot(dict, at = i, add = TRUE, col = colors[i])
    }
  }
  legend(15.5, 35, names(dictMeans), fill = colors, cex = 0.75, horiz = FALSE, y.intersp = 0.6)
}

PrepareDictInfo <- function() {
  language <- c("catalan",  "croatian",   "czech",   "danish",
                #"dutch",
                "english_uk", "french",  "german",
                "italian",  "latin",      "polish",  "portuguese",
                "romanian", #"russian",
                "spanish", "swedish")
  encoding <- c("latin1", "cp1250", "cp1250", "cp1250",
                #"cp1250",
                "cp1250", "cp1250", "cp1250",
                "cp1250", "latin1", "UTF-16LE", "UTF-16LE",
                "UTF-16LE", #"cp1250",
                "UTF-16LE", "UTF-16LE")
  path <- c("dict/winedt/catalan.dic",  "dict/winedt/croatian.dic",
            "dict/winedt/czech.dic",    "dict/winedt/danish.dic",
            #"dict/winedt/dutch.dic",
            "dict/winedt/english_uk.dic",
            "dict/winedt/french.dic",   "dict/winedt/german.dic",
            "dict/winedt/italian.dic",  "dict/winedt/latin.dic",
            "dict/winedt/polish.dic",   "dict/winedt/portuguese.dic",
            "dict/winedt/romanian.dic", #"dict/winedt/russian.dic",
            "dict/winedt/spanish.dic",  "dict/winedt/swedish.dic")
  data.frame(language, encoding, path, stringsAsFactors = FALSE)
}
