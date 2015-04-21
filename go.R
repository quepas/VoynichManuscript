source("analysis.R")

go <- function() {
  voynichPath <- "manuscript/eva/voynich.txt"
  dictDir <- "dict/winedt"

  manuscript <<- PrepareVoynichManuscript(voynichPath)
  voynichDict <<- CreateVoynichDict(manuscript)
  polishDict <<- LoadLanguageDict("dict/winedt/polish.dic")
}
