source("analysis.R")

go <- function() {
  voynichPath <- "manuscript/eva/voynich.txt"

  manuscript <<- PrepareVoynichManuscript(voynichPath)
}
