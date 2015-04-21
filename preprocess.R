PrepareVoynichManuscript <- function(filepath) {
  # load file with manuscript
  file <- scan(filepath, character(0))
  # remove characters !, -, %, ?, =
  file <- gsub("[!=?%-]", "", file)
}

CreateVoynichDict <- function(manuscript) {
  # retrive single words
  words <-sapply(manuscript, strsplit, split = "[.]")
  # make vector with unique words
  words <- unique(unlist(words))
  # remove words with uncertain character (*)
  words <- words[-grep("[*]", words)]
  # order words by length (ascending)
  words[order(nchar(words), words)]
}

LoadLanguageDict <- function(dictPath, encoding = "UTF-16LE") {
  # load dict file
  dict <- scan(dictPath, character(0), fileEncoding = encoding)
  # use only unique words
  dict <- unique(dict)
}
