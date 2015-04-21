PrepareVoynichManuscript <- function(filepath) {
  # load file with manuscript
  file <- scan(filepath, character(0))
  # remove characters !, -, %, ?, =
  file <- gsub("[!=?%-]", "", file)
  file
}
