suppressWarnings(library(tm))

custom_input_text_clean <- function (testline) 
{
  line <- iconv(testline, "latin1", "ASCII", sub = "")
  line <- gsub('[0-9]+', '', line)
  line <- tolower(line)
  line <- removeWords(line, stopwords())
  line <- removePunctuation(line)
  line <- gsub('\\S+[^\x20-\x7E]', '', line)
  emptyLines <- grepl('^\\s*$', line)
  line <- line[!emptyLines]
  line <- stripWhitespace(line)
  return(as.data.frame(line))
}