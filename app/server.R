library(shiny)
library(ElemStatLearn)

# Preprocessing function: converts the mail to a vector of words
preprocess <- function(mail){
  # Split the mail into separate words, divided by (multiple) spaces or \n
  words <- unlist(strsplit(mail, '\n| +'))
  words1 <- words
  # Find indices with any remaining non-alphanumeric words
  noAlpNum <- which(!grepl('^[[:alnum:]]*$', words))
  # For these words, split the word into alphanumeric contributions and the rest
  if(length(noAlpNum) > 0){
    for (i in noAlpNum){
      # Split the i-th word
      new_words <- split_alphanumeric(words[i])
      # Add the subwords
      words <- c(words, new_words)
    }
    # Remove the noAlpNum words
    words <- words[-noAlpNum]
  }
  return(words)
}


# Split_alphanumeric function splits a word on any non-alphanumeric character
split_alphanumeric <- function(word){
  # new_words will contain a vector of new words
  new_words <- character(0)
  # new word will contain the current word
  new_word <- character(0)
  for (j in 1:nchar(word)){
    ch <- substr(word,j,j)
    # if the current character is alphanumeric, add it to current word
    if (grepl('[[:alnum:]]', ch)){
      new_word <- paste(new_word, ch, sep='')
    }
    # if not, add previous word to new_words, as well as the current character,
    # and start new word
    else{
      new_words = c(new_words, new_word, ch)
      new_word = character(0)
    }
    # if we are on the end of the word, add this word to new_words
    if(j==nchar(word) & length(new_word) > 0) new_words = c(new_words, new_word)
  }
  return(new_words)
}

# The frequency function calculates the frequency with which a word is found in a vector
frequency <- function(key, words){
  # Make sure the exact word is retrieved
  regex = paste('^',tolower(key),'$',sep='')
  # Sum the number of occurances, and remove uppercases
  ab <- sum(grepl(regex, tolower(words)))
  # Count the number of words
  return(ab/length(words))
}

# The capitals function calculates the number of capitals in a vector of words
capitals <- function(words){
  # Vector capitals will hold the number of capitals for each word
  capitals <- numeric(0)
  for (i in 1:length(words)){
    word <- words[i]
    # Vector capital will hold the number of capitals for the current word
    capital <- 0
    for (j in 1:nchar(word)){
      ch <- substr(word,j,j)
      if(grepl('^[[:upper:]]$', ch)){ capital <- capital + 1 }
    }
    capitals <- c(capitals, capital)
    capital <- 0
  }
  return(capitals)
}

# Makes a data frame from the given mail
makeDF <- function(mail){
  # Split the mail in words, separated by non-alphanumeric characters
  words <- preprocess(mail)
  # Calculate the number of capitals in each word
  n_capitals <- capitals(words)
  # Make a data frame for the new mail
  mail_DF <- data.frame(t(rep(0,ncol(spam)-1)))
  names(mail_DF) <- names(spam)[1:ncol(spam)-1]
  # For column 1 to 48: calculate the frequency of a given word
  for (i in 1:48){
    # Extract the key from the column name
    key = names(mail_DF)[i]
    # Remove the prefix 'num' if necessary
    if(grepl('^num[[:digit:]]',key)){
      key <- substr(key,4,nchar(key))
    }
    # Count the frequency which with the word 'key' appears, and put in the data frame
    mail_DF[i] <- frequency(key, words)
  }
  # For column 49: a semicolumn
  mail_DF[49] <- frequency(';', words)
  # For column 50: a round bracket
  mail_DF[50] <- frequency('\\(', words) + frequency('\\)', words)
  # For column 51: a square bracket
  mail_DF[51] <- frequency('\\[', words) + frequency('\\]', words)
  # For column 52: an exclamation point
  mail_DF[52] <- frequency('!', words)
  # For column 53: a dollar sign
  mail_DF[53] <- frequency('$', words)
  # For column 54: a hash sign
  mail_DF[54] <- frequency('#', words)
  # For column 56: the longest number of capital sequence
  mail_DF[56] <- max(n_capitals)
  # For column 55: the average length of capital sequence (not taking into account all lowercase words)
  if(mail_DF[56] > 0) {mail_DF[55] <- mean(n_capitals[n_capitals != 0])}
  # For column 57: the total number of capitals in a sequence
  mail_DF[57] <- sum(n_capitals)
  return(mail_DF)
}

result <- function(mail_DF){
  data(spam)
  # Convert the type column to 0 (= nonspam) and 1 (= spam)
  spam[ , ncol(spam)] <- as.numeric(spam[ , ncol(spam)])-1
  names(spam)[ncol(spam)] <- 'type'
  # Train a linear model to these data
  mod <- lm(type~., data=spam)
  
  # Predicted spam value
  predVal <- predict(mod, newdata=mail_DF)
  # Set limits
  if(predVal < 0){predVal <- 0}
  if(predVal > 1){predVal <- 1}
  
  # Define whether the mail is spam or not
  if(predVal < 0.5){
    text <- 'non-spam'
    proc <- round(100*(1-predVal),2)
  }
  else{
    text <- 'spam'
    proc <- round(100*predVal,2)
  }
  
  # Give result
  return(paste('Your mail is ', text, ' with a probability of ', proc, '%.', sep=''))
}

shinyServer(
  function(input,output){
    output$oid1 <- renderText(result(makeDF(input$id1)))
  }
)