# Data wrangling
library(tidyverse)

# Text processing
library(tidytext)
library(textclean)
library(tokenizers)

# Markov Chain
library(markovchain)

# a single sentence
short_text <- c("masih produksi kak mie dan beberapa snack seperti chimi ubi brownies crispy cassamo dan sola farm bisa kakak temui di indomaret maupun alfamart terdekat serta beberapa supermarket lainnya semoga membantu ya kak 😊🙏🏻 terima kasih")

# split the sentence into words
text_term <- strsplit(short_text, split = " ") %>% unlist()

short_text

fit_markov <- markovchainFit(text_term, method = "laplace")

set.seed(123)
plot(fit_markov$estimate)


# Assuming you have the required libraries loaded
library(markovchain)
library(dplyr)

# Read the CSV file
df <- read.csv('result.csv', sep=';')

# Split the text in the DataFrame into words
df$text_term <- strsplit(as.character(df$X.full_text), split = " ")

# Unlist the list of vectors into a single vector
text_states <- unlist(df$text_term)

# Fit a Markov chain model
fit_markov <- markovchainFit(text_states, method = "laplace")


# Set seed for reproducibility
set.seed(123)

# Plot the Markov chain model
plot(fit_markov$estimate)


# generate random sentence
for (i in 1:5) {
  
  set.seed(i)
  markovchainSequence(n = 6, # generate 7 next words 
                      markovchain = fit_markov$estimate, # transition matrix
                      t0 = "mie", include.t0 = T) %>%  # set the first word
    
    # joint words
    paste(collapse = " ") %>% 
    paste0(".") %>% 
    print()
}


predictive_text <- function(text, num_word) {
  library(stringr)
  
  text <- strsplit(text, " ") %>% unlist() %>% tail(1)
  
  # exclude punctuation
  punctuation <- which(fit_markov$estimate[tolower(text), ] %>% names() %>% str_detect("[:punct:]"))
  
  suggest <- fit_markov$estimate[tolower(text), -punctuation] %>%
    sort(decreasing = TRUE) %>% 
    head(num_word) 
  
  suggest <- suggest[suggest > 0] %>% 
    names()
  
  return(suggest)
}


predictive_text("beli mie goreng sama bakso sorean kalau lagi ada duit dan bahan nder", 5)
predictive_text("mie yang terbaik untuk menyehatkan masyarakat indonesia dengan varian baru", 5)
predictive_text("mie yang terlihat jelas pedes gledeek ya beli mie rendang ✨",10)
