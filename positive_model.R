#Dataset

library(markovchain)
library(dplyr)
library(readxl)

df <- read_excel('positif.xlsx')

df$text_term <- strsplit(as.character(df$text), split = " ")

text_states <- unlist(df$text_term)

fit_markov <- markovchainFit(text_states, method = "laplace")

set.seed(42)

plot(fit_markov$estimate)


# generate semi-random sentence
for (i in 1:3) {
  
  set.seed(i)
  markovchainSequence(n = 20,
                      markovchain = fit_markov$estimate, # transition matrix
                      t0 = "Ganjar", include.t0 = T) %>%  # set the first word
    
    # joint words
    paste(collapse = " ") %>% 
    paste0(".") %>% 
    print()
}


# a single sentence
short_text <- c("Dalam perbandingan antara Anies dan Prabowo, Ganjar menunjukkan keunggulan dengan memiliki gagasan dan ide yang lebih kreatif serta menjadi pemimpin masa depan yang paling solutif. Ganjar lebih inovatif dalam menyajikan konsep-konsep yang cemerlang dan menjadi sosok pemimpin yang memiliki visi mendalam untuk masa depan.")

# split the sentence into words
text_term <- strsplit(short_text, split = " ") %>% unlist()

short_text

fit_markov_short <- markovchainFit(text_term, method = "laplace")

set.seed(123)
plot(fit_markov_short$estimate)
