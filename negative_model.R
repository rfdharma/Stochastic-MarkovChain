library(markovchain)
library(dplyr)
library(readxl)

df <- read_excel('negatif.xlsx')

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
short_text <- c("Syarat pendukung Prabowo dianggap memiliki IQ rendah dan melakukan bully serta menghujat. Ada kritik bahwa sikap pendukung Prabowo berubah menjadi penjilat uang dan kekuasaan.")

# split the sentence into words
text_term <- strsplit(short_text, split = " ") %>% unlist()

short_text

fit_markov_short <- markovchainFit(text_term, method = "laplace")

set.seed(42)
plot(fit_markov_short$estimate)
