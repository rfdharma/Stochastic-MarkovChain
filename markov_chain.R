# Data wrangling
library(tidyverse)

# Text processing
library(tidytext)
library(textclean)
library(tokenizers)

# Markov Chain
library(markovchain)

# a single sentence
short_text <- c("Pemilu di Indonesia melibatkan banyak partai politik yang bersaing untuk memperoleh dukungan dari pemilih Calon-calon yang akan bertanding berasal dari berbagai partai politik dan independen Para pemilih akan mempertimbangkan berbagai faktor seperti visi politik program rekam jejak dan integritas calon saat memutuskan untuk memberikan suara Penting bagi warga negara Indonesia untuk mengikuti perkembangan politik mendapatkan informasi tentang berbagai calon dan partai politik yang berkompetisi serta memahami isu-isu yang relevan dalam pemilu Mengacu pada sumber berita yang andal mempelajari platform dan program calon serta berpartisipasi dalam diskusi terbuka dengan orang-orang di sekitar")

# split the sentence into words
text_term <- strsplit(short_text, split = " ") %>% unlist()

short_text

fit_markov <- markovchainFit(text_term, method = "laplace")

set.seed(123)
plot(fit_markov$estimate)


################################################################################
#Dataset

library(markovchain)
library(dplyr)
library(readxl)

df <- read.csv('result.csv', sep=';')
df <- read_excel("stokas.xlsx")

df$text_term <- strsplit(as.character(df$clean), split = " ")

text_states <- unlist(df$text_term)

fit_markov <- markovchainFit(text_states, method = "laplace")

set.seed(42)

plot(fit_markov$estimate)


# generate semi-random sentence
for (i in 1:5) {
  
  set.seed(i)
  markovchainSequence(n = 5,
                      markovchain = fit_markov$estimate, # transition matrix
                      t0 = "mie", include.t0 = T) %>%  # set the first word
    
    # joint words
    paste(collapse = " ") %>% 
    paste0(".") %>% 
    print()
}


# predictive
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
predictive_text("beli mie rendang sama ayang 😉",10)
