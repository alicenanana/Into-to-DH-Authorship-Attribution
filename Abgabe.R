# Required Libraries
library(dplyr) 
library(stringr) 
library(udpipe)
library(tm)
library(textstem)
library(SnowballC)
library(magrittr)  
library(stopwords) 
library(qdap)
library(stylo)
library(ggplot2)

# Set options
options(stringsAsFactors=FALSE)

# Function to extract third-order character trigrams
txt_to_features <- function(script_text, ngram_size) {
  ngram_list <- list()
  for (i in 1:(nchar(script_text) - ngram_size + 1)) {
    ngram <- substr(script_text, i, i + ngram_size - 1)
    ngram_list[[i]] <- ngram
  }
  ngrams <- unlist(ngram_list)
  return(ngrams)
}

# Function to extract POS tag ngrams
extract_pos_ngrams <- function(tokens, ngram_size) {
  pos_ngrams <- lapply(1:(nrow(tokens) - ngram_size + 1), function(i) {
    paste(tokens$pos[i:(i + ngram_size - 1)], collapse = " ")
  })
  return(pos_ngrams)
}

# Function to calculate POS tag ngram frequencies
calculate_pos_ngram_frequencies <- function(tokens, ngram_size) {
  pos_ngrams <- extract_pos_ngrams(tokens, ngram_size)
  # Remove punctuation from POS n-grams
  pos_ngrams <- lapply(pos_ngrams, function(ngram) gsub("[[:punct:]]", "", ngram))
  pos_ngrams <- unlist(pos_ngrams)
  pos_ngram_freq <- table(pos_ngrams)
  pos_ngram_freq_sorted <- sort(pos_ngram_freq, decreasing = TRUE)
  return(pos_ngram_freq_sorted)
}

# Function Definitions
# Remove non-textual elements
remove_non_textual_elements <- function(script_text) {
  script_text <- gsub(":", "", script_text)
  script_text <- gsub("-", "", script_text)
  script_text <- gsub("\\(b[a-z]+\\b)", "", script_text)
  return(script_text)
}

# Remove words that are all in capital letters
remove_all_caps_words <- function(script_text) {
  script_text <- gsub("\\b[A-Z]+\\b", "", script_text)
  return(script_text)
}

# Convert text to lowercase
convert_to_lowercase <- function(script_text) {
  return(tolower(script_text))
}

# Expand contractions
expand_contractions <- function(text) {
  contractions <- list(
    "'s\\b" = " is",
    "'m\\b" = " am",
    "'re\\b" = " are",
    "'ve\\b" = " have",
    "n't\\b" = " not",
    "'d\\b" = " would",
    "'ll\\b" = " will",
    "'t\\b" = " not"
  )
  for (pattern in names(contractions)) {
    replacement <- contractions[[pattern]]
    text <- gsub(pattern, replacement, text, perl = TRUE)
  }
  return(text)
}

# Tokenization
perform_tokenization <- function(script_text, model) {
  # Remove punctuation before tokenization
  script_text <- removePunctuation(script_text)
  
  annotation <- udpipe_annotate(model, x = script_text) %>%
    as.data.frame()
  tokens <- annotation$token
  pos <- annotation$upos  # Change to upos for universal part-of-speech tags
  stems <- annotation$lemma
  return(data.frame(token = tokens, pos = pos, stem = stems))
}

# Remove stopwords using stopwords package
remove_stopwords <- function(tokens) {
  stopwords <- stopwords::stopwords("en")
  tokens_cleaned <- tokens[!tolower(tokens$token) %in% stopwords, ]
  return(tokens_cleaned)
}

# Perform stemming
perform_stemming <- function(tokens) {
  tokens$stem <- wordStem(tokens$token)
  return(tokens)
}

# Function to filter out pronouns
filter_pronouns <- function(tokens) {
  pronouns <- c("PRON", "PRP", "PRP$")
  filtered_tokens <- tokens[tokens$pos %in% pronouns, ]
  return(filtered_tokens)
}

# Function to calculate word frequency
calculate_word_frequency <- function(tokens) {
  tokens_aggregated <- aggregate(tokens$stem, by = list(tokens$stem), FUN = length)
  names(tokens_aggregated) <- c("Word", "Frequency")
  tokens_aggregated$Word <- removePunctuation(tokens_aggregated$Word)
  tokens_aggregated$Word <- removeWords(tokens_aggregated$Word, stopwords("en"))
  tokens_aggregated$Word <- trimws(tokens_aggregated$Word)
  tokens_aggregated <- tokens_aggregated[nchar(tokens_aggregated$Word) > 0, ]
  sorted_word_freq <- tokens_aggregated[order(-tokens_aggregated$Frequency), ]
  top_100_words <- head(sorted_word_freq, 100)
  return(top_100_words)
}

# Function to extract sentence lengths
extract_sentence_lengths <- function(script_text) {
  sentences <- qdap::sent_detect(script_text)
  sentence_lengths <- sapply(sentences, function(sentence) {
    words <- unlist(strsplit(sentence, "\\s+"))
    return(length(words))
  })
  return(sentence_lengths)
}

# Function to calculate POS tag frequencies
calculate_pos_frequencies <- function(tokens) {
  pos_aggregated <- aggregate(tokens$pos, by = list(tokens$pos), FUN = length)
  names(pos_aggregated) <- c("POS_Tag", "Frequency")
  sorted_pos_freq <- pos_aggregated[order(-pos_aggregated$Frequency), ]
  return(sorted_pos_freq)
}

# Function to calculate POS ngram frequencies
calculate_pos_ngram_frequencies <- function(tokens, ngram_size) {
  pos_ngrams <- extract_pos_ngrams(tokens, ngram_size)
  # Remove punctuation from POS n-grams
  pos_ngrams <- lapply(pos_ngrams, function(ngram) gsub("[[:punct:]]", "", ngram))
  pos_ngrams <- unlist(pos_ngrams)
  pos_ngram_freq <- table(pos_ngrams)
  pos_ngram_freq_sorted <- sort(pos_ngram_freq, decreasing = TRUE)
  return(pos_ngram_freq_sorted)
}

# Function to compare POS tag tables for similarity
compare_pos_tables <- function(pos_tables) {
  # Calculate relative frequencies for each table
  rel_freq_tables <- lapply(pos_tables, function(pos_table) {
    total_count <- sum(pos_table$Frequency)
    rel_freq <- pos_table$Frequency / total_count
    return(data.frame(POS_Tag = pos_table$POS_Tag, Relative_Frequency = rel_freq))
  })
  
  # Get unique POS tags
  unique_tags <- unique(unlist(lapply(rel_freq_tables, function(x) x$POS_Tag)))
  
  # Fill missing values with zeros
  rel_freq_tables_filled <- lapply(rel_freq_tables, function(pos_table) {
    missing_tags <- setdiff(unique_tags, pos_table$POS_Tag)
    if (length(missing_tags) > 0) {
      missing_data <- data.frame(POS_Tag = missing_tags, Relative_Frequency = rep(0, length(missing_tags)))
      pos_table <- rbind(pos_table, missing_data)
    }
    return(pos_table)
  })
  
  # Convert to matrix
  rel_freq_matrices <- lapply(rel_freq_tables_filled, function(pos_table) {
    matrix(pos_table$Relative_Frequency, nrow = length(unique_tags), byrow = TRUE)
  })
  
  # Calculate cosine similarity matrix
  similarity_matrix <- matrix(NA, nrow = length(pos_tables), ncol = length(pos_tables))
  for (i in 1:length(pos_tables)) {
    for (j in 1:length(pos_tables)) {
      vec1 <- rel_freq_matrices[[i]]
      vec2 <- rel_freq_matrices[[j]]
      similarity <- sum(vec1 * vec2) / (sqrt(sum(vec1^2)) * sqrt(sum(vec2^2)))
      similarity_matrix[i, j] <- similarity
    }
  }
  
  # Print similarity matrix
  cat("\nCosine Similarity Matrix:\n")
  print(similarity_matrix)
  
  # Find most similar tables for each table
  most_similar <- list()
  for (i in 1:length(pos_tables)) {
    most_similar_indices <- order(similarity_matrix[i, ], decreasing = TRUE)[2:3]  # Excluding self
    most_similar[[i]] <- pos_tables[most_similar_indices]
  }
  
  # Print most similar tables
  cat("\nMost Similar POS Tag Tables:\n")
  for (i in 1:length(pos_tables)) {
    cat("\nFile:", names(pos_tables)[i], "\n")
    print(pos_tables[[i]])
    cat("\nMost Similar Tables:\n")
    print(most_similar[[i]])
  }
}


# Define folder path
folder_path <- "C:/Users/mofir/Desktop/Intro to DH/imsdb_raw_nov_2015/Animation"
file_paths <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE)

# Load udpipe model
m_eng <- udpipe_load_model(file = "english-ewt-ud-2.5-191206.udpipe")

# Initialize combined tables
combined_tables <- list()

for (file_path in file_paths) {
  cat("Processing script:", file_path, "\n")
  script_text <- readLines(file_path, encoding = "UTF-8") %>% paste0(collapse = "")
  
  # Preprocessing
  script_text <- script_text %>%
    remove_non_textual_elements() %>%
    remove_all_caps_words() %>%
    convert_to_lowercase() %>%
    expand_contractions() 
  
  # Check if the input text is not empty
  if (nchar(script_text) == 0) {
    cat("Input text is empty after preprocessing.\n")
    next
  }
  
  # Tokenization
  output <- perform_tokenization(script_text = script_text, model = m_eng)
  
  # Calculate word frequency
  word_freq_table <- calculate_word_frequency(output)
  
  # Remove stopwords
  output_cleaned <- remove_stopwords(output)
  
  # Filter out pronouns
  output_no_pronouns <- filter_pronouns(output_cleaned)
  
  # Calculate POS tag frequencies
  pos_freq_table <- calculate_pos_frequencies(output)
  
  # Calculate POS ngram frequencies
  pos_ngram_freq_table <- calculate_pos_ngram_frequencies(output_no_pronouns, ngram_size = 3)
  
  # Store the frequency tables for each file
  combined_tables[[file_path]] <- list(word_freq_table = word_freq_table,
                                       pos_freq_table = pos_freq_table,
                                       pos_ngram_freq_table = pos_ngram_freq_table)
}

# Call the function with combined POS tag tables
compare_pos_tables(lapply(combined_tables, function(x) x$pos_freq_table))

librray("stylo")
stylo()
