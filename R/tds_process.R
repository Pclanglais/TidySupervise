
#' A function to get word count corrected by tf_idf values
#'
#' @param text_count A tidy dataframe of textual data (typically the "long" format used by tidytext)
#' @param training Set to true if the output of the text process is to train a new model. The function will simply keep a "label" column that should exist.
#' @param segment_size The standard size of a continuous segment of text (100 as a default). Remaining segments within a document will be discarded.
#' @param no_number Set to true to remove all numbers from the dataset. This is usually recommended.
#' @param lemmatization Use one of the five available languages for lemmatization: "english", "french", "german", "spanish" or "italian". Lemmatization is approximative but usually enhance the results (since inflected words can be more readily used)
#' @param min_doc_count The minimum number of documents where a word should be present. It's useful to avoid over-fitting.
#' @param min_word_count The minimum number of occurrences of a word all documents considered.
#' @param max_word_set The maximum number of word to keep (3000 as a default). Massive matrix becomes harder to process for little gain.
#' @return A processed version of the textual dataset using tf_idf values with cosine normalization.
#' @examples
#'  tds_clean(text_count, segment_size = 90, min_doc_count = 4, max_word_set = 3000)
tds_process <- function(text_count, segment_size = 100, training = FALSE, lemmatization = "none", no_number = TRUE, min_doc_count = 2, min_word_count = 0, max_word_set = 3000) {

  #Cleaning the numbers (recommended)
  if (no_number == TRUE) {
    text_count <- text_count %>% filter(!grepl("\\d+", token))
  }

  #Using lemmatization (recommended but not always available and need to now the language in advance: default to none)
  available_lemmatization = c("french", "english", "spanish", "german", "italian")
  if (lemmatization %in% available_lemmatization) {
    #We load the zipped lookup files when they exist.
    lemmatization_file = paste0("lookup_", lemmatization, ".tsv.zip")
    lookup = readr::read_tsv(system.file("extdata", lemmatization_file, package = "tidysupervise"), col_types = "cc")

    #We transform token to lemma. Every token that is not in the lookup table is kept as such.
    text_count = text_count %>%
      left_join(lookup, by=c("token" = "token")) %>%
      mutate(lemma = ifelse(is.na(lemma), token, lemma)) %>%
      select(-token) %>%
      rename(token = lemma)
  }

  #We initialize the segments and filter every segments that is lower than the mean segment size.
  text_count <- text_count %>%
    group_by(document) %>%
    mutate(word_id = 1:n()) %>%
    ungroup() %>%
    mutate(segment = paste0(document, "_", (word_id+segment_size)%/%segment_size)) %>%
    ungroup() %>%
    group_by(segment) %>%
    mutate(total_segment = max(1:n())+1) %>%
    filter(total_segment>=segment_size)

  #Everything is set: we can now count the occurrences.
  #We check if we are in the training phase or not. In the last case, label do not exist (since we are to predict them)
  if(training == TRUE) {
    text_count <- text_count %>% group_by(document, label, segment, token) %>% summarise(count = n()) %>% ungroup()
  } else {
    text_count <- text_count %>% group_by(document, segment, token) %>% summarise(count = n()) %>% ungroup()
  }

  #We filter all the occurrences that do not appear in n different documents (n = min_doc_count)
  #Default is 2 (as word occurring in only one document will not be that useful)
  #We also filter the words that are below the nth row in descending occurrences order.
  #Default is 3000. Otherwhise, the matrix can become very heavy for little gains.
  #Both filters are done in one batch to avoid code duplication.
  if (min_doc_count > 0 | max_word_set > 0) {
    occur_doc <- text_count %>% group_by(token, document) %>% summarise() %>% ungroup() %>% group_by(token) %>% summarise(doc_count = n())

    if(min_doc_count > 0) {
      occur_doc = occur_doc %>% filter(doc_count>=min_doc_count)
    }

    #We ensure that the word set is smaller than max_word_set
    if(max_word_set > 0 & nrow(occur_doc) > max_word_set) {
      occur_doc = occur_doc %>% arrange(-doc_count) %>% mutate(rank = min_rank(desc(doc_count))) %>% filter(rank < max_word_set)
      occur_doc = occur_doc %>% filter(rank < max(rank))
    }

    #We keep only the word that are still selected
    text_count <- text_count %>% inner_join(occur_doc %>% select(token), by=c("token" = "token"))
  }

  #We filter all the occurrences that do not appear at least n time.
  if (min_word_count > 0) {
    text_count <- text_count %>% group_by(token) %>% mutate(word_count = n()) %>% filter(word_count >= min_word_count) %>% ungroup()
  }

  #We extract the tf_idf values thanks to the tidy text package
  text_count <- text_count %>% bind_tf_idf(token, segment, count)

  #We apply cosine normalization (essential to get good svm results)
  text_count <- text_count %>% group_by(segment) %>% mutate(cosine_variable = sqrt(sum(tf_idf^2)))
  text_count <- text_count %>% ungroup() %>% mutate(corrected_tf_idf = tf_idf/cosine_variable)

  return(text_count)
}
