#' A function to transform tf_idf counts into a matrix ready for svm classification
#'
#' @param text_count A tidy dataframe of textual data pre-processed by tds_process
#' @return A processed version of the textual dataset using tf_idf values with cosine normalization.
#' @examples
#'  tds_matrix(text_count)
tds_matrix <- function(text_count) {

  #We spread the word count in order to have one column by word and replace all nas by 0
  classification_matrix <- text_count %>% rename(segment_unique = segment) %>% select(segment_unique, token, corrected_tf_idf) %>% tidyr::spread(token, corrected_tf_idf)
  classification_matrix[is.na(classification_matrix)] <- 0

  #The dataset becomes a matrix and documents are displaced to row names
  segments <- classification_matrix$segment_unique
  classification_matrix <- as.matrix(classification_matrix[,2:ncol(classification_matrix)])
  row.names(classification_matrix) <- segments

  #We reorder everything into a random order to test our classification
  classification_matrix <- classification_matrix[sample(nrow(classification_matrix), nrow(classification_matrix)),]

  return(classification_matrix)
}
