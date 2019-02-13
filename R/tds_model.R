
#' A function to train an SVM model.
#'
#' @param text_count A tidy dataframe of textual data pre-processed by tds_process
#' @param classification_matrix A term/document matrix. Set to false (by default) to create the matrix from text_count.
#' @param prob_state Set to true (default) to get the detailed probabilities. This is highly recommended but can take longer.
#' @param min_label The minimal number of document per labels. If some labels are poorly represented and harder to predict it can be useful to drop them.
#' @param max_label The maximal number of document per labels. SVM is sensible to over-fitting: if a label is too represented in the original corpora, it will over-populate the results.
#' @param cost_variable A parametrized value for svm classification (purely optional)
#' @return An SVM model that can be interpreted using tds_open.
#' @examples
#'  tds_model(text_count, classification_matrix = FALSE, prob_state = TRUE, min_label = 70, max_label = 150, cost_variable = 0)
tds_model <- function(text_count, classification_matrix = FALSE, prob_state = TRUE, min_label = 20, max_label = 100, cost_variable = 0){

  if (classification_matrix == FALSE) {
    classification_matrix = tds_matrix(text_count)
  }
  #We retrieve the name and label of each document.
  classification_code <- data_frame(segment = row.names(classification_matrix))
  classification_code <- classification_code %>%
    left_join(text_count %>% group_by(segment, label) %>% summarise(), by=c("segment" = "segment")) %>%
    mutate(labels = as.factor(label))

  if(min_label != 0 | max_label != 0) {
    #We apply several filters to the document selection (mostly by creating a random selection per label)
    classification_code <- classification_code %>% group_by(label) %>% mutate(total_label=n()) %>% ungroup()

    #A default in case max_label would be ill-defined (for instance bigger than min_label): it is switched off to 0.
    if(max_label <= min_label & max_label > 0) {
      max_label = 0
      message("max_label couldn't be used as it shouldn't be smaller than min_label")
    }

    if(min_label > 0) {
      classification_code <- classification_code %>% filter(total_label>min_label) %>% ungroup()
    }

    if(max_label > 0) {
      sample_label <- classification_code %>% filter(total_label>max_label) %>% group_by(label) %>% sample_n(max_label) %>% ungroup()
      classification_code <- classification_code %>% filter(total_label<max_label) %>% bind_rows(sample_label)
      classification_code <- classification_code %>% sample_n(nrow(classification_code))
    }

    #We filter the original matrix.
    classification_matrix <- classification_matrix[classification_code$segment,]
  }

  #Security check if some column sums are not equal to zero.
  i <- (colSums(classification_matrix, na.rm=T) != 0)
  classification_matrix <- classification_matrix[,i]

  #Actual modeling.
  if (cost_variable == 0) {
    fit <- svm(classification_matrix, classification_code$labels, kernel = "linear", probability = prob_state)
  } else {
    fit <- svm(classification_matrix, classification_code$labels, kernel = "linear", probability = prob_state, cost = cost_variable)
  }

  return(fit)
}
