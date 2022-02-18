
#' A function to test an SVM model
#'
#' @param text_count A tidy dataframe of textual data pre-processed by tds_process
#' @param classification_matrix A term/document matrix. Set to false (by default) to create the matrix from text_count.
#' @param prop_train The share of the original corpora that will be used for training the model. Default is 80\% (which means that the remaining 20\% will serve to evaluate the model).
#' @param min_label The minimal number of document per labels. If some labels are poorly represented and harder to predict it can be useful to drop them.
#' @param max_label The maximal number of document per labels. SVM is sensible to over-fitting: if a label is too represented in the original corpora, it will over-populate the results.
#' @param cost_variable A parametrized value for svm classification (purely optional)
#' @return Raw results in a tidy format.
#' @examples
#'  tds_test(text_count, prop_train = 85, min_label = 70, max_label = 150, cost_variable = 0)
tds_test <- function(text_count, classification_matrix = FALSE, prop_train = 80, min_label = 0, max_label = 0, cost_variable = 0){


  #A small hack to take into account the use of entire documents for classification purposes.
  if(!"segment" %in% colnames(text_count)) {
    text_count = text_count %>% rename(segment = document)
  }

  if (classification_matrix == FALSE) {
    classification_matrix = tds_matrix(text_count)
  }

  #Converting prop train to percents
  prop_train = prop_train/100

  #We retrieve the name and label of each document.
  classification_code <- tibble(segment = row.names(classification_matrix))
  classification_code <- classification_code %>%
    inner_join(text_count %>% group_by(segment, label) %>% summarise(), by=c("segment" = "segment")) %>%
    mutate(labels = as.factor(label))

  if(min_label != 0 | max_label != 0) {
    #We apply several filters to the document selection (mostly by creating a random selection per label)
    classification_code <- classification_code %>% group_by(label) %>% mutate(total_label=n()) %>% filter(total_label>min_label) %>% ungroup()
    sample_label <- classification_code %>% filter(total_label>max_label) %>% group_by(label) %>% sample_n(max_label) %>% ungroup()
    classification_code <- classification_code %>% filter(total_label<max_label) %>% bind_rows(sample_label)
    classification_code <- classification_code %>% sample_n(nrow(classification_code))

    #We filter the original matrix.
    classification_matrix <- classification_matrix[classification_code$segment,]
  }

  #We check the number of kept elements.
  n_element = round(nrow(classification_matrix)*prop_train, digits = 0)
  message(paste0("We are going to test the model on ", n_element, " documents"))

  #We separate the training matrix from the text matrix.
  train_matrix <- classification_matrix[1:n_element,]
  i <- (colSums(train_matrix, na.rm=T) != 0)
  train_matrix <- train_matrix[,i]
  test_matrix <- classification_matrix[(n_element+1):nrow(classification_matrix),colnames(train_matrix)]

  #Actual modeling.
  if (cost_variable == 0) {
    fit <- svm(train_matrix, classification_code$labels[1:n_element], kernel = "linear")
  } else {
    fit <- svm(train_matrix, classification_code$labels[1:n_element], kernel = "linear", cost = cost_variable)
  }
  predictions <- predict(fit,test_matrix)
  predictions <- tibble(original = classification_code$labels[(n_element+1):nrow(classification_matrix)], prediction = predictions) %>% mutate(result = ifelse(original == prediction, 1, 0))
  message(paste0("Results are at ", as.character(100*(sum(predictions$result)/nrow(predictions))), "% for ", as.character(nrow(predictions)), " attempts"))
  return(predictions)
}

