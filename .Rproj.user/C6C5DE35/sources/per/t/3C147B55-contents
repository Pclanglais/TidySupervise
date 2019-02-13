
#' A function to output the word probabilities inside a model.
#'
#' @param model An SVM model.
#' @param show_graphic If set to TRUE (default), using the function will produce a graphic of the most important word per labels.
#' @param display_token The number of word per labels to show in the graphics.
#' @return A tidy dataset from the predictions
#' @examples
#'  tds_retrieve(predictions)
tds_open <- function(model, show_graphic = TRUE, display_token = 10) {

  #Crowling through the complicated svm data structure to get the labels and the documents…
  labels <- data_frame(label = model$fitted, document = attr(model$fitted, "name"))

  #…and the values per word
  grouped_sv <- as_data_frame(model$SV) %>%
    mutate(document = row.names(model$SV)) %>%
    inner_join(labels, by=c("document" = "document")) %>%
    group_by(label) %>%
    select(-document) %>%
    summarise_all(mean)

  grouped_word <- grouped_sv %>% gather(token, mean_value, -label)

  #Display a graphic with the main words
  if (show_graphic == TRUE) {
    if("ggplot2" %in% rownames(installed.packages())) {
      graph_words <- grouped_word %>%
        mutate(label = as.character(label)) %>%
        group_by(label) %>%
        top_n(display_token, mean_value) %>% #We only show the a number of word equal to display_token
        ungroup() %>%
        arrange(label, -mean_value) %>%
        mutate(token = as.factor(as.character(token))) %>%
        mutate(token = reorder(token, mean_value)) %>%
        mutate(label = as.factor(label)) %>%
        ggplot(aes(reorder(token, mean_value), mean_value, fill = label)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        facet_wrap(~ label, scales = "free_y", ncol=3) +
        coord_flip() +
        labs(x="", y="", title = "Main tokens used by the model to guess the label") +
        theme_classic()
    }

    print(graph_words)
  }

  return(grouped_word)
}
