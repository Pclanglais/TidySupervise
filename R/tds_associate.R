#' A function to combine the text with a labelled set of metadata.
#'
#' @param dir A directory containing the training corpora, with one document per text file.
#' @param training_metadata The name of the file containing the name of the name of the documents, the labels and possibly other metadata.
#' @param csv_type A type of csv file: "comma" (by default) uses read_csv while "semicolon" uses read_csv2.
#' @param max_char The maximum number of characters per documents (500000 by default). In practive, it should not be a problem.
#' @return A data frame combining the texts located in a directory and the labelled data in a tsv/csv file.
#' @examples
#'  tds_associate(dir = "corpora", training_metadata = "my_corpora.csv")
tds_associate <- function(dir, training_metadata = "", csv_type = "comma", max_char = 500000) {

  #Open the training_metadata file with either tsv or csv
  if (grepl("tsv$", training_metadata)) {
    training_metadata = read_tsv(training_metadata)
  }

  if (grepl("csv$", training_metadata)) {
    if(csv_type == "comma") {
      training_metadata = read_csv(training_metadata)
    }
    if(csv_type == "semicolon"){
      training_metadata = read_csv2(training_metadata)
    }
  }

  #Get all the files from the directory.
  text_files = list.files(dir, full.names = TRUE)

  #Create the text data frame.
  text_data = data_frame(document = basename(text_files),
                         text = map(text_files, readChar, nchar = max_char))

  text_data = text_data %>% unnest(text)

  #Join the metadata on the text data frame.
  text_data = text_data %>%
    inner_join(training_metadata, by=c("document" = "document"))

  return(text_data)

}
