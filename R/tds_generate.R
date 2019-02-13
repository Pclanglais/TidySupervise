
#' A function to generate a csv file for training out of a directory of text
#'
#' @param dir A directory containing the training corpora, with one document per text file.
#' @param training_file The name of the file containing the output a the function: a dataset linking document name (and, possibly, texts) with empty labels.
#' @param metadata_only Whether the output will contain only the metadata (the name of the documents) or also the text. Set to false by default.
#' @param sample_document A sample size smaller than the number of the document in the directory. When set at 0 (default), sampling is not used.
#' @param output_format The format of the file where the data frame will be stored. Either "tsv" or "csv": "tsv" is usually preferable for textual data, since the tab key is much less used than punctuation.
#' @param max_char The maximum number of characters per documents (500000 by default). In practive, it should not be a problem.
#' @return A data frame prepared for classification labelling.
#' @examples
#'  tds_generate(dir = "corpora", training_file = "my_corpora.csv", metadata_only = TRUE, sample_document = 500, output_format = "csv")
tds_generate <- function(dir, training_file = "", metadata_only = FALSE, sample_document = 500, output_format = "csv", max_char = 500000) {

  #output_format has to be either csv or tsv.
  if (!output_format %in% c("tsv", "csv")) {

    output_format = "tsv"

  }

  #If a training file has not been stated, we will reuse the name of the directory.
  if (training_file == "") {

    training_file = paste0(dir, ".", output_format)

    }

  #Get all the files from the directory.
  text_files = list.files(dir, full.names = TRUE)

  #Sample the list of files, if sample_document is higher than 0 and smaller than the number of files.
  if (sample_document > 0 & length(text_files) > sample_document) {
    text_files = sample(text_files, sample_document)
  }

  #Retrieve all the texts and initialize the classification dataset.
  if (metadata_only == FALSE) {

    text_data = data_frame(document = basename(text_files),
                           label = "",
                           text = map(text_files, readChar, nchar = max_char))

    text_data = text_data %>% unnest(text)

  } else {

    text_data = data_frame(document = basename(text_files),
                           label = "")

  }

  if (output_format == "tsv") {

    write_tsv(text_data, training_file)

  }

  if (output_format == "csv") {

    write_csv(text_data, training_file)

  }
}
