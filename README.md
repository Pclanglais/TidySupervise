# TidySupervise

The TidySupervise project aims to fill a gap. Unsupervised text classification is already well-represented in the R text mining ecosystem with tools like stm, that have few equivalents in other programming languages. Supervised methods are not as prevalent and most existing packages, like RTextTools, are not readily compatible with the tidyverse.

Supervised models can still be very useful. Although they require some extensive data and corpora preparations before hands, this trade-off comes with significant advantages:
* Models are fixed and transferable.
* Model results do not require lengthy interpretation, since categories have been already defined.
* Applying the model is very quick and scale well with big corpora.

TidySupervise is still in the alpha stage: supervised classification is currently limited to SVM models.

You can install the package using 

```
devtools::install_github("Numapresse/TidySupervise")
```

An introduction vignette gives a thorough tour of all the functions, using a labelled dataset of French newspaper from the 1920s and the 1930s.