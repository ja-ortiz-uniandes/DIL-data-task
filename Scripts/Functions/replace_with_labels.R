# Function to replace values with their value labels
replace_with_labels <- function(df, ordered = T) {
  df %>%
    mutate(across(everything(), ~ {
      if (is.labelled(.) & !is.null(attr(., "labels"))) {
        as_factor(., ordered = ordered) %>% as.character
      } else {
        .
      }
    }))
}
