#' Tidy Data With AI and R
#'
#' This function takes a text string or a path to a text file containing non-tidy data as input,
#' processes the data using OpenAI's chat completion API enpoint as backend and returns a tidied data.frame
#' if the input is a text string or the file path of the generated CSV file.
#' If the input is a path to a text file (containing the messy data), the function
#'  also writes the tidied data to a new CSV file with the same name and path as
#'  the input file but with a "-tidy.csv" suffix.
#'
#' @param file A character string specifying the path to a CSV file, or a data.frame
#'   containing air data to be processed.
#' @return A data.frame containing the processed and tidied data if the input is
#'   a text string, or a character string with the file path of the generated CSV file if the input is a file.
#' @author Ulrich Matter umatter@protonmail.com
#'
#' @export
#' @import OpenAIR
#' @importClassesFrom OpenAIR chatlog
#' @examples
#' \dontrun{
#' # Create a messy data.frame with air data
#' messy_data <- data.frame(
#'   date_pm25 = c("2021-01-01|10", "2021-01-02|12", "2021-01-03|15"),
#'   date_pm10 = c("2021-01-01|20", "2021-01-02|25", "2021-01-03|30"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Process the data.frame using tidyair
#' tidied_data <- tidyair(air_data)
#' print(tidied_data)
#'
#' messy_md <-
#' "
#' | Merged Economics Book Data                 |
#' |--------------------------------------------|
#' | The Wealth of Nations - Adam Smith - 12000 |
#' | Capital - Karl Marx - 15000                |
#' | The General Theory of Employment, Interest and Money - John Maynard Keynes - 9000 |
#' | The Road to Serfdom - Friedrich Hayek - 8000 |
#' | Human Action - Ludwig von Mises - 6000     |
#' | The Theory of the Leisure Class - Thorstein Veblen - 4500 |
#' | The Great Transformation - Karl Polanyi - 4000 |
#' | An Inquiry into the Nature and Causes of the Wealth of States - Arthur Laffer - 3000 |
#' | The Affluent Society - John Kenneth Galbraith - 5500 |
#' | The Limits of Morality - Amartya Sen - 6500 |
#' "
#' tidyair(messy_md)
#' }
#'
tidyair <- function(file) {

  is_df <- is.data.frame(file)
  if (is_df) {
    file <- OpenAIR::df_to_csv(file)
  }

  # import, process text
  r_function <- OpenAIR::read_text(file)
  text <-
    r_function$text %>%
    paste0(collapse = "\n")

  # Create user input
  n_msgs <- nrow(tidyair_prompt)
  tidyair_prompt$content[n_msgs] <-
    sprintf(fmt = tidyair_prompt$content[n_msgs], text)

  # Generate response output by chatting
  resp <- OpenAIR::chat_completion(tidyair_prompt)
  #total_tokens_used <- OpenAIR::usage(resp)$total_tokens
  #message("Total tokens used: ", total_tokens_used)

  # extract output
  output <-
    resp %>%
    OpenAIR::messages_content()

  # process output
  filename <- unique(r_function$file)

  if (filename == "character string") {
    # console/df input is returned as df
    output_df <- readr::read_csv(output)
    return(output_df)

  } else {
    # file name for csv file
    filename <- paste0(OpenAIR::replace_file_extension(filename, ""), "-tidy.csv")

    # parse and write csv
    output_df <- readr::read_csv(output)
    readr::write_csv(output_df, file = filename)
    message("CSV-file generated: ", filename)

    return(filename)
  }

}
