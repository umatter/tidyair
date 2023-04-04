#' Convert JSON input to CSV
#'
#' This function takes a file with JSON input and processes it to generate
#' corresponding output in CSV format. If the input is a character string,
#' the output will be printed to the console. Otherwise, a CSV file will be
#' generated with the same name as the input file but with a .csv extension.
#'
#' @param file A character string representing the path to the file with JSON input.
#' @param n_tokens_limit An integer representing the maximum number of tokens allowed in the input text.
#' Defaults to 2000.
#' @param ... Additional arguments passed down to lower-level functions.
#'
#' @return If the input is a character string, the function returns the output
#' as a character string and prints it to the console. Otherwise, the function returns
#' the output as a data frame and generates a CSV file with the same name as the input file
#' but with a .csv extension.
#'
#' @examples
#' # Example: JSON string
#' json_string <- '{"employees":[
#' { "firstName":"John", "lastName":"Doe" },
#' { "firstName":"Anna", "lastName":"Smith" },
#' { "firstName":"Peter", "lastName":"Jones" }
#' ]}'
#' json_to_csv(json_string)
#'
#' @export
json_to_csv <- function(file, n_tokens_limit=2000, ...) {

  # import, process text
  r_function <- OpenAIR::read_text(file)
  text <-
    r_function$text %>%
    paste0(collapse = "\n")

  # validate input string
  if (unique(r_function$file)=="character string") {
    if (!OpenAIR::is_json(text)){
      stop("No valid JSON string provided!")
    }
  }

  # Make sure the text input is not too long
  n_tokens <- count_tokens(text)
  if (n_tokens_limit < n_tokens) {
    stop("Text input contains too many tokens!")
  }


  # Create user input
  n_msgs <- nrow(json_to_csv_prompt)
  json_to_csv_prompt$content[n_msgs] <-
    sprintf(fmt = json_to_csv_prompt$content[n_msgs], text)

  # Generate response output by chatting
  resp <- OpenAIR::chat_completion(json_to_csv_prompt, ...)
  #total_tokens_used <- OpenAIR::usage(resp)$total_tokens
  #message("Total tokens used: ", total_tokens_used)

  # extract output
  output <-
    resp %>%
    OpenAIR::messages_content()

  # Check if filename is a character string
  # process output
  filename <- unique(r_function$file)

  if (filename == "character string") {
    message(output)
    return(output)

  } else {
    # file name for csv file
    filename <- OpenAIR::replace_file_extension(filename, ".csv")
    # parse and write csv
    output_df <- readr::read_csv(output)
    readr::write_csv(output_df, file = filename)
    message("CSV-file generated: ", filename)
    return(output_df)

  }

}
