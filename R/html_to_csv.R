#' Convert HTML Table Elements to CSV
#'
#' This function takes a file with HTML input and processes it to generate
#' corresponding output in CSV format. If the input is a character string,
#' the output will be printed to the console. Otherwise, a CSV file will be
#' generated with the same name as the input file but with a .csv extension.
#'
#' @param file A character string representing the path to the file with HTML input.
#' @param n_tokens_limit An integer representing the maximum number of tokens allowed in the input text. Defaults to 2000.
#' @param ... Additional arguments passed down to lower-level functions.
#'
#' @return If the input is a character string, the function returns the output
#' as a character string and prints it to the console. Otherwise, the function returns
#' the output as a data frame and generates a CSV file with the same name as the input file
#' but with a .csv extension.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: HTML string
#' html_string <- '<table>
#'  <tr>
#'    <th>firstName</th>
#'    <th>lastName</th>
#'  </tr>
#'  <tr>
#'    <td>John</td>
#'    <td>Doe</td>
#'  </tr>
#'  <tr>
#'    <td>Anna</td>
#'    <td>Smith</td>
#'  </tr>
#'  <tr>
#'    <td>Peter</td>
#'    <td>Jones</td>
#'  </tr>
#'</table>'
#' html_to_csv(html_string)
#'
#' # Example 2: HTML file
#' html_file <- "example.html"
#' html_to_csv(html_file)
#' }
#'

html_to_csv <- function(file, n_tokens_limit=2000, ...) {

  # import, process text
  r_function <- TheOpenAIR::read_text(file)
  text <-
    r_function$text %>%
    paste0(collapse = "\n")

  # Check validity of input
  if (unique(r_function$file)=="character string") {
    if (!TheOpenAIR::is_xml(text)){
      stop("No valid HTML/XML string provided!")
    }
  }

  # extract html tables as one character string
  text <-
    rvest::read_html(text) %>%
    rvest::html_nodes(xpath=".//table") %>%
    as.character() %>%
    paste0(collapse = "\n")

  # Make sure the text input is not too long
  n_tokens <- count_tokens(text)
  if (n_tokens_limit < n_tokens) {
    stop("Text input contains too many tokens!")
  }

  # Create user input
  n_msgs <- nrow(html_to_csv_prompt)
  html_to_csv_prompt$content[n_msgs] <-
    sprintf(fmt = html_to_csv_prompt$content[n_msgs], text)

  # Generate response output by chatting
  resp <- TheOpenAIR::chat_completion(html_to_csv_prompt, ...)


  # extract output
  output <-
    resp %>%
    TheOpenAIR::messages_content()

  # Check if filename is a character string
  # process output
  filename <- unique(r_function$file)

  if (filename == "character string") {
    return(output)

  } else {
    # file name for csv file
    filename <- TheOpenAIR::replace_file_extension(filename, ".csv")
    # parse and write csv
    output_df <- readr::read_csv(output)
    readr::write_csv(output_df, file = filename)
    cli::cli_alert_success(paste0("CSV-file generated: ", filename))

    return(output_df)

  }

}
