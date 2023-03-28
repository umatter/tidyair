#' Convert HTML input to CSV
#'
#' This function takes a file with HTML input and processes it to generate
#' corresponding output in CSV format. If the input is a character string,
#' the output will be printed to the console. Otherwise, a CSV file will be
#' generated with the same name as the input file but with a .csv extension.
#'
#' @param file A character string representing the path to the file with HTML input
#'
#' @return If the input is a character string, the function returns the output
#' as a character string and prints it to the console. Otherwise, the function returns
#' the output as a data frame and generates a CSV file with the same name as the input file
#' but with a .csv extension.
#' @export
#' @examples
#' \dontrun{
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
#' }

html_to_csv <- function(file) {

  # import, process text
  r_function <- OpenAIR::read_text(file)
  text <-
    r_function$text %>%
    paste0(collapse = "\n")

  if (unique(r_function$file)=="character string") {
    if (!OpenAIR::is_xml(text)){
      stop("No valid XML string provided!")
    }
  }

  # Create user input
  n_msgs <- nrow(html_to_csv_prompt)
  html_to_csv_prompt$content[n_msgs] <-
    sprintf(fmt = html_to_csv_prompt$content[n_msgs], text)

  # Generate response output by chatting
  resp <- OpenAIR::chat_completion(html_to_csv_prompt)
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
