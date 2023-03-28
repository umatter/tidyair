#' Convert XML input to CSV
#'
#' This function takes a file with XML input and processes it to generate
#' corresponding output in CSV format. If the input is a character string,
#' the output will be printed to the console. Otherwise, a CSV file will be
#' generated with the same name as the input file but with a .csv extension.
#'
#' @param file A character string representing the path to the file with XML input
#'
#' @return If the input is a character string, the function returns the output
#' as a character string and prints it to the console. Otherwise, the function returns
#' the output as a data frame and generates a CSV file with the same name as the input file
#' but with a .csv extension.
#' @export
#' @examples
#' \dontrun{
#' xml_string <- '<?xml version="1.0"?>
#'<employees>
#'  <employee>
#'    <firstName>John</firstName>
#'    <lastName>Doe</lastName>
#'  </employee>
#'  <employee>
#'    <firstName>Anna</firstName>
#'    <lastName>Smith</lastName>
#'  </employee>
#'  <employee>
#'    <firstName>Peter</firstName>
#'    <lastName>Jones</lastName>
#'  </employee>
#'</employees>'
#' xml_to_csv(xml_string)
#' }
xml_to_csv <- function(file) {

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
  n_msgs <- nrow(xml_to_csv_prompt)
  xml_to_csv_prompt$content[n_msgs] <-
    sprintf(fmt = xml_to_csv_prompt$content[n_msgs], text)

  # Generate response output by chatting
  resp <- OpenAIR::chat_completion(xml_to_csv_prompt)
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
