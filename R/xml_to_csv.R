#' Convert XML input to CSV
#'
#' This function takes a file with XML input and processes it to generate
#' corresponding output in CSV format. If the input is a character string,
#' the output will be printed to the console. Otherwise, a CSV file will be
#' generated with the same name as the input file but with a .csv extension.
#'
#' @param file A character string representing the path to the file with XML input.
#' @param n_tokens_limit An integer representing the maximum number of tokens allowed in the input text. Defaults to 2000.
#' @param ... Additional arguments passed down to lower-level functions.
#'
#' @return If the input is a character string, the function returns the output
#' as a character string and prints it to the console. Otherwise, the function returns
#' the output as a data frame and generates a CSV file with the same name as the input file
#' but with a .csv extension.
#'
#' @examples
#' \dontrun{
#' # Example 1: XML string
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
#'
#' # Example 2: XML file
#' # create temporary file
#' temp_file <- tempfile(fileext = ".xml")
#'
#' # write XML content to temporary file
#' cat('<?xml version="1.0"?>
#' <employees>
#'   <employee>
#'     <firstName>John</firstName>
#'     <lastName>Doe</lastName>
#'   </employee>
#'   <employee>
#'     <firstName>Anna</firstName>
#'     <lastName>Smith</lastName>
#'   </employee>
#'   <employee>
#'     <firstName>Peter</firstName>
#'     <lastName>Jones</lastName>
#'   </employee>
#' </employees>', file = temp_file)
#'
#' # call xml_to_csv function with temporary file
#' xml_to_csv(temp_file)
#'
#' # remove temporary file
#' file.remove(temp_file)
#'}
#' @export
xml_to_csv <- function(file, n_tokens_limit = 2000, ...) {

  # import, process text
  r_function <- TheOpenAIR::read_text(file)
  text <-
    r_function$text %>%
    paste0(collapse = "\n")

  # check validity of input text
  if (unique(r_function$file)=="character string") {
    if (!TheOpenAIR::is_xml(text)){
      stop("No valid XML string provided!")
    }
  }

  # Make sure the text input is not too long
  n_tokens <- count_tokens(text)
  if (n_tokens_limit < n_tokens) {
    stop("Text input contains too many tokens!")
  }

  # Create user input
  n_msgs <- nrow(xml_to_csv_prompt)
  xml_to_csv_prompt$content[n_msgs] <-
    sprintf(fmt = xml_to_csv_prompt$content[n_msgs], text)

  # Generate response output by chatting
  resp <- TheOpenAIR::chat_completion(xml_to_csv_prompt, ...)

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
