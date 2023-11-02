# Check if the API key is available
if (!nchar(Sys.getenv("OPENAI_API_KEY")) == 0) {

  # Begin the test
  test_that("xml_to_csv function works correctly", {
    # Test that function returns output for valid input
    test_that("xml_to_csv returns output for valid input", {
      # Create a test file
      test_file <- tempfile(fileext = ".xml")
      write('<?xml version="1.0"?><employees><employee><firstName>John</firstName><lastName>Doe</lastName></employee><employee><firstName>Anna</firstName><lastName>Smith</lastName></employee><employee><firstName>Peter</firstName><lastName>Jones</lastName></employee></employees>', test_file)

      # Test function
      result <- xml_to_csv(test_file)

      # Check result
      expect_true(is.data.frame(result))
      expect_true(file.exists(TheOpenAIR::replace_file_extension(test_file, ".csv")))
    })

    # Test that function prints output for valid input as character string
    test_that("xml_to_csv prints output for valid input as character string", {
      # Create a test string
      test_string <- '<?xml version="1.0"?><employees><employee><firstName>John</firstName><lastName>Doe</lastName></employee><employee><firstName>Anna</firstName><lastName>Smith</lastName></employee><employee><firstName>Peter</firstName><lastName>Jones</lastName></employee></employees>'

      # Capture console output
      capture_output(result <- xml_to_csv(test_string))

      # Check result
      expect_type(result, "character")
    })

    # Test that function throws an error for invalid input
    test_that("xml_to_csv throws an error for invalid input", {
      # Test function with invalid input
      expect_error(xml_to_csv("nonexistent_file.xml"))
    })
  })

} else {
  test_that("xml_to_csv skips test if API key not set", {
    skip("API key not set, skipping test.")
  })
}
