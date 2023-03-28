# Check if the API key is available
if (!nchar(Sys.getenv("OPENAI_API_KEY")) == 0) {

  # Test file for html_to_csv function

  # Begin the test
  test_that("html_to_csv function works correctly", {
    # Test that function returns output for valid input
    test_that("html_to_csv returns output for valid input", {
      # Create a test file
      test_file <- tempfile(fileext = ".html")
      write('<table>
            <tr>
              <th>firstName</th>
              <th>lastName</th>
            </tr>
            <tr>
              <td>John</td>
              <td>Doe</td>
            </tr>
            <tr>
              <td>Anna</td>
              <td>Smith</td>
            </tr>
            <tr>
              <td>Peter</td>
              <td>Jones</td>
            </tr>
          </table>', test_file)

      # Test function
      result <- html_to_csv(test_file)

      # Check result
      expect_true(is.data.frame(result))
      expect_true(file.exists(OpenAIR::replace_file_extension(test_file, ".csv")))
    })

    # Test that function prints output for valid input as character string
    test_that("html_to_csv prints output for valid input as character string", {
      # Create a test string
      test_string <-
        test_string <- '<table>
                      <tr>
                        <th>firstName</th>
                        <th>lastName</th>
                      </tr>
                      <tr>
                        <td>John</td>
                        <td>Doe</td>
                      </tr>
                      <tr>
                        <td>Anna</td>
                        <td>Smith</td>
                      </tr>
                      <tr>
                        <td>Peter</td>
                        <td>Jones</td>
                      </tr>
                    </table>'

      # Capture console output
      capture_output(result <- html_to_csv(test_string))

      # Check result
      expect_type(result, "character")
    })

    # Test that function throws an error for invalid input
    test_that("html_to_csv throws an error for invalid input", {
      # Test function with invalid input
      expect_error(html_to_csv("nonexistent_file.html"))
    })
  })


} else {
  test_that("html_to_csv skips test if API key not set", {
    skip("API key not set, skipping test.")
  })
}

