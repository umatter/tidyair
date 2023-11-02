
# tidyair <a href="https://umatter.github.io/tidyair/"><img src="man/figures/logo.png" align="right" height="139" /></a>


<!-- badges: start -->
 [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/umatter/tidyair/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/umatter/tidyair/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Tidy Data With Large Language Models.

## Installation

You can install the development version of tidyair from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("umatter/tidyair")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# load package
library(tidyair)

# set your OpenAI API key
TheOpenAIR::openai_api_key("YOUR-KEY-HERE")

 # Create a messy data.frame with air data
 messy_data <- data.frame(
   date_pm25 = c("2021-01-01|10", "2021-01-02|12", "2021-01-03|15"),
   date_pm10 = c("2021-01-01|20", "2021-01-02|25", "2021-01-03|30"),
   stringsAsFactors = FALSE
 )

 # Process the data.frame using tidyair
 tidied_data <- tidyair(air_data)
 print(tidied_data)
```

