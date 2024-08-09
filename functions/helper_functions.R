# Helper functions for UI

# BCBS Logo - Sidebar
bcbsLogo <- function() {
  HTML(paste0(
    "<br>",
    "<a href='https://www.bcbs.com/' target='_blank'><img style = 'display: block; margin-left: 50px; margin-right: auto; margin-top: 0px; margin-bottom: -10px;' src='logo.svg' width = '186'></a>",
    "<br>"
  ))
}

# BCBS Footer - Sidebar 
bcbsFooter <- function() {
  HTML(
    HTML(paste0(
      "<script>",
      "var today = new Date();",
      "var yyyy = today.getFullYear();",
      "</script>",
      "<p style = 'text-align: center; color: #a7a9ac; position: fixed; bottom: 0; padding: 10px; left: 90px;'><small>&copy; - <a href='https://www.bcbs.com/' target='_blank'>bcbs.com</a> - <script>document.write(yyyy);</script></small></p>")
    ))
}

# Custom BCBS Box
bcbsBox <- function(...) {
  div(box(...), class = "bcbs-box")
}

# Custom BCBS Tab Box
bcbsTabBox <- function(...) {
  div(tabBox(...), class = "bcbs-box")
}

# Custom BCBS Button
bcbsButton <- function(...) {
  div(actionBttn(..., style = "simple", color = "primary"), class = "bcbs-button")
}


# Round entire dataframe - all numeric columns to 'x' number of digits
round_df <- function(x, digits) {
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
