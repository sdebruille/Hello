#' Hello World
#'
#' `hello` says _"hello"_ in the user-specified language
#'
#' @return a `character` vector with a personalized _"hello"_ message.
#'
#' @param
#' who a `character` vector of length 1 thay specifies the name of the person to whom the message is addressed.
#' @param
#' lang a `character` vector of length 1 thay specifies the preferred language. Default to "EN" for English. Other possible values...
#' @param
#' LangData an optional data.frame with two columns each of mode `character`. The first column gives the language codes and the second column
#' gives the corresponding "hello" word. Default to `language`.
#'
#' see `?language`
#'
#' @examples
#' # `hello`("James")
#' # `hello`("Amelia", "Es")
#'
#' @import stringr
#'
#' @export

hello <- function(who, lang = "EN", LangData = Hello::language) {
  if (!exists("who", mode = "character") | length(who) > 1) {
    stop("Please enter a valid name; see ?hello")
  }

  LangData <- data.frame(LangData)

  if (ncol(LangData) > 2) {
    stop("Please enter a valid language data set; see ?hello")
  }

  colnames(LangData) <- c("code", "hello")

  if ((mode(LangData$code) != "character") | (mode(LangData$hello) != "character")) {
    stop("Please enter a valid language data set; see ?hello")
  }

  llang <- tolower(lang)

  hello <- subset(LangData, LangData$code == llang)[[2]]

  ifelse(
    length(hello) == 1,
    str_c(hello, ", ", who, "!", sep = ""),
    paste0("Sorry, ", who, ", ", "your language ", "('", lang, "') ", "is not available!", sep = "")
  ) |> cat()
}
