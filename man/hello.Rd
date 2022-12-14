% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Funs.R
\name{hello}
\alias{hello}
\title{Hello World}
\usage{
hello(who, lang = "EN", LangData = Hello::language)
}
\arguments{
\item{who}{a \code{character} vector of length 1 thay specifies the name of the person to whom the message is addressed.}

\item{lang}{a \code{character} vector of length 1 thay specifies the preferred language. Default to "EN" for English. Other possible values...}

\item{LangData}{an optional data.frame with two columns each of mode \code{character}. The first column gives the language codes and the second column
gives the corresponding "hello" word. Default to \code{language}.

see \code{?language}}
}
\value{
a \code{character} vector with a personalized \emph{"hello"} message.
}
\description{
\code{hello} says \emph{"hello"} in the user-specified language
}
\examples{
# `hello`("James")
# `hello`("Amelia", "Es")

}
