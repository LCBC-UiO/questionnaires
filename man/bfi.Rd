% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bfi2-funcs.R
\name{bfi}
\alias{bfi}
\alias{bfi_compute}
\alias{bfi_compute_domains}
\alias{bfi_compute_facets}
\title{Big 5 Inventory}
\usage{
bfi_compute(
  data,
  type = c("domains", "facets"),
  keep_all = FALSE,
  prefix = "bfi_"
)

bfi_compute_domains(
  data,
  domains = c("extraversion", "agreeableness", "conscientiousness",
    "negative emotionality", "open-mindedness"),
  keep_all = FALSE,
  prefix = "domain_"
)

bfi_compute_facets(
  data,
  facets = c("sociability", "assertiveness", "energy", "compassion", "respectful",
    "trust", "organization", "productive", "responsibility", "anxiety", "depression",
    "emotional volatility", "intellectual curiosity", "aesthetic sensebility",
    "creative imagination"),
  keep_all = FALSE,
  prefix = "facet_"
)
}
\arguments{
\item{data}{data.frame containing bfi data}

\item{type}{Choose domains or facets. Default is both}

\item{keep_all}{logical, append to data.frame}

\item{prefix}{string to prefix column names of computed values}

\item{domains}{string vector of domains to compute}

\item{facets}{string vector of facets to compute}
}
\value{
data.frame with calculated scores
}
\description{
{
The BFI-2 is a measure of the Big Five personality domains (which we label Extraversion, Agreeableness, Conscientiousness, Negative Emotionality, and Open-Mindedness) and 15 more-specific facet traits.
The Big Five personality traits was the model to comprehend the relationship between personality and academic behaviors.
This model was defined by several independent sets of researchers who used factor analysis of verbal descriptors of human behavior.
These researchers began by studying relationships between a large number of verbal descriptors related to personality traits.
They reduced the lists of these descriptors by 5–10 fold and then used factor analysis to group the remaining traits (using data mostly based upon people's estimations, in self-report questionnaire and peer ratings) in order to find the underlying factors of personality

Item numbers for the BFI-2 domain and facet scales are listed below. Reverse-keyed items are denoted by “R.”
For more information about the BFI-2, visit the Colby Personality Lab website
(http://www.colby.edu/psych/personality-lab/).
\subsection{Domain Scales}{

\strong{Extraversion:} 1, 6, 11R, 16R, 21, 26R, 31R, 36R, 41, 46, 51R, 56
\strong{Agreeableness:} 2, 7, 12R, 17R, 22R, 27, 32, 37R, 42R, 47R, 52, 57
\strong{Conscientiousness:} 3R, 8R, 13, 18, 23R, 28R, 33, 38, 43, 48R, 53, 58R
\strong{Negative Emotionality:} 4R, 9R, 14, 19, 24R, 29R, 34, 39, 44R, 49R, 54, 59
\strong{Open-Mindedness:} 5R, 10, 15, 20, 25R, 30R, 35, 40, 45R, 50R, 55R, 60
}

\subsection{Facet Scales}{

\strong{Sociability:} 1, 16R, 31R, 46
\strong{Assertiveness:} 6, 21, 36R, 51R
\strong{Energy Level:} 11R, 26R, 41, 56
\strong{Compassion:} 2, 17R, 32, 47R
\strong{Respectfulness:} 7, 22R, 37R, 52
\strong{Trust:} 12R, 27, 42R, 57
\strong{Organization:} 3R, 18, 33, 48R
\strong{Productiveness:} 8R, 23R, 38, 53
\strong{Responsibility:} 13, 28R, 43, 58R
\strong{Anxiety:} 4R, 19, 34, 49R
\strong{Depression:} 9R, 24R, 39, 54
\strong{Emotional Volatility:} 14, 29R, 44R, 59
\strong{Intellectual Curiosity:} 10, 25R, 40, 55R
\strong{Aesthetic Sensitivity:} 5R, 20, 35, 50R
\strong{Creative Imagination:} 15, 30R, 45R, 60
}

\subsection{Relational table}{\tabular{lll}{
   Domain \tab Factor-pure facet \tab Complementary facets \cr
   E \tab Sociability \tab Assertiveness, Energy Level \cr
   A \tab Compassion \tab Respectfulness, Trust \cr
   C \tab Organization \tab Productiveness, Responsibility \cr
   N \tab Anxiety \tab Depression, Emotional Volatility \cr
   O \tab Aesthetic Sensitivity \tab Intellectual Curiosity, Creative Imagination \cr
}

}

\subsection{Data requirements}{
\subsection{Column names}{

The package functions expect the data to be named in a specific way, and to not contain data other than the BFI-2 data.
Column names should be zero-leading two digits to indicate the question number, and they should end with these two digits.
If this system is followed, then all functions work out of the box.

Examples that work:
\itemize{
\item \code{bfi_01} \code{bfi_02} ... \code{bfi_59} \code{bfi_60}
\item \code{big_five_01} \code{big_five_02} ... \code{big_five_59} \code{bbig_five_60}
}

Examples that won't work
\itemize{
\item \code{bfi_1} \code{bfi_2} ... \code{bfi_59} \code{bfi_60}
\item \code{big_five_01_trust} \code{big_five_02_change} ... \code{big_five_59_test} \code{bbig_five_60_lat}
}
}

\subsection{Data values}{

The data should be coded with the original scoring system 1-5.
The data should \strong{not} have implemented necessary reversal of answers for any of the questions, the functions will take care of this.
}

}

\subsection{References}{

Soto, C. J., & John, O. P. (2017). The next Big Five Inventory (BFI-2): Developing and assessing a hierarchical model with 15 facets to enhance bandwidth, fidelity, and predictive power. \emph{Journal of Personality and Social Psychology}, \strong{113}(1), 117–143. \url{https://doi.org/10.1037/pspp0000096}

}
}
}
\section{Functions}{
\itemize{
\item \code{bfi_compute_domains()}: Compute BFI-2 domains and return in a data.frame

\item \code{bfi_compute_facets()}: Compute BFI-2 domains and return in a data.frame

}}
\examples{
library(dplyr)
# Making some test data
test_data <- tibble(
  id = rep(1:10, each = 60),
  name = rep(sprintf("bfi_\%02d", 1:60), 10),
  value = lapply(1:10, function(x){
    sample(1:5, size = 60, replace = TRUE)
  }) \%>\% unlist()
) \%>\% 
  tidyr::pivot_wider()
  
bfi_compute(test_data)
bfi_compute(test_data, prefix = "bfi_")
}
