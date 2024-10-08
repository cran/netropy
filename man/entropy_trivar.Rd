% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entropy_trivar.R
\name{entropy_trivar}
\alias{entropy_trivar}
\title{Trivariate Entropy}
\usage{
entropy_trivar(dat)
}
\arguments{
\item{dat}{dataframe with rows as observations and columns as variables.
Variables must all be observed or transformed categorical with finite range spaces.}
}
\value{
Dataframe with the first three columns representing possible triples of variables (\code{V1,V2,V3})
and the fourth column gives trivariate entropies \code{H(V1,V2,V3)}.
}
\description{
Computes trivariate entropies of all triples of (discrete)
variables in a multivariate data set.
}
\details{
Trivariate entropies can be used to check for functional relationships and
stochastic independence between triples of variables.
The trivariate entropy \emph{H(X,Y,Z)} of three discrete random variables \emph{X, Y} and \emph{Z}
is bounded according to \cr

\emph{H(X,Y) <= H(X,Y,Z) <= H(X,Z) + H(Y,Z) - H(Z)}.
\cr

The increment between the trivariate entropy and its lower bound is equal to the expected conditional entropy.
}
\examples{
# use internal data set
data(lawdata)
df.att <- lawdata[[4]]

# three steps of data editing:
# 1. categorize variables 'years' and 'age' based on
# approximately three equally size groups (values based on cdf)
# 2. make sure all outcomes start from the value 0 (optional)
# 3. remove variable 'senior' as it consists of only unique values (thus redundant)
df.att.ed <- data.frame(
    status = df.att$status,
    gender = df.att$gender,
    office = df.att$office - 1,
    years = ifelse(df.att$years <= 3, 0,
        ifelse(df.att$years <= 13, 1, 2)
    ),
    age = ifelse(df.att$age <= 35, 0,
        ifelse(df.att$age <= 45, 1, 2)
    ),
    practice = df.att$practice,
    lawschool = df.att$lawschool - 1
)

# calculate trivariate entropies
H.triv <- entropy_trivar(df.att.ed)
}
\references{
Frank, O., & Shafie, T. (2016). Multivariate entropy analysis of network data.
\emph{Bulletin of Sociological Methodology/Bulletin de Méthodologie Sociologique}, 129(1), 45-63.
}
\seealso{
\code{\link{entropy_bivar}}, \code{\link{prediction_power}}
}
\author{
Termeh Shafie
}
