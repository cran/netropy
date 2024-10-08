% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/div_gof.R
\name{div_gof}
\alias{div_gof}
\title{Divergence Tests of Goodness of Fit}
\usage{
div_gof(dat, var1, var2, var_cond = NULL)
}
\arguments{
\item{dat}{dataframe with rows as observations and columns as variables.
Variables must all be observed or transformed categorical with finite range spaces.}

\item{var1}{variable name as character in \code{dat} specified to be independent to
those specified in \code{var2} under \emph{p0}}

\item{var2}{variable name as character in \code{dat} specified to be independent to
those specified in \code{var1}}

\item{var_cond}{character of variable name in \code{dat} to condition the independence specification on,
must be different variables than those specified in \code{var1} and \code{var2}).
Default = empty (no conditioning).}
}
\value{
Message indicating whether the hypothesis with the specified independence model
can be rejected or not on approximately 5\% level of significance.
\item{summary}{Dataframe including the value of the divergence \emph{D} and its degrees of freedom}
}
\description{
Tests of various hypothetical structural models \emph{p0} against
the general model \emph{p} which is estimated using empirical data.
}
\details{
this function is currently implemented to only test goodness of fit of models specified as
\emph{X} independent of \emph{Y}, or \emph{X} independent of \emph{Y} given \emph{Z}.
}
\examples{
# Data editing and creation of dyad variables:
data(lawdata)
adj.advice <- lawdata[[1]]
adj.friend <- lawdata[[2]]
adj.cowork <-lawdata[[3]]
df.att <- lawdata[[4]]
att.var <-
data.frame(
 status   = df.att$status-1,
 gender   = df.att$gender,
 office   = df.att$office-1,
 years    = ifelse(df.att$years<=3,0,
            ifelse(df.att$years<=13,1,2)),
 age  = ifelse(df.att$age<=35,0,
        ifelse(df.att$age<=45,1,2)),
 practice = df.att$practice,
 lawschool= df.att$lawschool-1)
dyad.status    <- get_dyad_var(att.var$status, type = 'att')
dyad.gender    <- get_dyad_var(att.var$gender, type = 'att')
dyad.office    <- get_dyad_var(att.var$office, type = 'att')
dyad.years     <- get_dyad_var(att.var$years, type = 'att')
dyad.age       <- get_dyad_var(att.var$age, type = 'att')
dyad.practice  <- get_dyad_var(att.var$practice, type = 'att')
dyad.lawschool <- get_dyad_var(att.var$lawschool, type = 'att')
dyad.cwk    <- get_dyad_var(adj.cowork, type = 'tie')
dyad.adv    <- get_dyad_var(adj.advice, type = 'tie')
dyad.frn    <- get_dyad_var(adj.friend, type = 'tie')
dyad.var <-
data.frame(cbind(status    = dyad.status$var,
                 gender    = dyad.gender$var,
                 office    = dyad.office$var,
                 years     = dyad.years$var,
                 age       = dyad.age$var,
                 practice  = dyad.practice$var,
                 lawschool = dyad.lawschool$var,
                 cowork    = dyad.cwk$var,
                 advice    = dyad.adv$var,
                 friend    = dyad.frn$var)
                 )
# To test whether friend is independent of cowork given advice:
div_gof(dat = dyad.var, var1 = "friend", var2 = "cowork", var_cond = "advice")
}
\references{
Frank, O., & Shafie, T. (2016). Multivariate entropy analysis of network data.
\emph{Bulletin of Sociological Methodology/Bulletin de Méthodologie Sociologique}, 129(1), 45-63.
}
\seealso{
\code{\link{joint_entropy}},   \code{\link{assoc_graph}},  \code{\link{entropy_trivar}}
}
\author{
Termeh Shafie
}
