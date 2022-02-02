## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(out.width = "100%",
  cache = FALSE
)

## ----eval=TRUE----------------------------------------------------------------
library('netropy')

## ----load_data, eval=TRUE-----------------------------------------------------
data(lawdata) 
adj.advice <- lawdata[[1]]
adj.friend <- lawdata[[2]]
adj.cowork <-lawdata[[3]]
df.att <- lawdata[[4]]

## ----cdf, eval=TRUE, include=TRUE, results ='markup'--------------------------
# for years:
x <- table(df.att$years) 
values<-as.numeric(names(x))
prop.x <- round(prop.table(x),2)
cum.prop=cumsum(prop.x)
frq.years = data.frame(value=values,freq=as.vector(x),
                       rel.freq=as.vector(prop.x), 
                       cum.rel.freq=as.vector(cum.prop))

# for age:
x <- table(df.att$age) 
values<-as.numeric(names(x))
prop.x <- round(prop.table(x),2)
cum.prop=cumsum(prop.x)
frq.age = data.frame(value=values,freq=as.vector(x),
                       rel.freq=as.vector(prop.x), 
                       cum.rel.freq=as.vector(cum.prop))

## ----cdf2, eval=TRUE, include=TRUE, results ='markup'-------------------------
frq.years
frq.age

## ----edit_data1, eval=TRUE, include=TRUE, results ='markup'-------------------
att.var <-
  data.frame(
    status   = df.att$status-1,
    gender   = df.att$gender,
    office   = df.att$office-1,
    years    = ifelse(df.att$years <= 3,0,
                      ifelse(df.att$years <= 13,1,2)),
    age      = ifelse(df.att$age <= 35,0,
                      ifelse(df.att$age <= 45,1,2)),
    practice = df.att$practice,
    lawschool= df.att$lawschool-1
    )
head(att.var)

## ----edit_data2, eval=TRUE, include=TRUE, results ='markup'-------------------
dyad.status    <- get_dyad_var(att.var$status, type = 'att')
dyad.gender    <- get_dyad_var(att.var$gender, type = 'att')
dyad.office    <- get_dyad_var(att.var$office, type = 'att')
dyad.years     <- get_dyad_var(att.var$years, type = 'att')
dyad.age       <- get_dyad_var(att.var$age, type = 'att')
dyad.practice  <- get_dyad_var(att.var$practice, type = 'att')
dyad.lawschool <- get_dyad_var(att.var$lawschool, type = 'att')

## ----edit_data3, eval=TRUE, include=TRUE, message = FALSE, results ='markup'----
dyad.cwk    <- get_dyad_var(adj.cowork, type = 'tie')
dyad.adv    <- get_dyad_var(adj.advice, type = 'tie')
dyad.frn    <- get_dyad_var(adj.friend, type = 'tie')

## ----edit_data4, eval=TRUE, include=TRUE, results ='markup'-------------------
dyad.var <-
  data.frame(cbind(status   = dyad.status$var,
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
head(dyad.var)

## ----edit_data5, eval=TRUE, include=TRUE, results ='markup'-------------------
triad.status    <- get_triad_var(att.var$status, type = 'att')
triad.gender    <- get_triad_var(att.var$gender, type = 'att')
triad.office    <- get_triad_var(att.var$office, type = 'att')
triad.years     <- get_triad_var(att.var$years, type = 'att')
triad.age       <- get_triad_var(att.var$age, type = 'att')
triad.practice  <- get_triad_var(att.var$practice, type = 'att')
triad.lawschool <- get_triad_var(att.var$lawschool,type = 'att')

## ----edit_data6, eval=TRUE, include=TRUE, message = FALSE, results ='markup'----
triad.cwk    <- get_triad_var(adj.cowork, type = 'tie')
triad.adv    <- get_triad_var(adj.advice, type = 'tie')
triad.frn    <- get_triad_var(adj.friend, type = 'tie')

## ----edit_data7, eval=TRUE, include=TRUE, results ='markup'-------------------
triad.var <- data.frame(cbind(
             status    = triad.status$var,
             gender    = triad.gender$var,
             office    = triad.office$var,
             years     = triad.years$var,
             age       = triad.age$var,
             practice  = triad.practice$var,
             lawschool = triad.lawschool$var,
             cowork    = triad.cwk$var,
             advice    = triad.adv$var,
             friend    = triad.frn$var)
             )
head(triad.var, 5)

