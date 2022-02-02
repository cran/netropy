## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(out.width = "100%",
  cache = FALSE
)

## ----load_library, eval=TRUE--------------------------------------------------
library(netropy)

## ----load_data, eval=TRUE, include=FALSE--------------------------------------
data(lawdata) 
adj.advice <- lawdata[[1]]
adj.friend <- lawdata[[2]]
adj.cowork <-lawdata[[3]]
df.att <- lawdata[[4]]

## ----edit_data, eval=TRUE, include=FALSE, results ='hide'---------------------
att.var <-
  data.frame(
    status   = df.att$status-1,
    gender   = df.att$gender,
    office   = df.att$office-1,
    years    = ifelse(df.att$years<=3,0,
                      ifelse(df.att$years<=13,1,2)),
    age      = ifelse(df.att$age<=35,0,
                      ifelse(df.att$age<=45,1,2)),
    practice = df.att$practice,
    lawschool= df.att$lawschool-1
    )
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

## ----show_data, eval=TRUE, include=TRUE, results ='markup'--------------------
head(dyad.var)

## ----biv_ent, eval=TRUE, include=TRUE, results ='markup'----------------------
entropy_bivar(dyad.var)

## ----red, eval=TRUE, include=TRUE, results ='markup'--------------------------
redundancy(dyad.var)

## ----edit_data2, eval=TRUE, include=FALSE, results ='hide'--------------------
att.var <-
  data.frame(
    senior   = df.att$senior,
    status   = df.att$status-1,
    gender   = df.att$gender,
    office   = df.att$office-1,
    years    = ifelse(df.att$years<=3,0,
                      ifelse(df.att$years<=13,1,2)),
    age      = ifelse(df.att$age<=35,0,
                      ifelse(df.att$age<=45,1,2)),
    practice = df.att$practice,
    lawschool= df.att$lawschool-1
    )

## ----show_data2, eval=TRUE, include=TRUE, results ='markup'-------------------
head(att.var)

## ----red2, eval=TRUE, include=TRUE, results ='markup'-------------------------
redundancy(att.var)

## ----biv_ent2, eval=TRUE, include=TRUE, results ='markup'---------------------
entropy_bivar(att.var)

## ----triv_ent, eval=TRUE, include=TRUE, results ='markup'---------------------
entropy_trivar(dyad.var)

