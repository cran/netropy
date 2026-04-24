## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(out.width = "100%", cache = FALSE)

## ----load_library, eval=TRUE--------------------------------------------------
library(netropy)

## ----load_data, eval=TRUE, include=FALSE--------------------------------------
data(lawdata)
adj_advice <- lawdata[[1]]
adj_friend <- lawdata[[2]]
adj_cowork <- lawdata[[3]]
df_att <- lawdata[[4]]

## ----edit_data, eval=TRUE, include=FALSE, results ='hide'---------------------
att_var <- data.frame(
  status    = df_att$status - 1,
  gender    = df_att$gender,
  office    = df_att$office - 1,
  years     = ifelse(df_att$years <= 3, 0,
                ifelse(df_att$years <= 13, 1, 2)),
  age       = ifelse(df_att$age <= 35, 0,
                ifelse(df_att$age <= 45, 1, 2)),
  practice  = df_att$practice,
  lawschool = df_att$lawschool - 1
)

dyad_status    <- get_dyad_var(att_var$status, type = "att")
dyad_gender    <- get_dyad_var(att_var$gender, type = "att")
dyad_office    <- get_dyad_var(att_var$office, type = "att")
dyad_years     <- get_dyad_var(att_var$years, type = "att")
dyad_age       <- get_dyad_var(att_var$age, type = "att")
dyad_practice  <- get_dyad_var(att_var$practice, type = "att")
dyad_lawschool <- get_dyad_var(att_var$lawschool, type = "att")

dyad_cwk <- get_dyad_var(adj_cowork, type = "tie")
dyad_adv <- get_dyad_var(adj_advice, type = "tie")
dyad_frn <- get_dyad_var(adj_friend, type = "tie")

dyad_var <- data.frame(
  status    = dyad_status$var,
  gender    = dyad_gender$var,
  office    = dyad_office$var,
  years     = dyad_years$var,
  age       = dyad_age$var,
  practice  = dyad_practice$var,
  lawschool = dyad_lawschool$var,
  cowork    = dyad_cwk$var,
  advice    = dyad_adv$var,
  friend    = dyad_frn$var
)

## ----show_data, eval=TRUE, include=TRUE---------------------------------------
head(dyad_var)

## ----cond_ind-----------------------------------------------------------------
div_gof(
  dat = dyad_var,
  var1 = "friend",
  var2 = "cowork",
  var_cond = "advice"
)

## ----pair_ind-----------------------------------------------------------------
div_gof(
  dat = dyad_var,
  var1 = "friend",
  var2 = "cowork"
)

## ----unif---------------------------------------------------------------------
div_gof(
  dat = dyad_var,
  var_uniform = "friend"
)

## ----nest_ind1----------------------------------------------------------------
m_full <- list(D = 0, df = 0)

m_reduced <- div_gof(
  dat = dyad_var,
  var1 = "friend",
  var2 = "cowork"
)

div_gof(
  dat = dyad_var,
  model_full = m_full,
  model_reduced = list(D = m_reduced$D, df = m_reduced$df)
)

## ----nest_ind2----------------------------------------------------------------
m_reduced <- div_gof(
  dat = dyad_var,
  var1 = "friend",
  var2 = "cowork",
  var_cond = "advice"
)

div_gof(
  dat = dyad_var,
  model_full = m_full,
  model_reduced = list(D = m_reduced$D, df = m_reduced$df)
)

