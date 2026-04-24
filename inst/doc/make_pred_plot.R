## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(out.width = "100%", cache = FALSE)

## ----load-lib-----------------------------------------------------------------
library(netropy)

## ----data-edit----------------------------------------------------------------
df_att <- lawdata[[4]]
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

## -----------------------------------------------------------------------------
head(att_var)

## ----pred-pow-----------------------------------------------------------------
pred_status <- prediction_power("status", att_var)
pred_status

## ----pred-plot, fig.height=7, fig.width=8-------------------------------------
make_pred_plot(pred_status, "Prediction Power for Status")

## ----col-change, fig.height=7, fig.width=8------------------------------------
make_pred_plot(
  pred_status,
  "Prediction Power for Status",
  low = "steelblue",
  high = "white"
)

## ----text-change, fig.height=7, fig.width=8-----------------------------------
make_pred_plot(
  pred_status,
  "Prediction Power for Status",
  text_size = 6
)

