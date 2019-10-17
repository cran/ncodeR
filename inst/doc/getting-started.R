## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(ncodeR)

# Individual Pieces
data(RS.data)
rs_text = RS.data$text

## ------------------------------------------------------------------------
newcode = create.code(name = "Data", expressions = c("number","data"), excerpts = rs_text)

## ------------------------------------------------------------------------
# newcode.h = handcode(code = newcode, excerpts = rs_text, n = 4)

## ------------------------------------------------------------------------
# newcode.t = test(code = newcode.h, kappaThreshold = 0.65)

