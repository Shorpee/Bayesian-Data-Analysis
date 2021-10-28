library(psych)
data("Aids2", package="MASS")
set.seed(15)
Aids2_sub <- Aids2 %>% sample_n(10)
Aids2_sub
pairs.panels(Aids2_sub, ellipses= FALSE)
