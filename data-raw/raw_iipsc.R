library(haven)
library(tidyverse)
dat <- read_sav("FA09 Data ABCT.sav") %>%
  select(IIP01:IIP32) %>%
  as.matrix() %>%
  as.tibble()
ind <- sample(1:nrow(dat), 10)
raw_iipsc <- dat[ind, ]
use_data(raw_iipsc, internal = FALSE, overwrite = TRUE)
