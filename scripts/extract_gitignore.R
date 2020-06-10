install.packages('gitignore')
library(gitignore)

head(gi_available_templates(), 25)
gi_fetch_templates("R")

