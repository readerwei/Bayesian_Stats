install.packages('gitignore')
library(gitignore)

head(gi_available_templates(), 25)
gi_available_templates()
gi_fetch_templates("R")
gi_fetch_templates(c("python","jupyternotebooks"))

