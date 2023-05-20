pkgload::load_all(".")
options(shiny.reactlog = TRUE)
Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/quarto/bin/tools/pandoc")
#reactlog::reactlog_enable()
#openUI(db_path = "/home/scopinho/github/sdanalytics/inst/extdata")
options(arrow.pull_as_vector=TRUE)
openUI()

