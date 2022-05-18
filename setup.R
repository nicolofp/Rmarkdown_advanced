# Setup enviroment:
# - Identify the directory
# - Create two main folders for thereport (.Rmd and .html files)

directory = getwd()
dir.create(path = paste0(directory,"/rmd_report"))
dir.create(path = paste0(directory,"/html_report"))
