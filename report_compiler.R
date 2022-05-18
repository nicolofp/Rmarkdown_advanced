library(textutils)
library(ggplot2)
library(data.table)
library(xts)
library(rvest)
library(magrittr)
library(portfolioBacktest)
library(dplyr)

directory = getwd()

# The object of the "use_glue_master" should have the same name that 
# the object that we want to fill inside the "master_stock_report.Rmd"
use_glue_master = function(title, long_text){
  raw_rmd = readLines(paste0(directory,"/master_stock_report.Rmd"))
  filled_rmd = whisker::whisker.render(raw_rmd)
  writeLines(
    text = filled_rmd,
    con = glue::glue(paste0(directory,"/rmd_report/{title}_stock_report.Rmd"))
  )
}

# I created some fake stock at this link:
# https://provanik.s3.us-east-2.amazonaws.com/sim_stock.csv

X = fread("https://provanik.s3.us-east-2.amazonaws.com/sim_stock.csv")
X[,data := as.POSIXct(data)]

for(i in 3:10){
  stock_data  <- xts(X[,c(i),with=F],order.by = X$data)
  prices = stock_data
  log_returns = unname(diff(log(prices))[-1])
  
  NAV = data.table(data = index(log_returns),
                   nav = unname(exp(cumsum(log_returns))))
  setnames(NAV,names(NAV),c("date","nav"))
  
  # Plot time series
  plot_tmp = ggplot(NAV, aes(x = date, y = nav)) +
    geom_line() +
    theme_bw() +
    labs(title = paste0(names(stock_data)," time series"),
         subtitle = "The strategy is buy and hold",
         caption = "Data Source: Simulated") +
    theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(size = 8)) +
    xlab("Time") + ylab("NAV")
  
  tmp_table = data.table(date = index(stock_data),
                         price = unname(stock_data))
  setnames(tmp_table,names(tmp_table),c("Date","Price"))
  
  # Title of the stock that will fill the "title" object in Rmarkdown master
  title = colnames(stock_data)
  
  # we can import the "long_text" object even from a .txt file
  tmp_long_text_1 = paste0(readLines(paste0(directory,"/text_tmp.txt")),
                           collapse = "") 
  
  use_glue_master(title = title, long_text = tmp_long_text_1)
  
  dt_log_returns = data.table(date = index(log_returns),
                              ret = log_returns)
  
  # Returns density
  tmp_density = ggplot(dt_log_returns, aes(x = ret.V1)) + 
    geom_histogram(aes(y = ..density..),
                   colour = 1, fill = "white") +
    geom_density(lwd = 1.2,
                 linetype = 2,
                 colour = 2) +
    theme_bw() +
    labs(title = paste0(names(stock_data)," density returns"),
         subtitle = "The returs are log-transformed",
         caption = "Data Source: Simulated") +
    theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(size = 8)) +
    xlab("") + ylab("")
  
  # Let's add the parameters of RMarkdown with plots and tables
  params = list(tableA = tmp_table,
                plotA =  plot_tmp,
                plotB = tmp_density)
  
  rmarkdown::render(input = paste0(directory,"/rmd_report/",title,"_stock_report.Rmd"),
                    output_format = "html_document",
                    output_file = paste0(directory,"/html_report/",title,"_stock_report.html"),
                    params = params, #list of objects
                    envir = new.env(parent = globalenv()))
}



