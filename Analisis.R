library(readr)
library(ggplot2)
library(plotly)
library(tidyr)

database <- read_csv("data/power by DataBase.csv")

colnames(database)

years <- c(1960:2022)
years <- as.character(years)

columnas_importantes <- c("Country Code", years)

database1 <- database[,columnas_importantes]
names(database1)[names(database1) == "Country Code"] <- "country_code"

filter_base <- function(df,code){
  sub <- database1 %>% filter(country_code == code)
  sub <- sub %>% select(where(~ !any(is.nan(.))))
  
  value_row <- sub %>% slide(1) %>% select(2:ncol(sub))
  
  Value_vector <- as.vector(unlist(value_row))
  
  sub_df <- data.frame(
    years <- years, 
    value <- value_vector
  )
  p <- ggplot(sub_df,aes(x = years, y = value)) +
    geom_line() +
    geom_point() +
    ggtitle(paste("crecimiento poblacional",code)) +
    xlab("year") +
    ylab("value")
  
  print(p)
}

filter_base(database1, "ARG")






