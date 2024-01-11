data <- readxl::read_xlsx("Oct-Dec SC Analysis.xlsx", "Qualified")
summary(data)
data$CPMQL <- as.numeric(data$CPMQL)
data$CPMQL <- round(data$CPMQL, 2)

clean_data <- na.omit(data)

data$Type <- ifelse(grepl('^FPT', data$`Campaign Name`), 'FPT', 'WP')


avgCPMQL <- sum(data$`Total Spent`) / sum(data$MQL)
avgMQL <- mean(data$MQL)

library(dplyr)
data$cohort <- case_when(
  data$MQL > avgMQL & data$CPMQL > avgCPMQL ~ "Cohort A",
  data$MQL > avgMQL & data$CPMQL < avgCPMQL ~ "Cohort B",
  data$MQL < avgMQL & data$CPMQL > avgCPMQL ~ "Cohort C",
  TRUE ~ "Cohort D"
)

data$cohort[data$`Campaign Name` == "FPT_TechValidate_Fin_DKC_0523"] <- "Cohort C"
data$cohort[data$`Campaign Name` == "FPT_TechValidate_Retargeting_DKC_0523"] <- "Cohort C"

install.packages("plotly")
library(plotly)
# Create a scatter plot

vline <- function(x = 0, color = "black") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    text = "AVG CPMQL",
    line = list(color = color, opacity = 0.5)
  )
}

hline <- function(y = 0, color = "black", text = "AVG MQL") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    text = "AVG MQL",
    line = list(color = color, text = "AVG MQL")
  )
}


library(htmlwidgets)
library(htmltools)
plot <- plot_ly(data, x = ~CPMQL, y = ~MQL, type = 'scatter', mode = 'markers',
                color = ~cohort,
                symbol = ~Type,
                symbols = c("star", "circle"),
                text = ~paste("Campaign: ", `Campaign Name`,
                              "\n", "CP MQL:", data$CPMQL, "\n", "MQL:", data$MQL),
                hoverinfo = "text",
                colors = c("Orange", "Green", "Red", "Blue")) %>%
            
  layout(
  xaxis = list(zeroline = FALSE),
  yaxis = list(zeroline = FALSE),
  shapes = list(vline(avgCPMQL), hline(avgMQL)), 
  annotations = list(
    list(x = avgCPMQL + 100, y = max(data$MQL),
         text = "Average CP MQL: $301.58",
         showarrow = FALSE,
         textangle = 90),
    list(x = 6500, y = avgMQL + 5,
         text = "Average MQL: 27.3",
         showarrow = FALSE)
    )
  )

plot

output_file <- "/Users/danielbeim/Downloads/December Performance/SC_Peformance_Plot.html"

# Save the Plotly plot as an HTML file
saveWidget(plot, file = output_file, selfcontained = FALSE)

# Display a message with the link to the HTML file
cat("Plot saved as HTML file. Open the following link in your browser:\n", output_file)

write_ht

library(writexl)
library(data.table)
data <- as.data.frame(data)
dataexp <- data %>% select(c('Campaign Name', 'CID Form', 'Total Spent', 'MQL','CPMQL', 'cohort'))
write_xlsx(dataexp, "SC_Campaign_Cohorts.xlsx")


#IM CAMPAIGNS

dataIM <- readxl::read_xlsx("Oct-Dec IM analysis.xlsx", "Qualified")
summary(dataIM)
dataIM$`CP MQL` <- as.numeric(dataIM$`CP MQL`)
dataIM$`CP MQL` <- round(dataIM$`CP MQL`, 2)

clean_dataIM <- na.omit(dataIM)
dataIM$Type <- ifelse(grepl('^FPT', dataIM$`Campaign Name`), 'FPT', 'WP')


IMavgCPMQL <- sum(dataIM$`Total Spent`) / sum(dataIM$MQL)
IMavgMQL <- mean(dataIM$MQL)

library(dplyr)
dataIM$cohort <- case_when(
  dataIM$MQL > IMavgMQL & dataIM$`CP MQL` > IMavgCPMQL ~ "Cohort A",
  dataIM$MQL > IMavgMQL & dataIM$`CP MQL` < IMavgCPMQL ~ "Cohort B",
  dataIM$MQL < IMavgMQL & dataIM$`CP MQL` > IMavgCPMQL ~ "Cohort C",
  TRUE ~ "Cohort D"
)


plot2 <- plot_ly(dataIM, x = ~`CP MQL`, y = ~MQL, type = 'scatter', mode = 'markers',
                color = ~cohort,
                symbol = ~Type,
                symbols = c("star", "circle"),
                text = ~paste("Campaign: ", `Campaign Name`,
                              "\n", "CP MQL:", dataIM$`CP MQL`, "\n", "MQL:", dataIM$MQL),
                hoverinfo = "text",
                colors = c("Orange", "Green", "Red", "Blue")) %>%
  
  layout(
    xaxis = list(zeroline = FALSE),
    yaxis = list( zeroline = FALSE),
    shapes = list(vline(IMavgCPMQL), hline(IMavgMQL))
  )

plot2

dataIM <- as.data.frame(dataIM)
dataexp2 <- dataIM %>% select(c('Campaign Name', 'CID Form', 'Total Spent', 'MQL','CP MQL', 'cohort'))
write_xlsx(dataexp2, "IM_Campaign_Cohorts.xlsx")

