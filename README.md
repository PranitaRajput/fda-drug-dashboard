# 📊 FDA Drug Event Dashboard (Shiny App)

This Shiny dashboard visualizes and explores adverse drug event reports from the openFDA API. It allows users to interactively filter and examine patterns in reported side effects, serious cases, and commonly affected drugs.

## 🚀 Features

- 📌 **Real-time Data**: Fetches latest data from the FDA Drug Event API.
- 📈 **Interactive Visualizations**:
  - Pie chart for seriousness of cases
  - Bar plot of most reported reactions
  - Table of drug reports with filters
- 📂 **Tidy and readable data structure** using `dplyr` and `tidyr`
- 📱 **User-friendly UI** with `shiny` and `shinydashboard`

## 🔧 Technologies Used

- `R`
- `Shiny`
- `shinydashboard`
- `httr`, `jsonlite`, `dplyr`, `tidyr`, `ggplot2`

