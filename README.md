# Data3888_Optiver5 Shiny App User Guide

This guide will walk you through setting up and running the Data3888_Optiver5 Shiny App. This app offers a robust toolset for stock data visualization and analysis, with a clear, intuitive interface.

## Table of Contents

- [Downloading the Project](#downloading-the-project)
- [Setting Up the Environment](#setting-up-the-environment)
- [Data Files](#data-files)
- [Running the App](#running-the-app)
- [App Screenshots](#app-screenshots)

## Downloading the Project

Firstly, you need to clone the project from GitHub. You can do this by running the following command in your terminal:

```bash
git clone [GitHub Repository URL]
```

Please replace `[GitHub Repository URL]` with the actual URL of the repository.

## Setting Up the Environment

This app is based on R and the Shiny web application framework. To set it up, you need to:

1. Install R. You can download it from the [R Project website](https://www.r-project.org/).
2. Install the required R packages. The main package you need is Shiny. You can install it in R with the command: `install.packages("shiny")`. Please make sure to install any other necessary packages as specified in the `app.R` file.

## Data Files

The app requires two types of data files: 'all stock data' and 'additional stock data'. Please ensure that you have these files ready and correctly placed in the file path specified at the beginning of `app.R`.

## Running the App

To run the app, you need to:

1. Open RStudio and load `app.R`.
2. Run `app.R`. This process might take a few minutes as the app initializes the data.
3. Once the initialization process is done, your Shiny app should be up and running.

## App Screenshots

Here are some screenshots from the Shiny app:

- Home Screen ![Home Screen](https://github.com/NightmareQAQ/DATA3888_Optiver5/blob/main/img/home.png)
- About Screen ![About Screen](https://github.com/NightmareQAQ/DATA3888_Optiver5/blob/main/img/about.png)
- Introduction Screen ![Introduction Screen](https://github.com/NightmareQAQ/DATA3888_Optiver5/blob/main/img/introductioon.png)
- Cluster Model Screens
  - Cluster Model 1 ![Cluster Model 1](https://github.com/NightmareQAQ/DATA3888_Optiver5/blob/main/img/cluster_model.png)
  - Cluster Model 2 ![Cluster Model 2](https://github.com/NightmareQAQ/DATA3888_Optiver5/blob/main/img/cluster_model_2.png)
- Monitor Screen ![Monitor Screen](https://github.com/NightmareQAQ/DATA3888_Optiver5/blob/main/img/minitor.png)

Please note that these images are located in the `./img` folder within the project. This guide provides a glimpse of what you can expect when you run the Data3888_Optiver5 Shiny App. Enjoy exploring the data with this dynamic tool!

If you encounter any issues during the setup or operation of this Shiny app, please check the `README.md` file in the project repository or submit an issue on the GitHub project page.
