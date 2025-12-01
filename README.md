# Forecasting with Linear Regression Models

Materials for the "Forecasting with Linear Regression Models" workshop at the LaticR 2025 conference. 

This workshop focuses on the foundation of feature-based forecasting with linear regression using R. The workshop is beginner-friendly, and it covers the following topics:

- How to frame a time series as a regression problem
- Feature engineering techniques for time-aware data
- Modeling trend, seasonality, outliers, and breaks
- Practical tips for evaluation and validation
<figure>
 <img src="assets/latin_america_map.png" width="100%" align="center"/></a>
<figcaption> Latin America 3D map (created with Midjourney)</figcaption>
</figure>

<br>
<br />

This workshop is for practitioners (data analysts/scientists) who wish to learn how to forecast with regression models. It assumes no background in time series analysis and forecasting, but assumes basic knowledge of probability, linear regression, and R programming.




When 📆: December 1th, 2025 from 9am to 11am PST

Where 📌: Online

Tickets: https://www.eventbrite.cl/e/forecasting-with-regression-models-tickets-1955640717029

## Environment Settings

To ensure a smooth workshop experience, you can set up your R environment using one of the following options:

### Option 1: Dev Container (Recommended)

If you're using VSCode, you can use the Dev Container extension to run the workshop in a pre-configured containerized environment:

1. Install [Docker Desktop](https://www.docker.com/products/docker-desktop)
2. Install the [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) in VSCode
3. Open this repository in VSCode
4. When prompted, click "Reopen in Container" or run the command "Dev Containers: Reopen in Container"

The container includes all required R packages and dependencies pre-installed.

### Option 2: Using renv

If you prefer to work in your local R environment, you can use `renv` to install the exact package versions used in the workshop:

1. Ensure you have R (version 4.0 or higher) installed
2. Open R in your preferred IDE (Positron/RStudio/VScode) in the project directory
3. Install renv if not already installed:
   ```r
   install.packages("renv")
   ```
4. Restore the environment using the provided `renv.lock` file:
   ```r
   renv::restore()
   ```

This will automatically install all required packages with the exact versions specified in the `renv.lock` file.

