Tool for Intervention Prioritization (TIP)
Live Application: https://esteps.shinyapps.io/prioritization/

About The Tool
The Tool for Intervention Prioritization (TIP) is an open-source R Shiny application designed to help public health professionals, researchers, and community stakeholders collaboratively prioritize interventions. It provides a user-friendly platform for teams to evaluate options using two established frameworks:

The Customizable 2x2 Matrix: For rapid, high-level qualitative assessment based on user-defined criteria (defaulting to the Impact-Effort Matrix).

The RE-AIM Framework: For a more detailed, multi-dimensional evaluation of an intervention's potential for real-world public health impact.

This tool was developed by the STEPS-INDIA Collaborators to facilitate structured, transparent, and evidence-informed decision-making during consultative meetings.

Installation and Usage
To run this application on your own computer, you will need R and RStudio.

1. Install Required R Packages:
Open your R console and run the following commands to install the necessary packages. This only needs to be done once.

# Install packages from the main repository (CRAN)
install.packages(c("shiny", "ggplot2", "plotly", "DT", "rmarkdown", "shinycssloaders", "shinyjs", "dplyr", "tidyr", "scales", "remotes"))

# The 'ggradar' package for radar charts must be installed from GitHub
remotes::install_github("ricardo-bion/ggradar", dependencies = TRUE)

2. Download Project Files:
Download the following files from this repository and place them all in the same folder:

app.R

tutorial.Rmd

report_template.Rmd

3. Run the App:
Open the app.R file in RStudio and click the "Run App" button in the top-right corner of the script editor.

License
This project is open-source and is licensed under the MIT License. See the "About & License" tab in the application for more details.
