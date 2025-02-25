
## Project Description
In this project, our goal is to study **what factors affect the level of individual income in China**. It is commonly believed that the socioeconomic backgrounds and characteristics of residential community have large impact on the income level of individual. To extend existing findings, we proposed the research question on what factors impact personal income and how they impact. Our main data source is China Family Panel Studies (CFPS) in 2010 and 2014.

## About all directories and files

`Final_Report_with_Viz_files/figure-latex` : All graph files of `Final_Report_with_Viz.pdf`

`Visualization_grpahs_files/figure-gfm`	: All graph files of `Visualization_graphs.md`

`data` :	Directory contains raw data files of CFPS

`Final Report.Rmd` : Final paper for exlaining project

`Final_Report.pdf` :	Final paper for exlaining project

`Final_Report_with_Viz.Rmd/.pdf` : Final paper with visualization graphs 
(We don't recommend using this file since the formatting with many graphs is not ideal. Please refer to `Final_Report.pdf` and `Visualization_graphs.md`.)

`Visualization_grpahs.Rmd` : Data visualization R codes for the final paper

`Visualization_grpahs.md` : Data visualization gitghub document with all graphs 

`data.RData` : RData file that contains all the data to run the `Visualization_grpahs.Rmd`

## Instruction 
### 1. Clone the Repository
In your terminal, type below command.

```
git clone https://github.com/uc-dataviz/fp-nxu-achung.git
```

### 2. Install Packages to run the code
In your Rstudio terminal, type below commands.

```
install.packages('ggplot2')
install.packages('RColorBrewer')
install.packages('dplyr')
install.packages('plm')
install.packages('coefplot')
install.packages('corrplot')
install.packages('sp')
install.packages('SparseM')
 ```

### 3. Run or Knit `Visualization_grpahs.Rmd` in your Rstudio

## Contributors

* Alice Mee Seon Chung

  Implemented R codes for geospatial map and visualization of linear regression model part

* Ningyin Xu

  Implemented R codes for statistical graphs (distribution and relationship) with all the variables part





