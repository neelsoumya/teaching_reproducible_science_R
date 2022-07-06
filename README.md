# teaching_reproducible_science_R

[![License](https://img.shields.io/badge/license-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)


## Introduction

Material and notes for teaching reproducible science in R. 

## Installation

* Install R

    https://www.r-project.org/

* and R Studio

https://www.rstudio.com/products/rstudio/download/preview/

* Install the following packages in R:

```r
install.packages('rmarkdown')
install.packages('knitr')
install.packages('tinytex')
install.packages('sqldf')
install.packages('ggplot2')
install.packages('gplots')
install.packages('lme4')
install.packages('lmerTest')
install.packages('pROC')
install.packages('precrec')
install.packages('PRROC')
install.packages('boot')
install.packages('mlbench')
install.packages('caret')


```

* Download a zip file of this repository and unzip it

or

clone it 

```r
git clone https://github.com/neelsoumya/teaching_reproducible_science_R

cd teaching_reproducible_science_R
```

* Go to this new directory and set working directory to this directory in R.

```r
setwd('~/teaching_reproducible_science_R')
```

* In R studio, run the markdown


`rmarkdown.rmd`

https://github.com/neelsoumya/teaching_reproducible_science_R/blob/main/rmarkdown.rmd

## Code and lecture notes

A typical header of a R markdown file will look like

---

title: "Analysis and Writeup"

header-includes:

- \usepackage{placeins}

- \usepackage{float}

- \floatplacement{figure}{H}

output:

  pdf_document:

    fig_caption: yes

    keep_tex: yes

    latex_engine: xelatex

    number_sections: yes

  word_document: default

  html_document:

    df_print: paged

bibliography: Periphery_project.bib

urlcolor: blue

---

```{r include=FALSE}

knitr::opts_chunk$set(echo = TRUE)

knitr::opts_chunk$set(cache = TRUE)

knitr::opts_chunk$set(warning = FALSE)

knitr::opts_chunk$set(out.extra = '')

#knitr::opts_chunk$set(fig.pos = 'H')

```

\begin{centering}

\vspace{3 cm}

\Large

\normalsize

Soumya Banerjee, `r format(Sys.time(), "%b %d %Y")`

\vspace{3 cm}

\end{centering}

\setcounter{tocdepth}{2}

\tableofcontents

\newpage

```{r,include=FALSE}

library(knitr)

library(gridExtra)

library(rmarkdown)

# EQUATIONS in rmarkdown

$$ eGFR = eGFR_{0} + b_{before}*t_{before} $$

```

Italics in rmarkdown using *metafor*

Code can be rendered or shown in rmarkdown using

```

dsBaseClient::ds.summary(x='surv_object')

```

```{r, include=FALSE}

# Load packages and settings

library(sqldf)

library(ggplot2)

library(knitr)

library(rmarkdown)

library(gplots)

library(RColorBrewer)

library(reshape2)

library(png)

library(grid)

library(gridExtra)

library(lme4)

library(lmerTest)

library(lubridate)

library(reshape2)

library(data.table)

library(pheatmap)

library(cba)

library(rstanarm)

library(shinystan)

library(rstantools) # make a rstanarm package

library(bayesplot)

library("factoextra") #install_github("kassambara/factoextra")

library(rpart)

REQUIRED_PACKAGES <- c(

  "car",  # may need: sudo apt install libgfortran3

  "data.table",

  "ggplot2",

  "lubridate",

  "RODBC",

  "shinystan"

  # "survival"

  # "timereg"

)

LIBRARY_PREFIX <- "https://egret.psychol.cam.ac.uk/rlib/"

#source(paste0(LIBRARY_PREFIX, "datetimefunc.R"))

source("datetimefunc.R")

source(paste0(LIBRARY_PREFIX, "dbfunc.R"))

source(paste0(LIBRARY_PREFIX, "listfunc.R"))

source(paste0(LIBRARY_PREFIX, "listassign.R"))

source(paste0(LIBRARY_PREFIX, "miscfile.R"))

source(paste0(LIBRARY_PREFIX, "misclang.R"))

source(paste0(LIBRARY_PREFIX, "miscstat.R"))

source(paste0(LIBRARY_PREFIX, "stanfunc.R"))

source(paste0(LIBRARY_PREFIX, "cris_common.R"))

misclang$library_or_install(REQUIRED_PACKAGES)

```


## Template code

`rmarkdown.rmd`

https://github.com/neelsoumya/teaching_reproducible_science_R/blob/main/rmarkdown.rmd

## Exercises

* Create an Rmarkdown for the Boston housing dataset. See the tutorial below and just load and plot the data.

https://medium.com/analytics-vidhya/a-simple-ml-project-in-r-using-the-boston-dataset-e1143146ffb0

* A simple script to help you get started is here `simple_script.R`

https://github.com/neelsoumya/teaching_reproducible_science_R/blob/main/simple_project.R


## Resources and further reading

https://rmarkdown.rstudio.com/lesson-1.html

https://bookdown.org/yihui/rmarkdown-cookbook/

https://ropensci.org/

https://bookdown.org/home/

https://github.com/neelsoumya/dsSurvival_bookdown


## Contact

Soumya Banerjee

sb2333@cam.ac.uk


[![License](https://img.shields.io/badge/license-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
