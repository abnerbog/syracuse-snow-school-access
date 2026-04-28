# syracuse-snow-school-access
Reproducible data workflow in the R programming language for examining municipal snow removal services and access to education in Syracuse.

To run this workflow, create an `.Renviron` file in the root directory with your HydroShare credentials:

```
HYDROSHARE_USR=username
HYDROSHARE_PWD=password
```

Then run `quarto render` in the root directory to generate the report. Note that you will need to install the `tidyverse`, `scales` and `httr` R packages to run this workflow.