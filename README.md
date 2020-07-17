# Tools for data.table objects in the REMIND context

R package **rmndt**, version **0.4.3**

  

## Purpose and Functionality

Helper functions for REMIND-related tasks with data.table objects, e.g., interpolation and (dis-)aggregation.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("rmndt")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Alois Dirnaichner <dirnaichner@pik-potsdam.de>.

## Citation

To cite package **rmndt** in publications use:

Dirnaichner A (2020). _rmndt: Tools for data.table objects in the
REMIND context_. R package version 0.4.3.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {rmndt: Tools for data.table objects in the REMIND context},
  author = {Alois Dirnaichner},
  year = {2020},
  note = {R package version 0.4.3},
}
```

