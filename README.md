# A collection of functions to work with the data from the iffi-database in R 

The package (without the vignettes) can be installed with:

**INSTALL FROM GITHUB**

`remotes::install_github("https://github.com/RobinKohrs/iffitoR")`

**INSTALL FROM GITLAB**

```r
# the paths to the public and private ssh-key
pubic = "C://PATH/TO/PUBLIC/id_rsa.pub",
private = "C://PATH/TO/PRIVATE/id_rsa"

# install it using devtools
devtools::install_git("git@gitlab.inf.unibz.it:proslide/iffitor.git",
                      credentials = creds)
```

**BUILD THE PACKAGE LOCALLY**

In order to get the vignettes locally you can build the package yourself by:

1. Clone the repo with:

    *  `git clone "https://github.com/RobinKohrs/iffitoR"`
  
  
    *  `git clone git@gitlab.inf.unibz.it:proslide/iffitor.git`

2. Build the package locally

    * `R CMD build iffitoR`
  
3. Install it

    * `R CMD INSTALL iffitoR`
    
****

When working under linux one will not be able to use the `RODBC`-functions in order to access the `.mdb`-databases. The "solution" at the moment is to use the data that comes with the package (`landsld`) and to not build the vignettes. This data already comes with some important information about the lanslides. For querying additional information one needs access to the databases.

## More information

Some example queries, as welll as more information about the funcions in the package can be found here:

[Vignettes](https://robinkohrs.github.io/iffitoR/docs/index.html)

***

![](man/figures/anim.gif)
