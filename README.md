# Rose

Install from Rstudio please or try inside the this directory start R and

```
devtools::load_all()
devtools::install()
```

call it in your scripts with

```
library(Rose)
```

do add documentation you can use roxygen, add special comment before the 
functions and then type
```
library("devtools")
library(roxygen2)
document()
```

## WASM




pull the container
```
docker pull ghcr.io/r-wasm/webr:main
```

run doker in the directory `Rose-wasm`

```
docker run -it --rm -v ${PWD}/Rose:/home/garofalo/programs/Rose -w /home/garofalo/programs/Rose  ghcr.io/r-wasm/webr:main R
```

in the container then install 
```
install.packages("pak")
pak::pak("r-wasm/rwasm")
```
load the library
```
library(rwasm)
```

install devtools in the container

```
install.packages("devtools")
```




look at https://github.com/r-wasm/rwasm/blob/main/vignettes/rwasm.Rmd