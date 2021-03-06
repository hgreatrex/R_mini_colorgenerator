Palette maker example
================

``` r
source("HLG_Palettemaker_function.R")
```

-   **package:** Currently either “brewer” or “viridis”
-   **pal:** The sub-palette you want, e.g. “YlGnBu”, “magma”
-   **n:** How many output colours you want
-   **reverse :** Set to true if you want the colors to go the other way
-   **removeend:** 1 removes the final color, 2 the two final colors
    etc, default=0
-   **removestart:** 1 removes the first color, 2 the two first colors
    etc, default=0
-   **rgb:** Set to true to output rgb values, else hex

It outputs a list, with the first value a description of the options and
the second a list value with the colors either hex or rgb

<br>

# Examples

### 1. Basic hex

``` r
package <- "viridis" ; pal <- "viridis"
        n <- 10      ; reverse<-TRUE
removeend <- 0       ; removestart<-2   

hexpal <- palette.function(package,pal,n,rgb=FALSE,reverse,removeend,removestart)
```

<br>

The first list value is the option you entered

``` r
print(hexpal[[1]])
```

    ## [1] "viridis_viridis_10_remstart2_reversed_hex"

The second list value are the colours

``` r
hexpal[[2]]
```

    ##  [1] "#FDE725" "#C5DF2D" "#90D542" "#5DC863" "#39B676" "#25A384" "#21908C"
    ##  [8] "#287C8D" "#31678D" "#3B528B"

<br>

``` r
plot(1:n, 1:n,cex=2,pch=16,col= hexpal[[2]],main=hexpal[[1]])
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### 2. Make a new variable with your options

``` r
hexpal <- palette.function("brewer","Blues",n,rgb=FALSE,reverse,removeend,removestart)

# set the colors to be your variable string
eval(parse(text=paste(paste(hexpal[[1]], "<- hexpal[[2]]"))))

plot(1:n, 1:n,cex=2,pch=16,
     col= brewer_Blues_10_remstart2_reversed_hex)
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

<br>

### 3. RGB output to file

Make an rgb output and save output text file to the Output subfolder

``` r
rgbpal <- palette.function(package,pal,n,rgb=TRUE,reverse,removeend,removestart)

filename <- paste("./Output/",rgbpal[[1]],".txt",sep="")

write.table(rgbpal[[2]],filename ,
            col.names = FALSE,row.names = FALSE,
            quote=FALSE,sep = ",")
```
