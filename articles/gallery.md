# Plot gallery

A visual reference for every plot type in mSigPlot.

``` r
library(mSigPlot)
orders <- catalog_row_order()
```

## SBS96

``` r
plot_SBS96(catalog_sbs96, plot_title = "SBS96")
```

![](gallery_files/figure-html/sbs96-1.png)

## SBS192

``` r
plot_SBS192(catalog_sbs192, plot_title = "SBS192")
```

![](gallery_files/figure-html/sbs192-1.png)

## SBS288

``` r
plot_SBS288(sbs288_df[, 1, drop = FALSE], plot_title = "SBS288A")
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
```

![](gallery_files/figure-html/sbs288-1.png)

## SBS12

``` r
plot_SBS12(catalog_sbs192, plot_title = "SBS12 strand bias")
```

![](gallery_files/figure-html/sbs12-1.png)

## SBS1536

``` r
plot_SBS1536(catalog_sbs1536, plot_title = "SBS1536")
```

![](gallery_files/figure-html/sbs1536-1.png)

## DBS78

``` r
plot_DBS78(catalog_dbs78, plot_title = "DBS78")
```

![](gallery_files/figure-html/dbs78-1.png)

## DBS144

``` r
plot_DBS144(catalog_dbs144, plot_title = "DBS144")
```

![](gallery_files/figure-html/dbs144-1.png)

## DBS136

``` r
plot_DBS136(dbs136_df[, 1, drop = FALSE], plot_title = "DBS136")
```

![](gallery_files/figure-html/dbs136-1.png)

## ID83

``` r
plot_ID83(id83_sigs[, "ID1", drop = FALSE], plot_title = "ID83 (COSMIC ID1)")
```

![](gallery_files/figure-html/id83-1.png)

## ID89

``` r
plot_ID89(id89_sigs[, 1, drop = FALSE], plot_title = "ID89")
```

![](gallery_files/figure-html/id89-1.png)

## ID166

``` r
plot_ID166(sig_id166, plot_title = "ID166 (simulated)")
```

![](gallery_files/figure-html/id166-1.png)

## ID476

``` r
plot_ID476(id476_sigs[, 1, drop = FALSE], plot_title = "ID476")
```

![](gallery_files/figure-html/id476-1.png)

## ID476 right panel

``` r
plot_ID476_right(id476_sigs[, 1, drop = FALSE], plot_title = "ID476 right panel")
```

![](gallery_files/figure-html/id476-right-1.png)
