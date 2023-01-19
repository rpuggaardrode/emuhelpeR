
<!-- README.md is generated from README.Rmd. Please edit that file -->

# emuhelpeR: Convenience functions for working with emuR

<!-- badges: start -->
<!-- badges: end -->

This package collects convenience functions for working with `emuR` and
EMU-SDMS. For more information on those tools, see [this useful
manual](https://ips-lmu.github.io/The-EMU-SDMS-Manual/index.html).

For now, functions are available to bulk extract and pre-process [SSFF
tracks](https://ips-lmu.github.io/The-EMU-SDMS-Manual/chap-sigDataExtr.html#extracting-pre-defined-tracks),
with specific functions available for processing fundamental frequency,
formants, and measures which depend directly on these. This can all be
done in a single step with the function `import_ssfftracks()`, but the
different steps can also be carried out independently. Some of the
independent functions may well be useful for raw data that isn’t
generated in `emuR`. These functions are adapted from the data
processing used in [Kirby et
al. forthc.](https://doi.org/10.17605/OSF.IO/WV6QZ).

## At a glance

The function `import_ssfftracks()` assumes that you have raw data stored
in an EMU database which has already been loaded into R, and that you
have generated a segment list with relevant portions of the data using
[the querying
system](https://ips-lmu.github.io/The-EMU-SDMS-Manual/chap-querysys.html)
in `emuR`. Let’s load some example data from [Kirby et
al. forthc.](https://doi.org/10.17605/OSF.IO/WV6QZ) into R.

``` r
datapath <- system.file('extdata/db', package='emuhelpeR')
raw <- emuR::load_emuDB(datapath)
#> INFO: Checking if cache needs update for 2 sessions and 10 bundles ...
#> INFO: Performing precheck and calculating checksums (== MD5 sums) for _annot.json files ...
#> INFO: Nothing to update!
```

This data can be inspected in EMU-SDMS by typing `emuR::serve(raw)` in
the R console. Let’s have a look at a segment list I prepared:

``` r
library(emuhelpeR)
seg_list
#> # A tibble: 20 × 28
#>    labels start   end db_uuid       session bundle speaker repet…¹ word  start…²
#>    <chr>  <dbl> <dbl> <chr>         <chr>   <chr>  <chr>   <chr>   <chr>   <int>
#>  1 cl     1134. 1235. 33157d9f-4a3… f1      F1-00… EF1     1       bOh         4
#>  2 cl      527.  632. 33157d9f-4a3… f1      F1-00… EF1     1       biit        4
#>  3 cl      407.  497. 33157d9f-4a3… f1      F1-00… EF1     1       biq         4
#>  4 cl      456.  555. 33157d9f-4a3… f1      F1-00… EF1     1       buh         4
#>  5 cl     1061. 1173. 33157d9f-4a3… f1      F1-00… EF1     1       buq         4
#>  6 cl     2028. 2120. 33157d9f-4a3… m1      M1-00… EM1     1       bOh         4
#>  7 cl     2009. 2054. 33157d9f-4a3… m1      M1-00… EM1     1       biit        6
#>  8 cl     1465. 1544. 33157d9f-4a3… m1      M1-00… EM1     1       biq         5
#>  9 cl      804.  902. 33157d9f-4a3… m1      M1-00… EM1     1       buh         4
#> 10 cl      647.  781. 33157d9f-4a3… m1      M1-00… EM1     1       buq         4
#> 11 op     1235. 1453. 33157d9f-4a3… f1      F1-00… EF1     1       bOh         5
#> 12 op      632.  788. 33157d9f-4a3… f1      F1-00… EF1     1       biit        5
#> 13 op      497.  646. 33157d9f-4a3… f1      F1-00… EF1     1       biq         5
#> 14 op      555.  789. 33157d9f-4a3… f1      F1-00… EF1     1       buh         5
#> 15 op     1173. 1323. 33157d9f-4a3… f1      F1-00… EF1     1       buq         5
#> 16 op     2120. 2316. 33157d9f-4a3… m1      M1-00… EM1     1       bOh         5
#> 17 op     2054. 2197. 33157d9f-4a3… m1      M1-00… EM1     1       biit        7
#> 18 op     1544. 1745. 33157d9f-4a3… m1      M1-00… EM1     1       biq         6
#> 19 op      902. 1164. 33157d9f-4a3… m1      M1-00… EM1     1       buh         5
#> 20 op      781.  940. 33157d9f-4a3… m1      M1-00… EM1     1       buq         5
#> # … with 18 more variables: end_item_id <int>, level <chr>, attribute <chr>,
#> #   start_item_seq_idx <int>, end_item_seq_idx <int>, type <chr>,
#> #   sample_start <int>, sample_end <int>, sample_rate <int>, ov <dbl>,
#> #   cv <dbl>, rv <dbl>, onset <chr>, vowel <chr>, coda <chr>, template <chr>,
#> #   presyllable <chr>, register <chr>, and abbreviated variable names
#> #   ¹​repetition, ²​start_item_id
dplyr::glimpse(seg_list)
#> Rows: 20
#> Columns: 28
#> $ labels             <chr> "cl", "cl", "cl", "cl", "cl", "cl", "cl", "cl", "cl…
#> $ start              <dbl> 1134.0023, 527.4263, 407.4263, 456.1111, 1060.9184,…
#> $ end                <dbl> 1234.8866, 631.6440, 497.2222, 554.5918, 1173.0499,…
#> $ db_uuid            <chr> "33157d9f-4a3a-468a-882b-60d3b10ea771", "33157d9f-4…
#> $ session            <chr> "f1", "f1", "f1", "f1", "f1", "m1", "m1", "m1", "m1…
#> $ bundle             <chr> "F1-0002-car-rep1-bOh-18", "F1-0002-car-rep1-biit-7…
#> $ speaker            <chr> "EF1", "EF1", "EF1", "EF1", "EF1", "EM1", "EM1", "E…
#> $ repetition         <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "…
#> $ word               <chr> "bOh", "biit", "biq", "buh", "buq", "bOh", "biit", …
#> $ start_item_id      <int> 4, 4, 4, 4, 4, 4, 6, 5, 4, 4, 5, 5, 5, 5, 5, 5, 7, …
#> $ end_item_id        <int> 4, 4, 4, 4, 4, 4, 6, 5, 4, 4, 5, 5, 5, 5, 5, 5, 7, …
#> $ level              <chr> "ORL", "ORL", "ORL", "ORL", "ORL", "ORL", "ORL", "O…
#> $ attribute          <chr> "ORL", "ORL", "ORL", "ORL", "ORL", "ORL", "ORL", "O…
#> $ start_item_seq_idx <int> 3, 3, 3, 3, 3, 3, 5, 4, 3, 3, 4, 4, 4, 4, 4, 4, 6, …
#> $ end_item_seq_idx   <int> 3, 3, 3, 3, 3, 3, 5, 4, 3, 3, 4, 4, 4, 4, 4, 4, 6, …
#> $ type               <chr> "SEGMENT", "SEGMENT", "SEGMENT", "SEGMENT", "SEGMEN…
#> $ sample_start       <int> 50010, 23260, 17968, 20115, 46787, 89436, 88615, 64…
#> $ sample_end         <int> 54458, 27855, 21927, 24457, 51731, 93488, 90573, 68…
#> $ sample_rate        <int> 44100, 44100, 44100, 44100, 44100, 44100, 44100, 44…
#> $ ov                 <dbl> 1134.0136, 649.4785, 407.4376, 456.1224, 1060.9297,…
#> $ cv                 <dbl> 1185.3288, NA, 476.2358, 478.8209, 1135.4195, NA, N…
#> $ rv                 <dbl> 1241.0658, NA, 515.8957, 577.7551, 1192.2222, NA, N…
#> $ onset              <chr> "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "…
#> $ vowel              <chr> "O", "ii", "i", "u", "u", "O", "ii", "i", "u", "u",…
#> $ coda               <chr> "h", "t", "q", "h", "q", "h", "t", "q", "h", "q", "…
#> $ template           <chr> "bOh", "biit", "biq", "buh", "buq", "bOh", "biit", …
#> $ presyllable        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ register           <chr> "low", "low", "low", "low", "low", "low", "low", "l…
```

There are a bunch of functional measures available for this database, as
the following prompt will tell us:

``` r
emuR::list_ssffTrackDefinitions(raw)
#>       name columnName fileExtension
#> 1  praatF0        pF0           pF0
#> 2    eggF0       pdF0          pdF0
#> 3    H1H2c      H1H2c         H1H2c
#> 4    H1A1c      H1A1c         H1A1c
#> 5    H1A3c      H1A3c         H1A3c
#> 6      CPP        CPP           CPP
#> 7    CQ_PH      CQ_PH         CQ_PH
#> 8    CQ_PD      CQ_PD         CQ_PD
#> 9  praatF1        pF1           pF1
#> 10 praatF2        pF2           pF2
#> 11 praatF3        pF3           pF3
```

Using `import_ssfftracks()` we can bulk extract all these measures from
the segments in `seg_list` into a single data frame. I set `proc=FALSE`,
because right now we just want to extract the raw measures. `verbose` is
set to `FALSE` to avoid printing progress bars that look ugly on GitHub.

``` r
x <- import_ssfftracks(db_handle=raw, seg_list=seg_list, proc=FALSE, verbose=FALSE)
x
#> # A tibble: 2,823 × 43
#>    sl_rowIdx labels start   end db_uuid     session bundle speaker repet…¹ word 
#>        <int> <chr>  <dbl> <dbl> <chr>       <chr>   <chr>  <chr>   <chr>   <chr>
#>  1         1 cl     1134. 1235. 33157d9f-4… f1      F1-00… EF1     1       bOh  
#>  2         1 cl     1134. 1235. 33157d9f-4… f1      F1-00… EF1     1       bOh  
#>  3         1 cl     1134. 1235. 33157d9f-4… f1      F1-00… EF1     1       bOh  
#>  4         1 cl     1134. 1235. 33157d9f-4… f1      F1-00… EF1     1       bOh  
#>  5         1 cl     1134. 1235. 33157d9f-4… f1      F1-00… EF1     1       bOh  
#>  6         1 cl     1134. 1235. 33157d9f-4… f1      F1-00… EF1     1       bOh  
#>  7         1 cl     1134. 1235. 33157d9f-4… f1      F1-00… EF1     1       bOh  
#>  8         1 cl     1134. 1235. 33157d9f-4… f1      F1-00… EF1     1       bOh  
#>  9         1 cl     1134. 1235. 33157d9f-4… f1      F1-00… EF1     1       bOh  
#> 10         1 cl     1134. 1235. 33157d9f-4… f1      F1-00… EF1     1       bOh  
#> # … with 2,813 more rows, 33 more variables: start_item_id <int>,
#> #   end_item_id <int>, level <chr>, attribute <chr>, start_item_seq_idx <int>,
#> #   end_item_seq_idx <int>, type <chr>, sample_start <int>, sample_end <int>,
#> #   sample_rate <int>, ov <dbl>, cv <dbl>, rv <dbl>, onset <chr>, vowel <chr>,
#> #   coda <chr>, template <chr>, presyllable <chr>, register <chr>,
#> #   times_orig <dbl>, times_rel <dbl>, times_norm <dbl>, praatF0 <dbl>,
#> #   eggF0 <dbl>, H1H2c <dbl>, H1A1c <dbl>, H1A3c <dbl>, CPP <dbl>, …
dplyr::glimpse(x)
#> Rows: 2,823
#> Columns: 43
#> $ sl_rowIdx          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ labels             <chr> "cl", "cl", "cl", "cl", "cl", "cl", "cl", "cl", "cl…
#> $ start              <dbl> 1134.002, 1134.002, 1134.002, 1134.002, 1134.002, 1…
#> $ end                <dbl> 1234.887, 1234.887, 1234.887, 1234.887, 1234.887, 1…
#> $ db_uuid            <chr> "33157d9f-4a3a-468a-882b-60d3b10ea771", "33157d9f-4…
#> $ session            <chr> "f1", "f1", "f1", "f1", "f1", "f1", "f1", "f1", "f1…
#> $ bundle             <chr> "F1-0002-car-rep1-bOh-18", "F1-0002-car-rep1-bOh-18…
#> $ speaker            <chr> "EF1", "EF1", "EF1", "EF1", "EF1", "EF1", "EF1", "E…
#> $ repetition         <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "…
#> $ word               <chr> "bOh", "bOh", "bOh", "bOh", "bOh", "bOh", "bOh", "b…
#> $ start_item_id      <int> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, …
#> $ end_item_id        <int> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, …
#> $ level              <chr> "ORL", "ORL", "ORL", "ORL", "ORL", "ORL", "ORL", "O…
#> $ attribute          <chr> "ORL", "ORL", "ORL", "ORL", "ORL", "ORL", "ORL", "O…
#> $ start_item_seq_idx <int> 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, …
#> $ end_item_seq_idx   <int> 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, …
#> $ type               <chr> "SEGMENT", "SEGMENT", "SEGMENT", "SEGMENT", "SEGMEN…
#> $ sample_start       <int> 50010, 50010, 50010, 50010, 50010, 50010, 50010, 50…
#> $ sample_end         <int> 54458, 54458, 54458, 54458, 54458, 54458, 54458, 54…
#> $ sample_rate        <int> 44100, 44100, 44100, 44100, 44100, 44100, 44100, 44…
#> $ ov                 <dbl> 1134.014, 1134.014, 1134.014, 1134.014, 1134.014, 1…
#> $ cv                 <dbl> 1185.329, 1185.329, 1185.329, 1185.329, 1185.329, 1…
#> $ rv                 <dbl> 1241.066, 1241.066, 1241.066, 1241.066, 1241.066, 1…
#> $ onset              <chr> "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "…
#> $ vowel              <chr> "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "…
#> $ coda               <chr> "h", "h", "h", "h", "h", "h", "h", "h", "h", "h", "…
#> $ template           <chr> "bOh", "bOh", "bOh", "bOh", "bOh", "bOh", "bOh", "b…
#> $ presyllable        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ register           <chr> "low", "low", "low", "low", "low", "low", "low", "l…
#> $ times_orig         <dbl> 1135, 1136, 1137, 1138, 1139, 1140, 1141, 1142, 114…
#> $ times_rel          <dbl> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 1…
#> $ times_norm         <dbl> 0.00000000, 0.01010101, 0.02020202, 0.03030303, 0.0…
#> $ praatF0            <dbl> 177.128, 174.929, 172.729, 170.530, 168.330, 166.72…
#> $ eggF0              <dbl> 145.9689, 145.9689, 145.9689, 145.9689, 145.9689, 1…
#> $ H1H2c              <dbl> 7.146, 6.966, 6.906, 6.942, 6.558, 7.199, 7.933, 8.…
#> $ H1A1c              <dbl> 13.184, 13.637, 13.875, 14.011, 13.831, 14.256, 14.…
#> $ H1A3c              <dbl> 18.417, 19.378, 20.357, 21.405, 22.068, 21.353, 19.…
#> $ CPP                <dbl> 23.326, 20.667, 16.525, 11.272, 12.012, 11.597, 12.…
#> $ CQ_PH              <dbl> 0.5798880, 0.5798880, 0.5798880, 0.5798880, 0.57988…
#> $ CQ_PD              <dbl> 0.6028057, 0.6028057, 0.6028057, 0.6028057, 0.60280…
#> $ praatF1            <dbl> 504.725, 501.468, 498.210, 494.952, 491.694, 484.35…
#> $ praatF2            <dbl> 974.190, 996.762, 1019.334, 1041.906, 1064.478, 109…
#> $ praatF3            <dbl> 3179.065, 3179.028, 3178.991, 3178.953, 3178.916, 3…
```

Neat! But if we skip `proc=FALSE` and set some more parameters, we can
also do a bunch of preprocessing in the same step, such as by-speaker
normalization and automated removal of outliers that fall outside three
standard deviations from the mean within the same group. When the
function is called, it will also print a message telling us how many
outliers were removed from each track.

-   `f0col='praatF0'` specifies that F0 values are stored in the SSFF
    track `praatF0`. In this track, values of 0 should be recoded as
    `NA`, and outliers should be automatically removed after.
-   `f0dep='H1H2c'` specifies that the track `H1H2c` (the difference
    between the first two harmonics) is directly dependent on the F0
    measurements, so for each F0 measure coded as `NA`, the
    corresponding `H1H2c` should also be coded as `NA`.
-   `fncol=c('praatF1', 'praatF2', 'praatF3')` specifies that the
    available formant measures F1-F3 are stored in the SSFF tracks
    `praatF1`, `praatF2`, and `praatF3`. Outliers are automatically
    removed.
-   `fndep=list(c('H1A1c', 'F1'), c('H1A3c', 'F3'))` specifies that,
    respectively, `H1A1c` is a spectral measure that directly depends on
    F1 (and F0), and `H1A3c` is a spectral measure that directly depends
    on F3 (and F0). `H1A1c` values will be coded as `NA` if the
    corresponding F1 *or* F0 measure is `NA`, etc.
-   `speaker='speaker'` specifies that there is a column with speaker
    information in the `seg_list` data frame, and that column is labeled
    `speaker`. This is used for by-speaker normalization.
-   `group_var=c('speaker', 'vowel')` specifies that the columns
    `speaker` and `vowel` in `seg_list` should be used for determining
    which tokens should be automatically removed; only tokens that are
    three standard deviations from the mean within-speaker and
    within-vowel are removed.
-   `timing_rm=list('cl', 250)` specifies that F0 measurements that are
    more than 250 ms removed from a `cl` label in the data should be
    removed.
-   `outlier_rm='eggF0'` specifies that, in addition to the automated
    outlier procedures that have already been applied, outliers should
    also be automatically removed from the SSFF track `eggF0`.

``` r
y <- import_ssfftracks(db_handle=raw, seg_list=seg_list,
                       f0col='praatF0', f0dep='H1H2c', 
                       fncol=c('praatF1', 'praatF2', 'praatF3'),
                       fndep=list(c('H1A1c', 'F1'), c('H1A3c', 'F3')),
                       speaker='speaker', group_var=c('speaker', 'vowel'),
                       timing_rm=list('cl', 250), outlier_rm='eggF0',
                       verbose=FALSE)
#> [1] "Initial number of NAs in F0 track: 502"
#> [1] "Number of NAs removed from F0 track during automated outlier removal: 4"
#> [1] "Number of NAs removed from H1H2c track during automated outlier removal: 506"
#> [1] "Number of NAs removed from F1 track during automated outlier removal: 45"
#> [1] "Number of NAs removed from F2 track during automated outlier removal: 24"
#> [1] "Number of NAs removed from F3 track during automated outlier removal: 23"
#> [1] "Number of NAs removed from H1A1c track during automated outlier removal: 526"
#> [1] "Number of NAs removed from H1A3c track during automated outlier removal: 521"
#> [1] "Number of NAs removed from eggF0 track during automated outlier removal: 15"
y
#> # A tibble: 2,823 × 73
#>    sl_rowIdx labels start   end db_uuid     session bundle speaker repet…¹ word 
#>        <int> <chr>  <dbl> <dbl> <chr>       <chr>   <chr>  <chr>   <chr>   <chr>
#>  1         1 cl     1134. 1235. 33157d9f-4… f1      F1-00… EF1     1       bOh  
#>  2         1 cl     1134. 1235. 33157d9f-4… f1      F1-00… EF1     1       bOh  
#>  3         1 cl     1134. 1235. 33157d9f-4… f1      F1-00… EF1     1       bOh  
#>  4         1 cl     1134. 1235. 33157d9f-4… f1      F1-00… EF1     1       bOh  
#>  5         1 cl     1134. 1235. 33157d9f-4… f1      F1-00… EF1     1       bOh  
#>  6         1 cl     1134. 1235. 33157d9f-4… f1      F1-00… EF1     1       bOh  
#>  7         1 cl     1134. 1235. 33157d9f-4… f1      F1-00… EF1     1       bOh  
#>  8         1 cl     1134. 1235. 33157d9f-4… f1      F1-00… EF1     1       bOh  
#>  9         1 cl     1134. 1235. 33157d9f-4… f1      F1-00… EF1     1       bOh  
#> 10         1 cl     1134. 1235. 33157d9f-4… f1      F1-00… EF1     1       bOh  
#> # … with 2,813 more rows, 63 more variables: start_item_id <int>,
#> #   end_item_id <int>, level <chr>, attribute <chr>, start_item_seq_idx <int>,
#> #   end_item_seq_idx <int>, type <chr>, sample_start <int>, sample_end <int>,
#> #   sample_rate <int>, ov <dbl>, cv <dbl>, rv <dbl>, onset <chr>, vowel <chr>,
#> #   coda <chr>, template <chr>, presyllable <chr>, register <chr>,
#> #   times_orig <dbl>, times_rel <dbl>, times_norm <dbl>, eggF0 <dbl>,
#> #   H1H2c <dbl>, H1A1c <dbl>, H1A3c <dbl>, CPP <dbl>, CQ_PH <dbl>, …
dplyr::glimpse(y)
#> Rows: 2,823
#> Columns: 73
#> $ sl_rowIdx          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ labels             <chr> "cl", "cl", "cl", "cl", "cl", "cl", "cl", "cl", "cl…
#> $ start              <dbl> 1134.002, 1134.002, 1134.002, 1134.002, 1134.002, 1…
#> $ end                <dbl> 1234.887, 1234.887, 1234.887, 1234.887, 1234.887, 1…
#> $ db_uuid            <chr> "33157d9f-4a3a-468a-882b-60d3b10ea771", "33157d9f-4…
#> $ session            <chr> "f1", "f1", "f1", "f1", "f1", "f1", "f1", "f1", "f1…
#> $ bundle             <chr> "F1-0002-car-rep1-bOh-18", "F1-0002-car-rep1-bOh-18…
#> $ speaker            <chr> "EF1", "EF1", "EF1", "EF1", "EF1", "EF1", "EF1", "E…
#> $ repetition         <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "…
#> $ word               <chr> "bOh", "bOh", "bOh", "bOh", "bOh", "bOh", "bOh", "b…
#> $ start_item_id      <int> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, …
#> $ end_item_id        <int> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, …
#> $ level              <chr> "ORL", "ORL", "ORL", "ORL", "ORL", "ORL", "ORL", "O…
#> $ attribute          <chr> "ORL", "ORL", "ORL", "ORL", "ORL", "ORL", "ORL", "O…
#> $ start_item_seq_idx <int> 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, …
#> $ end_item_seq_idx   <int> 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, …
#> $ type               <chr> "SEGMENT", "SEGMENT", "SEGMENT", "SEGMENT", "SEGMEN…
#> $ sample_start       <int> 50010, 50010, 50010, 50010, 50010, 50010, 50010, 50…
#> $ sample_end         <int> 54458, 54458, 54458, 54458, 54458, 54458, 54458, 54…
#> $ sample_rate        <int> 44100, 44100, 44100, 44100, 44100, 44100, 44100, 44…
#> $ ov                 <dbl> 1134.014, 1134.014, 1134.014, 1134.014, 1134.014, 1…
#> $ cv                 <dbl> 1185.329, 1185.329, 1185.329, 1185.329, 1185.329, 1…
#> $ rv                 <dbl> 1241.066, 1241.066, 1241.066, 1241.066, 1241.066, 1…
#> $ onset              <chr> "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "…
#> $ vowel              <chr> "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "…
#> $ coda               <chr> "h", "h", "h", "h", "h", "h", "h", "h", "h", "h", "…
#> $ template           <chr> "bOh", "bOh", "bOh", "bOh", "bOh", "bOh", "bOh", "b…
#> $ presyllable        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ register           <chr> "low", "low", "low", "low", "low", "low", "low", "l…
#> $ times_orig         <dbl> 1135, 1136, 1137, 1138, 1139, 1140, 1141, 1142, 114…
#> $ times_rel          <dbl> -99, -98, -97, -96, -95, -94, -93, -92, -91, -90, -…
#> $ times_norm         <dbl> 0.00000000, 0.01010101, 0.02020202, 0.03030303, 0.0…
#> $ eggF0              <dbl> 145.9689, 145.9689, 145.9689, 145.9689, 145.9689, 1…
#> $ H1H2c              <dbl> 7.146, 6.966, 6.906, 6.942, 6.558, 7.199, 7.933, 8.…
#> $ H1A1c              <dbl> 13.184, 13.637, 13.875, 14.011, 13.831, 14.256, 14.…
#> $ H1A3c              <dbl> 18.417, 19.378, 20.357, 21.405, 22.068, 21.353, 19.…
#> $ CPP                <dbl> 23.326, 20.667, 16.525, 11.272, 12.012, 11.597, 12.…
#> $ CQ_PH              <dbl> 0.5798880, 0.5798880, 0.5798880, 0.5798880, 0.57988…
#> $ CQ_PD              <dbl> 0.6028057, 0.6028057, 0.6028057, 0.6028057, 0.60280…
#> $ F0                 <dbl> 177.128, 174.929, 172.729, 170.530, 168.330, 166.72…
#> $ uppF0              <dbl> 242.1947, 242.1947, 242.1947, 242.1947, 242.1947, 2…
#> $ lowF0              <dbl> 152.0185, 152.0185, 152.0185, 152.0185, 152.0185, 1…
#> $ zF0                <dbl> -1.229407, -1.286866, -1.344352, -1.401811, -1.4592…
#> $ normF0             <dbl> 110.08380, 106.98953, 103.89387, 100.79961, 97.7039…
#> $ zH1H2c             <dbl> -0.39531513, -0.43693926, -0.45081397, -0.44248919,…
#> $ normH1H2c          <dbl> 4.950326, 4.748684, 4.681470, 4.721798, 4.291629, 5…
#> $ F1                 <dbl> 504.725, 501.468, 498.210, 494.952, 491.694, 484.35…
#> $ uppF1              <dbl> 1456.536, 1456.536, 1456.536, 1456.536, 1456.536, 1…
#> $ lowF1              <dbl> -87.74296, -87.74296, -87.74296, -87.74296, -87.742…
#> $ zF1                <dbl> 0.025927425, 0.009327376, -0.007277649, -0.02388267…
#> $ normF1             <dbl> 474.2616, 471.0387, 467.8148, 464.5909, 461.3670, 4…
#> $ F2                 <dbl> 974.190, 996.762, 1019.334, 1041.906, 1064.478, 109…
#> $ uppF2              <dbl> 2649.806, 2649.806, 2649.806, 2649.806, 2649.806, 2…
#> $ lowF2              <dbl> 40.438, 40.438, 40.438, 40.438, 40.438, 40.438, 40.…
#> $ zF2                <dbl> -0.8744832, -0.8310210, -0.7875590, -0.7440968, -0.…
#> $ normF2             <dbl> 918.0967, 942.0886, 966.0804, 990.0722, 1014.0641, …
#> $ F3                 <dbl> 3179.065, 3179.028, 3178.991, 3178.953, 3178.916, 3…
#> $ uppF3              <dbl> 3755.745, 3755.745, 3755.745, 3755.745, 3755.745, 3…
#> $ lowF3              <dbl> 2677.057, 2677.057, 2677.057, 2677.057, 2677.057, 2…
#> $ zF3                <dbl> 0.54680526, 0.54668843, 0.54657082, 0.54645012, 0.5…
#> $ normF3             <dbl> 3054.735, 3054.696, 3054.657, 3054.617, 3054.578, 3…
#> $ zH1A1c             <dbl> 0.17247494, 0.25130790, 0.29272564, 0.31639287, 0.2…
#> $ normH1A1c          <dbl> 12.582032, 13.074322, 13.332965, 13.480760, 13.2851…
#> $ zH1A3c             <dbl> 0.64491675, 0.75627665, 0.86972234, 0.99116372, 1.0…
#> $ normH1A3c          <dbl> 17.068598, 18.190177, 19.332763, 20.555878, 21.3296…
#> $ uppeggF0           <dbl> 372.6801, 372.6801, 372.6801, 372.6801, 372.6801, 3…
#> $ loweggF0           <dbl> -64.50195, -64.50195, -64.50195, -64.50195, -64.501…
#> $ zCPP               <dbl> 1.101800046, 0.672774668, 0.004469691, -0.843093181…
#> $ normCPP            <dbl> 23.34715, 20.88814, 17.05768, 12.19979, 12.88413, 1…
#> $ zCQ_PH             <dbl> 0.4458473, 0.4458473, 0.4458473, 0.4458473, 0.44584…
#> $ normCQ_PH          <dbl> 0.5749189, 0.5749189, 0.5749189, 0.5749189, 0.57491…
#> $ zCQ_PD             <dbl> 0.1340909, 0.1340909, 0.1340909, 0.1340909, 0.13409…
#> $ normCQ_PD          <dbl> 0.5850945, 0.5850945, 0.5850945, 0.5850945, 0.58509…
```

Notice that the `praatF0`, `praatF1` columns etc. have been renamed to
`F0`, `F1`. Notice also that for each SSFF track has a corresponding
column with z-score normalized values (e.g. `zF1`) *and* a corresponding
column where these normalized values have been rescaled based on the
overall mean and standard deviation of the data (e.g. `normF1`).

`import_ssfftracks()` is very dependent on `emuR` and EMU-SDMS, but it
incorporates several independent functions which can in principle be
used on raw data generated with other software: `f0_proc()` for
processing F0 and dependencies, `fn_proc()` for processing formants and
dependencies, `outlier_rm` for automated removal of outliers, and
`normz` for z-score normalizing and rescaling by speaker. The syntax of
these functions is similar to `import_ssfftracks()`.

## Installation

You can install the development version of `emuhelpeR` from GitHub with:

``` r
# install.packages("devtools")
# devtools::install_github("rpuggaardrode/emuhelpeR")
```

## References

Kirby, James, Marc Brunelle & Pittayawat Pittayaporn (forthc.)
Transphonologization of onset voicing: Revisiting Northern and Eastern
Kmhmu. (To be published in *Phonetica*.). DOI:
[10.17605/OSF.IO/WV6QZ](https://doi.org/10.17605/OSF.IO/WV6QZ).