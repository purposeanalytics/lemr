
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lemur

CMHC Housing Supply Challenge Proof of Concept

## Installation

You can install lemur from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("purposeanalytics/lemur")
```

## Usage

To load the app, run:

``` r
lemur::run_app()
```

lemur also contains a utility function for geocoding addresses using the
Bing geocoder:

``` r
library(lemur)
geocode_address("220 Yonge St Toronto ON")
#> ℹ Fetching 220 Yonge St Toronto ON - Status: 200
#> # A tibble: 1 x 8
#>   bing_status_code bing_address bing_municipality bing_postal_code bing_method
#>              <int> <chr>        <chr>             <chr>            <chr>      
#> 1              200 220 Yonge St Toronto           M5B 2H1          Rooftop    
#> # … with 3 more variables: bing_confidence <chr>, bing_latitude <dbl>,
#> #   bing_longitude <dbl>
```

This looks up any address in Canada, so it’s helpful to supply the city
or postal code for the most accurate results - for example, 220 Yonge St
without “Toronto ON” gives us somewhere on an island in Lake Huron:

``` r
geocode_address("220 Yonge St")
#> ℹ Fetching 220 Yonge St - Status: 200
#> # A tibble: 1 x 8
#>   bing_status_code bing_address bing_municipality bing_postal_code bing_method  
#>              <int> <chr>        <chr>             <chr>            <chr>        
#> 1              200 220 Yonge St Manitoulin        P0P              Interpolatio…
#> # … with 3 more variables: bing_confidence <chr>, bing_latitude <dbl>,
#> #   bing_longitude <dbl>
```

An API token is required, and the function looks for it in the
`BING_TOKEN` environment variable, buit this can be changed or supplied
directly via the `token` argument.
