ACS 2016
================
Frank Gaunt
4/19/2018

a

``` r
library(readr)
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.4.2

    ## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 2.2.1     ✔ purrr   0.2.4
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.4
    ## ✔ tidyr   0.8.0     ✔ stringr 1.3.0
    ## ✔ ggplot2 2.2.1     ✔ forcats 0.3.0

    ## Warning: package 'tibble' was built under R version 3.4.3

    ## Warning: package 'tidyr' was built under R version 3.4.3

    ## Warning: package 'purrr' was built under R version 3.4.2

    ## Warning: package 'dplyr' was built under R version 3.4.2

    ## Warning: package 'stringr' was built under R version 3.4.3

    ## Warning: package 'forcats' was built under R version 3.4.3

    ## ── Conflicts ────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
acs2016block <- read_csv("~/urban-forest/data-raw/R11666747_SL150-2.csv") %>%
 # select(`Qualifying Name`, `Total Population`:`% Workers 16 Years and Over: Worked At Home`) %>%
  filter(`Occupied Housing Units` != "SE_T166_001") %>%
  rename("median_rent" = `Median Gross Rent`,
         "total_population" = `Total Population`,
         "population_density" = `Population Density (Per Sq. Mile)`,
         "area" = `Area (Land)_1`,
         "qualifying_name" = `Qualifying Name`,
         "fips" = `FIPS`,
         "census_tract" = `Census Tract`)
```

    ## Warning: Duplicated column names deduplicated: 'Area (Land)' => 'Area
    ## (Land)_1' [60], 'Households:' => 'Households:_1' [146], 'Households:'
    ## => 'Households:_2' [151], 'Households:' => 'Households:_3' [156],
    ## 'Households:' => 'Households:_4' [161], 'Households:' =>
    ## 'Households:_5' [166], 'Households:' => 'Households:_6' [171],
    ## 'Households:' => 'Households:_7' [176], 'Households:' =>
    ## 'Households:_8' [181], 'Households:' => 'Households:_9' [186], 'Workers
    ## 16 Years and Over:' => 'Workers 16 Years and Over:_1' [217], 'Workers 16
    ## Years and Over: Worked At Home' => 'Workers 16 Years and Over: Worked At
    ## Home_1' [226], '% Workers 16 Years and Over: Worked At Home' => '% Workers
    ## 16 Years and Over: Worked At Home_1' [235]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character()
    ## )

    ## See spec(...) for full column specifications.

    ## Warning: package 'bindrcpp' was built under R version 3.4.4

``` r
# Note: Everything in Percentages ||| hhs = households, otherwise = overall population ||| hs = high school
tidyblock2016 <- acs2016block %>%
  rename("white" = `% Total Population: White Alone`,
         "black" = `% Total Population: Black or African American Alone`,
         "am_indian" = `% Total Population: American Indian and Alaska Native Alone`,
         "asian" = `% Total Population: Asian Alone`,
         "pac_isl" = `% Total Population: Native Hawaiian and Other Pacific Islander Alone`,
         "other_race" = `% Total Population: Some Other Race Alone`,
         "two_more_races" = `% Total Population: Two or More Races`,
         "below_hs" = `% Population 25 Years and Over: Less than High School`,
         "hs_degree" = `% Population 25 Years and Over: High School Graduate (Includes Equivalency)`,
         "some_college" = `% Population 25 Years and Over: Some College`,
         "bachelors_degree" = `% Population 25 Years and Over: Bachelor's Degree`,
         "masters_degree" = `% Population 25 Years and Over: Master's Degree`,
         "professional_school_degree" = `% Population 25 Years and Over: Professional School Degree`,
         "doctorate_degree" = `% Population 25 Years and Over: Doctorate Degree`,
         "unemployment_rate" = `% Civilian Population in Labor Force 16 Years and Over: Unemployed`,
         "hhs_less_10k" = `% Households: Less than $10,000`,
         "hhs_10k_14999" = `% Households: $10,000 to $14,999`,
         "hhs_15k_19999" = `% Households: $15,000 to $19,999`,
         "hhs_20k_24999" = `% Households: $20,000 to $24,999`,
         "hhs_25k_29999" = `% Households: $25,000 to $29,999`,
         "hhs_30k_34999" = `% Households: $30,000 to $34,999`,
         "hhs_35k_39999" = `% Households: $35,000 to $39,999`,
         "hhs_40k_44999" = `% Households: $40,000 to $44,999`,
         "hhs_45k_49999" = `% Households: $45,000 to $49,999`,
         "hhs_50k_59999" = `% Households: $50,000 to $59,999`,
         "hhs_60k_74999" = `% Households: $60,000 to $74,999`,
         "hhs_75k_99999" = `% Households: $75,000 to $99,999`,
         "hhs_100k_124999" = `% Households: $100,000 to $124,999`,
         "hhs_125k_149999" = `% Households: $125,000 to $149,999`,
         "hhs_150k_199999" = `% Households: $150,000 to $199,999`,
         "hhs_200k_more" = `% Households: $200,000 or More`,
         "hhs_with_no_earnings" = `% Households: No Earnings`,
         "hhs_capital_income" = `% Households: with Interest, Dividends, or Net Rental Income`,
         "hhs_social_security" = `% Households: with Social Security Income`,
         "hhs_ssi" = `% Households: with Supplemental Security Income (Ssi)`,
         "hhs_public_assistance_income" = `% Households: with Public Assistance Income`,
         "hhs_retirement_income" = `% Households: with Retirement Income`,
         "med_family_income" = `Median Household Income (In 2016 Inflation Adjusted Dollars)`,
         "med_nonfamily_income" = `Median Nonfamily Household Income (In 2016 Inflation Adjusted Dollars)`,
         "renter_occupied_units" = `% Occupied Housing Units: Renter Occupied`,
         "owner_occupied_units" = `% Occupied Housing Units: Owner Occupied`,
         "10_less_commute" = `% Workers 16 Years and Over: Did Not Work At Home: Less than 10 Minutes`,
         "10_19_commute" = `% Workers 16 Years and Over: Did Not Work At Home: 10 to 19 Minutes`,
         "20_29_commute" = `% Workers 16 Years and Over: Did Not Work At Home: 20 to 29 Minutes`,
         "30_39_commute" = `% Workers 16 Years and Over: Did Not Work At Home: 30 to 39 Minutes`,
         "40_59_commute" = `% Workers 16 Years and Over: Did Not Work At Home: 40 to 59 Minutes`,
         "60_89_commute" = `% Workers 16 Years and Over: Did Not Work At Home: 60 to 89 Minutes`,
         "90_more_commute" = `% Workers 16 Years and Over: Did Not Work At Home: 90 or More Minutes`,
         "no_commute" = `% Workers 16 Years and Over: Worked At Home`) %>%
  select(qualifying_name, fips, census_tract, total_population, population_density, area, white:two_more_races, below_hs:doctorate_degree, unemployment_rate, hhs_less_10k:hhs_200k_more, med_family_income, med_nonfamily_income, hhs_with_no_earnings, hhs_capital_income, hhs_social_security, hhs_ssi, hhs_public_assistance_income, hhs_retirement_income, renter_occupied_units, owner_occupied_units, `10_less_commute`:no_commute)

tidyblock2016[, c(2:54)] <- sapply(tidyblock2016[, c(2:54)], as.numeric)

write_csv(tidyblock2016, "tidyblock2016.csv")
```
