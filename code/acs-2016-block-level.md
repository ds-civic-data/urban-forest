ACS 2016
================
Frank Gaunt
4/19/2018

a

``` r
library(readr)
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 2.2.1     ✔ purrr   0.2.4
    ## ✔ tibble  1.4.1     ✔ dplyr   0.7.4
    ## ✔ tidyr   0.7.2     ✔ stringr 1.3.0
    ## ✔ ggplot2 2.2.1     ✔ forcats 0.2.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
acs2016block <- read_csv("~/urban-forest/data-raw/R11666747_SL150-2.csv") %>%
  select(`Qualifying Name`, `Total Population`:`% Workers 16 Years and Over: Worked At Home`) %>%
  filter(`Occupied Housing Units` != "SE_T166_001") %>%
  rename("median_rent" = `Median Gross Rent`,
         "total_population" = `Total Population`,
         "population_density" = `Population Density (Per Sq. Mile)`)
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

``` r
acs2016blocktidy1 <- acs2016block %>%
  rename("white" = `% Total Population: White Alone`,
         "black" = `% Total Population: Black or African American Alone`,
         "am_indian" = `% Total Population: American Indian and Alaska Native Alone`,
         "asian" = `% Total Population: Asian Alone`,
         "pac_isl" = `% Total Population: Native Hawaiian and Other Pacific Islander Alone`,
         "other" = `% Total Population: Some Other Race Alone`,
         "two_more" = `% Total Population: Two or More Races`) %>%
  gather(white:two_more, key = "race", value = "percent_population") %>%
  select(`Qualifying Name`, race, percent_population)

acs2016blocktidy2 <- acs2016block %>%
  rename("1_person" = `% Occupied Housing Units: 1-Person Household`,
         "2_person" = `% Occupied Housing Units: 2-Person Household`,
         "3_person" = `% Occupied Housing Units: 3-Person Household`,
         "4_person" = `% Occupied Housing Units: 4-Person Household`,
         "5_person" = `% Occupied Housing Units: 5-Person Household`,
         "6_person" = `% Occupied Housing Units: 6-Person Household`,
         "7_more_person" = `% Occupied Housing Units: 7-or-More Person Household`) %>%
  gather(`1_person`:`7_more_person`, key = "household_size", value = "percent_occupied_housing_units") %>%
  select(`Qualifying Name`, household_size, percent_occupied_housing_units)

acs2016blocktidy3 <- acs2016block %>%
  rename("below_hs" = `% Population 25 Years and Over: Less than High School`,
         "hs_degree" = `% Population 25 Years and Over: High School Graduate (Includes Equivalency)`,
         "some_college" = `% Population 25 Years and Over: Some College`,
         "bachelors_degree" = `% Population 25 Years and Over: Bachelor's Degree`,
         "masters_degree" = `% Population 25 Years and Over: Master's Degree`,
         "professional_school_degree" = `% Population 25 Years and Over: Professional School Degree`,
         "doctorate_degree" = `% Population 25 Years and Over: Doctorate Degree`) %>%
  gather(`below_hs`:`doctorate_degree`, key = "education_level", value = "percent_population") %>%
  select(`Qualifying Name`, education_level, percent_population)

acs2016blocktidy4 <- acs2016block %>%
  rename("employed" = `% Civilian Population in Labor Force 16 Years and Over: Employed`,
         "unemployed" = `% Civilian Population in Labor Force 16 Years and Over: Unemployed`) %>%
  gather(employed:unemployed, key = "employment_status", value = "percent_population") %>%
  select(`Qualifying Name`, employment_status, percent_population)

acs2016blocktidy5 <- acs2016block %>%
  rename("less_10k" = `% Households: Less than $10,000`,
         "10k_14999" = `% Households: $10,000 to $14,999`,
         "15k_19999" = `% Households: $15,000 to $19,999`,
         "20k_24999" = `% Households: $20,000 to $24,999`,
         "25k_29999" = `% Households: $25,000 to $29,999`,
         "30k_34999" = `% Households: $30,000 to $34,999`,
         "35k_39999" = `% Households: $35,000 to $39,999`,
         "40k_44999" = `% Households: $40,000 to $44,999`,
         "45k_49999" = `% Households: $45,000 to $49,999`,
         "50k_59999" = `% Households: $50,000 to $59,999`,
         "60k_74999" = `% Households: $60,000 to $74,999`,
         "75k_99999" = `% Households: $75,000 to $99,999`,
         "100k_124999" = `% Households: $100,000 to $124,999`,
         "125k_149999" = `% Households: $125,000 to $149,999`,
         "150k_199999" = `% Households: $150,000 to $199,999`,
         "200k_more" = `% Households: $200,000 or More`) %>%
  gather(`less_10k`:`200k_more`, key = "household_income", value = "percent_households") %>%
  select(`Qualifying Name`, household_income, percent_households)

acs2016blocktidy6 <- acs2016block %>%
  rename("earnings" = `% Households: with Earnings`,
         "no_earnings" = `% Households: No Earnings`) %>%
  gather(earnings:no_earnings, key = "earnings", value = "percent_households") %>%
  select(`Qualifying Name`, earnings, percent_households)

acs2016blocktidy7 <- acs2016block %>%
  rename("family" = `Median Household Income (In 2016 Inflation Adjusted Dollars)`,
         "nonfamily" = `Median Nonfamily Household Income (In 2016 Inflation Adjusted Dollars)`) %>%
  gather(family:nonfamily, key = "income_type", value = "median_income") %>%
  select(`Qualifying Name`, income_type, median_income)

acs2016blocktidy8 <- acs2016block %>%
  rename("capital" = `% Households: with Interest, Dividends, or Net Rental Income`,
         "social_security" = `% Households: with Social Security Income`,
         "ssi" = `% Households: with Supplemental Security Income (Ssi)`,
         "public_assistance" = `% Households: with Public Assistance Income`,
         "retirement" = `% Households: with Retirement Income`,
         "other" = `% Households: with Other Types of Income`) %>%
  select(`Qualifying Name`, capital, social_security, ssi, public_assistance, retirement, other) %>%
  gather(`capital`:`other`, key = "income_type", value = "percent_households") %>%
  select(`Qualifying Name`, income_type, percent_households)

acs2016blocktidy9 <- acs2016block %>%
  rename("renter_occupied" = `% Occupied Housing Units: Renter Occupied`,
         "owner_occupied" = `% Occupied Housing Units: Owner Occupied`) %>%
  gather(owner_occupied:renter_occupied, key = "property_type", value = "percent_housing_units") %>%
  select(`Qualifying Name`, property_type, percent_housing_units)
```

acs2016blocktidy10 &lt;- acs2016block %&gt;% rename("10\_less" = `% Workers 16 Years and Over: Did Not Work At Home: Less than 10 Minutes`, "10\_19" = `% Workers 16 Years and Over: Did Not Work At Home: 10 to 19 Minutes`, "20\_29" = `% Workers 16 Years and Over: Did Not Work At Home: 20 to 29 Minutes`, "30\_39" = `% Workers 16 Years and Over: Did Not Work At Home: 30 to 39 Minutes`, "40\_59" = `% Workers 16 Years and Over: Did Not Work At Home: 40 to 59 Minutes`, "60\_89" = `% Workers 16 Years and Over: Did Not Work At Home: 60 to 89 Minutes`, "90\_more" = `% Workers 16 Years and Over: Did Not Work At Home: 90 or More Minutes`, "none\_home" = `% Workers 16 Years and Over: Worked At Home`) %&gt;% gather(`10_less`:`none_home`, key = "commute\_time", value = "percent\_population") %&gt;% select(`Qualifying Name`, commute\_time, percent\_population) \`\`\`
