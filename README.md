
<!-- README.md is generated from README.Rmd. Please edit that file -->

# trackr

<!-- badges: start -->
<!-- badges: end -->

## Overview

**trackr** extends dplyr to make it easier to see what changes are being made when running dplyr functions. It provides wrapper functions that print diagnostic information about data transformations while preserving the original dplyr workflow. This is particularly useful during data cleaning and exploratory analysis to understand the impact of each operation.

## Installation

You can install the development version of trackr from [GitHub](https://github.com/dmbwebb/trackr) with:

``` r
# install.packages("devtools")
devtools::install_github("dmbwebb/trackr")
```

## Functions

### Data Manipulation with Tracking

#### `filter_track()`
Filters data and prints a summary showing how many observations were kept, removed, and the total count with proportions.

#### `mutate_track()`
Applies mutations to variables and prints information about what changed, including the number of changes made and how many values were changed to NA for each modified variable.

#### `drop_na_track()`
Drops rows with missing values and displays a summary of how many observations were kept, removed, and the total count with proportions.

### Join Operations with Diagnostics

#### `full_join_track()`, `left_join_track()`, `right_join_track()`, `inner_join_track()`
Perform various types of joins while printing diagnostic information about the merge, including counts and proportions of rows that matched, came only from the left dataset, or only from the right dataset. All functions support an optional `.merge` parameter to add a merge status variable to the output.

### Viewing and Sampling

#### `view_n()`
Opens a viewer window with a random sample of n rows (or n groups if data is grouped). Useful for quickly inspecting large datasets.

#### `view_filter()`
Applies a filter and then opens a viewer with a random sample of the filtered data.

#### `view_select()`
Selects specific variables and then opens a viewer with a random sample of the data.

#### `sample_n_groups()`
Randomly samples n groups from a grouped data frame, or n rows from an ungrouped data frame.

### Counting and Frequency Tables

#### `count_nas()`
Counts missing values for each variable in a dataset, showing both the count and proportion of NAs. Can be sorted by proportion missing.

#### `count_print()`
Creates a count table and prints it, then invisibly returns the original data for use in pipelines.

#### `count_prop()`
Creates a frequency table with proportions and prints it, then invisibly returns the original data for use in pipelines.

### Utility Functions

#### `print_all()`
Prints all rows of a tibble without truncation.

#### `print_names()`
Prints all column names in a numbered list.

#### `check_row_diff()`
Compares two consecutive rows (k and k+1) and displays which variables differ between them and their values.

#### NA-safe aggregation functions
- `first_non_na()`, `last_non_na()`: Get first/last non-NA value from a vector
- `mean_na()`, `median_na()`, `sum_na()`, `min_na()`, `max_na()`: Calculate summary statistics that return NA if there are no non-missing values to calculate from (instead of errors or warnings)

#### `replace_with_na()`
Replaces specified values in a vector with NA.

## Example

``` r
library(trackr)
library(dplyr)

# Filter with tracking
mtcars %>%
  filter_track(mpg > 20)
#> # A tibble: 3 × 3
#>   obs_type      n  prop
#>   <chr>     <int> <dbl>
#> 1 kept         14 0.438
#> 2 removed      18 0.562
#> 3 total        32 1

# Mutate with tracking
mtcars %>%
  mutate_track(mpg = ifelse(mpg > 20, NA, mpg))
#> [1] "mpg: 14 changes made, 14 rows changed to NA"

# Join with tracking
left_join_track(band_members, band_instruments, by = "name")
#> # A tibble: 3 × 3
#>   merge_status     n  prop
#>   <chr>        <int> <dbl>
#> 1 x_only           1 0.333
#> 2 y_only           0 0
#> 3 matched          2 0.667
```

## Links

- GitHub: https://github.com/dmbwebb/trackr
- Issues: https://github.com/dmbwebb/trackr/issues
