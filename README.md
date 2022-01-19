# assessment-shiny-app
A shiny app that can perform simple claim development using development triangles and plot the results

This app will take in 1 csv file of the following long format:
Column 1: Loss Years
Column 2: Development Years
Column 3: Claims Paid that year
The order and the column names of the data shouldn't matter, as long as the data entry follows the columns specified above.

Problems:
1) It is possible for 0 claims to happen on a particular development year, this shouldn't affect the app but if the 0 happens in the first year, then the app will not work as intended. Perhaps a workaround will be to replace the 0 with like 0.1 or some immaterial values.

2) The plot is not exactly smooth, I decided to use geom_line() instead of geom_smooth() because the latter, although can plot, creates a bunch of error in the console.

