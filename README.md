# gtrends
some simple code to stitch together longer term search term trends data 
This code uses the gtrends R package to download and stitch together long time series data on seach term trends. The code as is collects trends on a number of phrases pertaining to the housing market, but can be modified to look for any number of phrases.  

The final output is a time series csv of the search term, normalized over time. This method is achieved by taking the trend from overlapping time periods, and applying that to an initial series, enabling longer data extracts than are allowed directly from the package.  

Comments and PRs welcome!
