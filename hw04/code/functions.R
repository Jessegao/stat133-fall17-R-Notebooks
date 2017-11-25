
# Remove Missing
# Arguments: vector v
# Description: Takes a vector and returns the input vector without missing values.
# Output: vector 
remove_missing = function(v) {
  return(v[!is.na(v)])
}

# Get Minimum
# Arguments: a numeric vector v and an optional logical na.rm argument
# Description: takes a numeric vector, and an optional logical na.rm argument to find the minimum value.
#              na.rm tells the function to remove the as.double(NA)'s
# Output: numeric value
get_minimum = function(v, na.rm) {
  if (! is.numeric(v)) {
    return("non-numeric argument")
  }
  if (length(v) == 0){
    return("vector is empty")
  }
  if (na.rm) {
    v = remove_missing(v)
  }
  min_val = v[1]
  for(i in v) {
    if(is.na(i)) {
      return(as.double(NA))
    }
    if(i < min_val) {
      min_val = i
    }
  }
  return(min_val)
}

# Get Maximum
# Arguments: a numeric vector v and an optional logical na.rm argument
# Description: takes a numeric vector, and an optional logical na.rm argument, to find the maximum value
#             na.rm tells the function to remove the as.double(NA)'s
# Output: numeric value
get_maximum = function(v, na.rm){
  if (! is.numeric(v)) {
    return("non-numeric argument")
  }
  if (length(v) == 0){
    return("vector is empty")
  }
  if (na.rm) {
    v = remove_missing(v)
  }
  max_val = v[1]
  for(i in v) {
    if(i > max_val) {
      max_val = i
    }
  }
  return(max_val)
}

# Get Range
# Arguments: a numeric vector v and an optional logical na.rm argument
# Description: takes a numeric vector, and an optional logical na.rm argument, 
#              to compute the overall range of the input vector
# Output: numeric value
get_range = function(v, na.rm) {
  if (! is.numeric(v)) {
    return("non-numeric argument")
  }
  if (length(v) == 0){
    return("vector is empty")
  }
  if (na.rm) {
    v = remove_missing(v)
  }
  return(get_maximum(v, na.rm = TRUE) - get_minimum(v, na.rm = TRUE))
}

# Get 10th percentile
# Arguments: a numeric vector v and an optional logical na.rm argument
# Description: computes the 10th percentile of the input vector
# Output: numeric value
get_percentile10 = function(v, na.rm) {
  if (! is.numeric(v)) {
    return("non-numeric argument")
  }
  if (length(v) == 0){
    return("vector is empty")
  }
  if (na.rm) {
    v = remove_missing(v)
  }else if(length(remove_missing(v) < length(v))) {
    return(as.double(NA))
  }
  return(quantile(v, c(.10))[[1]])
}

# Get 90th percentile
# Arguments: a numeric vector v and an optional logical na.rm argument
# Description: computes the 90th percentile of the input vector
# Output: numeric value
get_percentile90 = function(v, na.rm) {
  if (! is.numeric(v)) {
    return("non-numeric argument")
  }
  if (length(v) == 0){
    return("vector is empty")
  }
  if (na.rm) {
    v = remove_missing(v)
  }else if(length(remove_missing(v) < length(v))) {
    return(as.double(NA))
  }
  return(quantile(v, c(.90))[[1]])
}

# Get First Quartile
# Arguments: a numeric vector v and an optional logical na.rm argument
# Description: computes the first quartile of the input vector
# Output: numeric value
get_quartile1 = function(v, na.rm) {
  if (! is.numeric(v)) {
    return("non-numeric argument")
  }
  if (length(v) == 0){
    return("vector is empty")
  }
  if (na.rm) {
    v = remove_missing(v)
  }else if(length(remove_missing(v) < length(v))) {
    return(as.double(NA))
  }
  return(quantile(v, c(.25))[[1]])
}

# Get Third Quartile
# Arguments: a numeric vector v and an optional logical na.rm argument
# Description: computes the third quartile of the input vector
# Output: numeric value
get_quartile3 = function(v, na.rm) {
  if (! is.numeric(v)) {
    return("non-numeric argument")
  }
  if (length(v) == 0){
    return("vector is empty")
  }
  if (na.rm) {
    v = remove_missing(v)
  }else if(length(remove_missing(v) < length(v))) {
    return(as.double(NA))
  }
  return(quantile(v, c(.75))[[1]])
}

# Get Median
# Arguments: a numeric vector v and an optional logical na.rm argument
# Description: takes a numeric vector, and an optional logical na.rm argument, to
#              compute the median of the input vector
# Output: numeric value
get_median = function(v, na.rm) {
  if (! is.numeric(v)) {
    return("non-numeric argument")
  }
  if (length(v) == 0){
    return("vector is empty")
  }
  if (na.rm) {
    v = remove_missing(v)
  } else if(length(remove_missing(v) < length(v))) {
    return(as.double(NA))
  }
  return(quantile(v, c(.50))[[1]])
}

# Get Average
# Arguments: a numeric vector v and an optional logical na.rm argument
# Description: Computes the mean of the input vector
# Output: numeric value
get_average = function(v, na.rm) {
  if (! is.numeric(v)) {
    return("non-numeric argument")
  }
  if (na.rm) {
    v = remove_missing(v)
  }
  sum = 0
  for(i in v) {
    sum = sum + i
  }
  return(sum/length(v))
}

# Get Standard Deviation
# Arguments: a numeric vector v and an optional logical na.rm argument
# Description: 
# Output: numeric value
get_stdev = function(v, na.rm) {
  if (! is.numeric(v)) {
    return("non-numeric argument")
  }
  if (na.rm) {
    v = remove_missing(v)
  }
  avg = get_average(v, na.rm = FALSE)
  sum = 0
  for(i in v) {
    sum = sum + (i - avg)**2
  }
  return(sqrt((1/(length(v)-1)) * sum))
}

# Count Missing
# Arguments: numeric vector
# Description:  calculates the number of missing values as.double(NA)
# Output: numeric value
count_missing = function(v) {
  return(length(v[is.na(v)]))
}

# Summmary Stats
# Arguments: numeric vector v
# Description: takes a numeric vector, and returns a list of summary statistics
# Output: list
summary_stats = function(v) {
  newV = list(minimum = get_minimum(v, na.rm = TRUE),
              percent10 = get_percentile10(v, na.rm = TRUE),
              quartile1 = get_quartile1(v, na.rm = TRUE),
              median = get_median(v, na.rm = TRUE),
              mean = get_average(v, na.rm = TRUE),
              quartile3 = get_quartile3(v, na.rm = TRUE),
              percent90 = get_percentile90(v, na.rm = TRUE),
              maximum = get_maximum(v, na.rm = TRUE),
              range = get_range(v, na.rm = TRUE),
              stdev = get_stdev(v, na.rm = TRUE),
              missing = count_missing(v))
  return(newV)
}

# Print Stats
# Arguments: list of summary statistics
# Description: t takes a list of summary statistics, and prints the values in a nice format
# Output: printed statements
print_stats = function(stats){
  maxNameLength = 9
  formatedNames = c("minimum  :",
                    "percent10:",
                    "quartile1:",
                    "median   :",
                    "mean     :",
                    "quartile3:",
                    "percent90:",
                    "maximum  :",
                    "range    :",
                    "stdev    :",
                    "missing  :")
  values = unlist(stats, use.names = FALSE)
  values = format(round(values, digits = 4), nsmall = 4)
  both = paste(formatedNames, values, sep = ' ')
  for(i in both) {
    print(i)
  }
}
# Drop Lowest
# Arguments: numeric vector v
# Description:  takes a numeric vector of length n, and returns a vector of length n ??? 1 by dropping the lowest value
# Output: vector of length n ??? 1
drop_lowest = function(v) {
  min = get_minimum(v, na.rm = FALSE)
  for (i in 1:length(v)) {
    if(is.na(v[i]) || v[i] == min) {
      return(v[-i])
    }
     
  }
}
# Rescale 100
# Arguments: a numeric vector x, a minimum xmin, and a maximum xmax
# Description:  compute a rescaled vector with a potential scale from 0 to 100
# Output: numeric vector
rescale100 = function(x, xmin, xmax) {
  return (((x - xmin)/(xmax - xmin))*100)
}

# Score Homework
# Arguments: a numeric vector v, optional logical argument drop
# Description:  Compute a single homework value. If drop = TRUE, the
#               lowest HW score must be dropped
# Output: numeric value
score_homework =  function (v, drop) {
  if (missing(drop)) {
    return(get_average(v, na.rm = TRUE))
  }
  if (drop == TRUE) {
    return (get_average(drop_lowest(v), na.rm = TRUE))
  }
  if (drop == FALSE){
    return (get_average(v, na.rm = TRUE))
  }
}

# Score Quiz
# Arguments: numeric vector of quiz scores and an optional
#            logical argument drop
# Description:  compute a single quiz value. If drop = TRUE, the lowest quiz score
#               must be dropped.
# Output: numeric value
score_quiz = function (v, drop) {
  if (missing(drop)) {
    return(get_average(v, na.rm = TRUE))
  }
  if (drop == TRUE) {
    return (get_average(drop_lowest(v), na.rm = TRUE))
  }
  if (drop == FALSE){
    return (get_average(v, na.rm = T))
  }
}

# Score Lab
# Arguments: numeric value
# Description: uses a table to compute lab scores
# Output: numeric value
score_lab = function(value) {
  if (value == 11 | value == 12) {
    return(100)
  }
  else if (value == 10) {
    return(80)
  }
  else if (value == 9) {
    return(60)
  }
  else if (value == 8) {
    return(40)
  }
  else if (value == 7) {
    return(20)
  }
  else if (value >=0 & value <= 6) {
    return (0)
  }
  else {
    return(as.double(NA))
  }
}
#download.file("https://github.com/ucb-stat133/stat133-fall-2017/tree/master/data", "../data/rawdata/rawscores.csv")
  