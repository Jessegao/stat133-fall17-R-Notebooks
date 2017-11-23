
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
#              na.rm tells the function to remove the NA's
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
    if(i < min_val) {
      min_val = i
    }
  }
  return(min_val)
}

# Get Maximum
# Arguments: a numeric vector v and an optional logical na.rm argument
# Description: takes a numeric vector, and an optional logical na.rm argument, to find the maximum value
#             na.rm tells the function to remove the NA's
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
get_range = function(v) {
  if (! is.numeric(v)) {
    return("non-numeric argument")
  }
  if (length(v) == 0){
    return("vector is empty")
  }
  if (na.rm) {
    v = remove_missing(v)
  }
  return(get_maximum(v) - get_minimum(v))
}

# Get 10th percentile
# Arguments: a numeric vector v and an optional logical na.rm argument
# Description: computes the 10th percentile of the input vector
# Output: numeric value
get_percentile10 = function()

# 
# Arguments: a numeric vector v and an optional logical na.rm argument
# Description: 
# Output: numeric value
get_percentile90 = function()

# 
# Arguments: a numeric vector v and an optional logical na.rm argument
# Description: 
# Output: numeric value
get_quartile1 = function()

# 
# Arguments: a numeric vector v and an optional logical na.rm argument
# Description: 
# Output: numeric value
get_quartile3 = function()

# Get Median
# Arguments: a numeric vector v and an optional logical na.rm argument
# Description: takes a numeric vector, and an optional logical na.rm argument, to
#              compute the median of the input vector
# Output: numeric value
get_median = function()

# 
# Arguments: a numeric vector v and an optional logical na.rm argument
# Description: 
# Output: numeric value
get_average = function()

# 
# Arguments: a numeric vector v and an optional logical na.rm argument
# Description: 
# Output: numeric value
get_stdev = function()

# 
# Arguments: 
# Description: 
# Output: numeric value
count_missing = function()

# 
# Arguments: 
# Description: 
# Output: 
summary_stats = function()

# 
# Arguments: 
# Description: 
# Output: 
print_stats = function()

# 
# Arguments: 
# Description: 
# Output: 
drop_lowest = function()

# 
# Arguments: 
# Description: 
# Output: 
rescale100 = function()

# 
# Arguments: 
# Description: 
# Output: numeric value
score_homework = function()

# 
# Arguments: 
# Description: 
# Output: numeric value
score_quiz = function()

# 
# Arguments: 
# Description: 
# Output: numeric value
score_lab = function()