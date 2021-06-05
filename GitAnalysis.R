
  
#  Playing with git log analysis

# Parts 1 and 2
library(tidyverse)
library(glue)
library(stringr)
library(forcats)


# Remote repository URL
repo_url <- "https://github.com/tidyverse/ggplot2.git"
# Directory into which git repo will be cloned
clone_dir <- file.path(tempdir(), "git_repo")
# Create command
clone_cmd <- glue("git clone {repo_url} {clone_dir}")
# Invoke command
system(clone_cmd)


system(glue('git -C {clone_dir} log -3'))

log_format_options <- c(datetime = "cd", commit = "h", parents = "p", author = "an", subject = "s")
option_delim <- "\t"
log_format   <- glue("%{log_format_options}") %>% glue_collapse(option_delim)
log_options  <- glue('--pretty=format:"{log_format}" --date=format:"%Y-%m-%d %H:%M:%S"')
log_cmd      <- glue('git -C {clone_dir} log {log_options}')

system(glue('{log_cmd} -3'))

history_logs <- system(log_cmd, intern = TRUE) %>% 
  str_split_fixed(option_delim, length(log_format_options)) %>% 
  as_tibble() %>% 
  setNames(names(log_format_options))



# Because the parents commit can be multiple parents, we change that column into a list.

history_logs <- history_logs %>% 
  mutate(parents = str_split(parents, " "))


# I don't know what's going on here, or why there's a loop instead of a function with lapply or map

# Finally, be sure to assign branch numbers to commits. There’s surely a better way to do this, but here’s one (very untidy) method:

# Start with NA
history_logs <- history_logs %>% mutate(branch = NA_integer_)


# Create a boolean vector to represent free columns (1000 should be plenty!)
free_col <- rep(TRUE, 1000)


for (i in seq_len(nrow(history_logs) - 1)) { # - 1 to ignore root # The seq_len creates a sequence from 1 to the number of rows in history (minus one, to ignore the initial commit which doesn't have a parent). So we are going to loop through the entire dataframe.
  # Check current branch col and assign open col if NA (NA means we haven't evaluated it yet)
  branch <- history_logs$branch[i] # Building a vector called branch, with value NA_integer
  if (is.na(branch))  #It's NA (the first time), so we perform this loop
    branch <- which.max(free_col) #the which.max the first time should be 1, so branch is now 1
  free_col[branch] <- FALSE # And now the first entry in free_col is false, with the other 999 as TRUE
  history_logs$branch[i] <- branch #and now we have branch as 1, so this loop will end because it's no longer NA.
} 
# Go through parents
parents <- history_logs$parents[[i]] # Assign parents the object in the i'th row. We look through the entire dataframe each time.
for (p in parents) { # Parents is a list. So this loop looks through the list of parents in the row.
  parent_col <- history_logs$branch[history_logs$commit == p] # we create a vector and assign the row where the commit is equal to the parent
  # If col is missing, assign it to same branch (if first parent) or new
  # branch (if other)
  if (is.na(parent_col)) { #If there's no parent, it means there's a new branch?
    parent_col <- if_else(p == parents[1], branch, which.max(free_col)) # If 
    # If NOT missing this means a split has occurred. Assign parent the lowest
    # and re-open both cols (parent closed at the end)
  } else {
    free_col[c(branch, parent_col)] <- TRUE 
    parent_col <- min(branch, parent_col)
  }
  # Close parent col and assign
  free_col[parent_col] <- FALSE
  history_logs$branch[history_logs$commit == p] <- parent_col #We place the value in the history_log and go through the loop
}
}
```


