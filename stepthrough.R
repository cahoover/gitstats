# Start with NA
history_logs <- history_logs %>% mutate(branch = NA_integer_)

# Create a boolean vector to represent free columns (1000 should be plenty!)
free_col <- rep(TRUE, 1000)

#seq_len creates a sequence from one to n, with n defined in the argument.
#nrow counts the number of rows in history_logs (4756).
#Here we are creating a sequence that's the one short of the number of rows (4755).
 # for (i in seq_len(nrow(history_logs) - 1)) { # Cycle through nrow number of times and perform the following function
    # Check current branch col and assign open col if NA
  i <- 1   
  branch <- history_logs$branch[i] #Create a variaible called "branch" and assign it the value in the current row of history_logs
    #So branch is NA, of course, because we've set all values of branch to NA.
    if (is.na(branch)) { #All of the branches are NA at first, see the mutuate function above.
      branch <- which.max(free_col) #If the branch is NA, then assign the max value in free_col. The first iteration is going to be 1. 
      free_col[branch] <- FALSE #set the first column of free_col to false
      history_logs$branch[i] <- branch #set the first branch to 1
    }
 
    # Go through parents - the first thing we do is look at the parent of the row we're working on.
    parents <- history_logs$parents[[i]] #We're going to get the name of a commit here. For row 1 it's c9adeedd
    
    for (p in parents) { #there might be more than one parent, so we iterate. Most of the time there's only one.
      parent_col <- history_logs$branch[history_logs$commit == p] #parent_col is the value of branch in the row that matches commit. The first time will be NA. 
      
      # If col is missing, assign it to same branch (if first parent) or new branch (if other)
      
      if (is.na(parent_col)) { #and it will be, for the first iteration
        parent_col <- if_else(p == parents[1], branch, which.max(free_col)) #If the first parent matches [1], parent_col = branch, otherwise match free_col
        
        # If NOT missing this means a split has occurred, because a previous commit will have pointed to this parent, and 
        # now we see a second (or third) commit pointing to the same parent. Assign parent the lowest (not sure what this means)
        # and re-open both cols (parent closed at the end)
      } else {
        free_col[c(branch, parent_col)] <- TRUE #All values between branch and parent_col are not true
        parent_col <- min(branch, parent_col) #Assign the parent_col the lowest value between branch and parent_col
        
      }
      
      # Close parent col and assign
      free_col[parent_col] <- FALSE #This increments the counter
      history_logs$branch[history_logs$commit == p] <- parent_col #and we assign the value in parent_col to the branch
    }
  }
