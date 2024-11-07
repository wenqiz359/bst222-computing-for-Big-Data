source("hashing.R")  
source("ChangepointDetection.R")      

cat("### Testing Hashing Class ###\n\n")

# Initialize a new hash table
myHash <- hashing(5) 

# Test Case 1: No Collision
cat("### Test Case 1: No Collision ###\n")

# Inserting values that should hash to different indices
myHash <- store(myHash, 3)  # Expected hash index: 4
myHash <- store(myHash, 7)  # Expected hash index: 3

# Check if values are correctly inserted without collision
cat("Expected TRUE: ", lookup(myHash, 3), "\n")  # Expected output: TRUE
cat("Expected TRUE: ", lookup(myHash, 7), "\n")  # Expected output: TRUE
cat("Expected FALSE: ", lookup(myHash, 10), "\n")  # Expected output: FALSE

# Test Case 2: Collision Handling
cat("\n### Test Case 2: Collision Handling ###\n")
# Initialize a new hash table

# Inserting values that should hash to the same index
myHash <- store(myHash, 8)  # Expected hash index: 4 (collision with 3)
myHash <- store(myHash, 13) # Expected hash index: 4 (collision with 3 and 8)

# Check if all values are correctly stored in case of collision
cat("Expected TRUE: ", lookup(myHash, 3), "\n")  # Expected output: TRUE
cat("Expected TRUE: ", lookup(myHash, 8), "\n")  # Expected output: TRUE
cat("Expected TRUE: ", lookup(myHash, 13), "\n") # Expected output: TRUE

# Test Case 3: Re-inserting the Same Value
cat("\n### Test Case 3: Re-inserting the Same Value ###\n")

# Re-insert an existing value and ensure it doesn't duplicate
myHash <- store(myHash, 3)
cat("Expected TRUE (no duplication): ", lookup(myHash, 3), "\n")

cat("\n### Testing Complete for Hashing Class ###\n\n")


cat("### Testing CUSUM Class ###\n\n")

# Initialize a new CUSUM object with a threshold
cusumObj <- cusumCpt(data = numeric())

# Test Case 1: Adding Data
cat("### Test Case 1: Adding Data ###\n")

# Add initial data and check if it is stored correctly
cusumObj <- addData(cusumObj, c(1, 2, 3, 4, 5))
cat("Expected Data: c(1, 2, 3, 4, 5)\n")
cat("Actual Data: ", cusumObj@data, "\n")

# Test Case 2: No Changepoint Detected
cat("\n### Test Case 2: No Changepoint Detected ###\n")

# Threshold is set high enough to avoid detecting any changepoint
cat("Expected FALSE: ", checkCpt(cusumObj,threshold=10), "\n")

# Test Case 3: Detecting a Changepoint
cat("\n### Test Case 3: Detecting a Changepoint ###\n")

# Add a large value to trigger a changepoint
cusumObj <- addData(cusumObj, 50)
cat("Expected TRUE: ", checkCpt(cusumObj,threshold=10), "\n")


