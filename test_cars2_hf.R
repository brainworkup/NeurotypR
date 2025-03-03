# Test if cars2_hf is properly exported from NeurotypR
library(NeurotypR)

# Try to access the cars2_hf dataset
head(cars2_hf)

# If this runs without errors, the fix worked!
cat("Success! The cars2_hf dataset is now properly exported from NeurotypR.\n")
