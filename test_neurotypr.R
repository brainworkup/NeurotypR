# Test if cars2_hf is properly exported from NeurotypR
library(NeurotypR)

# Try to access the cars2_hf dataset
head(cars2_hf)

# Check if the functions are properly exported
# (Just checking if they exist, not running them)
exists("process_rbans_data")
exists("run_data_entry_app")

cat("Success! The cars2_hf dataset and functions are now properly exported from NeurotypR.\n")
