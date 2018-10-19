# How to install cbn on your R

# Step 1. Install devtools and here packages
install.packages(c("devtools", "here"))

# Step 2. Pull from your library
require("devtools")
require("here")

# Step 3. Set your working directory
setwd("Delete this message and enter the location of cbn folder here. Keep the quotation marks.")
# ex. setwd("Users/Sophie/Smart/cbn/")

# Install cbn
devtools::install(here(""))

# If you get an error that reads:
#     Error: Could not find build tools necessary to build cbn
# Then, run this code:
options(buildtools.check = function(action) TRUE)

# If you get an error that reads:
#     xcrun: error: invalid active developer path (/....), missing xcrun at: ...
# Then, run this code in the TERMINAL
sudo xcode-select --install