# Set Up Papaja

# Intall Tex Engine 
tinytex::install_tinytex()

# Install devtools package if necessary
if(!"devtools" %in% rownames(installed.packages())) install.packages("devtools")

# Install the stable development verions from GitHub
devtools::install_github("crsh/papaja")

install.packages("citr")
