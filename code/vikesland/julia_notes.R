install.packages("validate")
library(validate)

# check the qualisty of the data
# some notes here: 

# when fluxID is uneven it should correspond to NEE values. PAR values should be
# higher than zero...except for night hours...
# when fluxID is even it should correspond to ER values. In this case PAR values
# should be always close to zero.

# there cannot be two nee in a row