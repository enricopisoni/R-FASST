# -*- mode: R -*-

# calculates with an analytical function the Risk Rate (RR) for a given PM exposure;
#
# Input parameters:
#   - C  - cause of death currently evaluated;
#   - PM - PM concentration (can be an array);
#
# Output parameters:
#   none;
#
# Returns:
#   RR with same dimension as PM grid map with
#   corresponding RR calculated in each grid cell;
#
rrate <- function( C, PM )
{

        # Initialize RR with same dimensions as PM
        RR <- numeric( length( PM ) )

        # elements less than than C[4]
        jj <- PM >= C[ 4 ]
        # Other grid cells: RR as a function of 4
        # C[i] function parameter
        RR[ jj ] <- 1 + C[1] * ( 1 - exp( -C[2] * ( PM[ jj ] - C[4] ) ^ C[3] ) )

        # grid cells where PM < C[4] get RR=1
        RR[ ! jj ]  <- 1

        # return result
        RR
}
