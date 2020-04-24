# -*- mode: R -*-

#' Calculates with an analytical function the Risk Rate (RR)
#' for a given PM exposure;
#'
#' @param C  cause of death currently evaluated;
#' @param PM PM concentration (can be an array);
#'
#' @return RR with same dimension as PM grid map with
#'         corresponding RR calculated in each grid cell;
#'
rrate <- function( C, PM )
{
	# Other grid cells: RR as a function of 4
	# C[i] function parameter
	RR                 <- 1 + C[1] * ( 1 - exp( -C[2] * ( PM - C[4] ) ^ C[3] ) )

	# grid cells where PM < C[4] get RR=1
	RR[ PM < C[ 4 ] ]  <- 1

	# return result
	RR
}
