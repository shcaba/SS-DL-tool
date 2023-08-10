#' Get FishLife life history traits
#'
#' Retrieves life history trait predictions from FishLife (Thorson et al. 2018). This
#' is a wrapper for the Plot_taxa() function in the FishLife R package.
#'
#' @param species A character vector of species scientific names
#' @return A dataframe with life history trait predictions from FishLife for each species
#' @examples
#' # Look up life history traits
#' species <- c("Gadus morhua", "Centropristis striata", "Paralichthys dentatus")
#' fishlife(species)
#' @export
fishlife <- function(species){

  # Setup container
  spp <- sort(unique(species))
  fl <- data.frame(species=spp, linf_cm=NA, k=NA, winf_g=NA, tmax_yr=NA, tmat_yr=NA,
                   m=NA, lmat_cm=NA, temp_c=NA, stringsAsFactors=F)

  # Loop through species
  for(i in 1:nrow(fl)){

    # Get spp info
    sciname <- fl$species[i]
    genus <- stringr::word(sciname, 1)
    nwords_in_spp <- length(strsplit(sciname, " ")[[1]])
    species <- stringr::word(sciname, start=2, end=nwords_in_spp)
    species <- ifelse(species=="spp", "predictive", species)

    # Try looking up in FishLife
    spp_info <- try(FishLife::Plot_taxa(FishLife::Search_species(Genus=genus, Species=species)$match_taxonomy))
    if(inherits(spp_info, "try-error")){
      # Record blanks
#      fl[i,2:ncol(fl)] <- rep(NA, ncol(fl)-1)
    }else{
      # Values are in log-scale except temperature
      spp_lh_vals_log <- spp_info[[1]]$Mean_pred
      spp_lh_vals <- c(exp(spp_lh_vals_log[1:7]), spp_lh_vals_log[8],spp_lh_vals_log[9:20])
 #     fl[i,2:ncol(fl)] <- spp_lh_vals
    }

  }

  # Return
#  return(fl)
  return(spp_lh_vals)
}
