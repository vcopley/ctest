##' Function to calculate carbon impact of change in active travel for HEAT
##'
##' Takes three user-created data frames as input and returns carbon saving of additional
##' walking and/or cycling
##'
##' @param df_ucc A data frame class object containing the use case criteria
##' @param df_qual A data frame class object containing the data qualifiers
##' @param df_adj A data frame class object containing mode distance data in km
##' @param df_pop A data frame class object containing population in study
##'
##' @return A list with overall carbon impact of change in active travel and split by active mode
##' @author Christian Brand, Vicky Copley
##' @export
##' @keywords carbon
##' @examples
##' # Example usage of impact_carbon
##' results <- impact_carbon(df_ucc, df_qual, df_adj, df_pop)
##'
##'


impact_carbon <- function(df_ucc, df_qual, df_adj, df_pop, df_generic_path="./generic_data/") {

  # Make sure the adjusted data are only for carbon pathway
  df_adj <- df_adj[df_adj$pathway=='carbon',]

  # Read in background carbon data with emissions etc.
  background_data <- read.csv(paste0(df_generic_path,"carbon_background_dataR.csv"), stringsAsFactors = FALSE)

  # Read in currency exchange rate data
  exchange_rates <- read.csv(paste0(df_generic_path,"euro_converter.csv"), stringsAsFactors = FALSE)
  usd_eur <- 1/exchange_rates[nrow(exchange_rates), "USD"]

  # Get the start and end years of the assessment
  start_year <- as.numeric(df_ucc$r_value[df_ucc$r_varname=="ucc_yearref"])
  assessmenttime <- as.numeric(df_ucc$r_value[df_ucc$r_varname=="ucc_assessmenttime"])
  yearcf <- as.numeric(df_ucc$r_value[df_ucc$r_varname=="ucc_yearcf"])

  # Set some defaults if no user-specified values for years
  # This might be checked for outside this function - TO DO


  #Set the start year to current year if it is missing
  if(is.na(start_year))  start_year <- as.numeric(substr(Sys.Date(),1, 4))
  # Set year to end assessment
  if(is.na(assessmenttime)) {
    if(df_ucc$r_value[df_ucc$r_varname=="ucc_comparisoncases_onecase"]==TRUE) {
      end_year <- start_year + 1
    } else {   # It's a before and after comparison
      if(!is.na(yearcf)) end_year <- yearcf
    }
  } else { # there is an assessmenttime
      end_year <- start_year + assessmenttime
  }

  no_years <- max(c(end_year - start_year, 1))

  # Get the selected background country and relevant years
  background_country <- background_data[background_data$countryiso3 ==
                                          df_ucc$r_value[df_ucc$r_varname=="ucc_countryiso_countryiso"] &
                                          background_data$year >   start_year &
                                          background_data$year <= (start_year + no_years)
                                        ,]

  # Get the generic data
  df_gen <- read.csv(paste0(df_generic_path,"generic_data.csv"), stringsAsFactors = FALSE)


  # Identify all the active modes
  modes <- df_ucc$level_varname[df_ucc$question_varname %in% c( "ucc_activemode" ,"ucc_otheractivemode")
                                & df_ucc$level_varname !="other"
                                & df_ucc$r_value == TRUE]

  # Do we have refined motor modes, or basic/default?
  refined <- as.logical(df_ucc$r_value[df_ucc$r_varname=="ucc_motormode_refined"])


  # Define some variables for later
  # mode_shift_vars are the names of the variables in the df_adj data frame which give mode shift in km
  if(refined==TRUE) {
    mode_shift_vars <- c("vol_shifted",
                       "volfromwalk", "volfrombike","volfromrun","volfromebike","volfrombikeshare",
                       "volfromcardriver", "volfromcarpassenger",
                       "volfrommotorbike", "volfrombus", "volfromlightrail","volfromtrain")
    emission_modes <- c("cardriver", "carpassenger", "motorbike", "bus", "lightrail", "train",
                        "bike", "ebike", "bikeshare")
    car_type <- emission_modes[1]
  } else {
    mode_shift_vars <- c("vol_shifted",
                       "volfromwalk", "volfrombike","volfromrun","volfromebike","volfrombikeshare",
                       "volfromcar","volfrompt")
    emission_modes <- c("car", "pt", "bike", "ebike", "bikeshare")
    car_type <- emission_modes[1]
  }

  traffic_speeds  <-  c("euromeancongestion","freeflow", "somecongestion", "heavycongestion")



  # Set up empty list to hold detailed results for each active and motor mode
  # results <- list(NULL)

  # Set up data frame for summary results to pass back

  output_variables <- c("activemode","impacttrendco2", "impactcontrastpercentco2", "impactcontrastabsoluteco2",
                        "impacttotalco2", "impactperyearaveco2", "impactperyearmaxco2", "scc",
                        "moneytotal", "moneyperyear", "moneymax", "yearmoneymax",
                        "moneyperyeardisc", "moneytotaldisc", "moneymaxdisc", "cost", "bcratio")


  active_res <- setNames(data.frame(matrix(ncol = length(output_variables), nrow = length(modes))), output_variables)

  # Loop around the chosen active modes

  for (i in 1:length(modes)) {

      # Get the traffic condition index
      traffic_index <- which(df_qual$r_value[df_qual$level_varname %in% traffic_speeds
                                                              & df_qual$activemode==modes[i]]==TRUE)


      # Get the emissions factors
      background_country$ef_car_rw_pkm <- unlist(unname(background_country[paste0("ef_car_rw_pkm_",traffic_index-1)]))
      background_country$ef_wtt_car <- unlist(unname(background_country[paste0("ef_wtt_car_",traffic_index-1)]))
      background_country$ef_wtt_cardriver <- unlist(unname(background_country[paste0("ef_wtt_cardriver_",traffic_index-1)]))


      # Check  whether single point in time or pre/post assessment
      # and draw out / calculate the appropriate mode shift data.  All carbon are in "km"

      if(df_ucc$r_value[df_ucc$r_varname=="ucc_comparisoncases_onecase"]) {

        mode_shiftW <- df_adj[df_adj$comparison_case=="ref" & df_adj$r_level_varname==modes[i], mode_shift_vars]
        # Ensure there is a minus sign on the variables which aren't the active mode
        for (j in 2:ncol(mode_shiftW)) {
          if(!is.na(mode_shiftW[1,j]) & mode_shiftW[1, j]>0) {
            mode_shiftW[1,j] <- mode_shiftW[1,j]* -1
          }
        }

        prop_rec <- 1 - (df_adj[df_adj$comparison_case=="ref" & df_adj$r_level_varname==modes[i], "percenttransport"])
        prop_new <- df_adj[df_adj$comparison_case=="ref" & df_adj$r_level_varname==modes[i], "percentnew"]
        prop_from_car <- df_adj[df_adj$comparison_case=="ref" & df_adj$r_level_varname==modes[i], "fromcar"]

        } else {

        ref <- df_adj[df_adj$comparison_case=="ref" & df_adj$r_level_varname==modes[i],mode_shift_vars]
        cf <- df_adj[df_adj$comparison_case=="cf" & df_adj$r_level_varname==modes[i], mode_shift_vars]

        mode_shiftW <- cf - ref

        prop_rec <- 1 - (df_adj[df_adj$comparison_case=="cf" & df_adj$r_level_varname==modes[i], "percenttransport"])
        prop_new <- df_adj[df_adj$comparison_case=="cf" & df_adj$r_level_varname==modes[i], "percentnew"]
        prop_from_car <- df_adj[df_adj$comparison_case=="cf" & df_adj$r_level_varname==modes[i], "fromcar"]

      }

      # Transpose the mode_shift to long format, add a mode type column
      mode_shiftT <- as.data.frame(t(mode_shiftW))
      mode_shiftT$mode <- substr(row.names(mode_shiftT), 8, 23)

      # Drop the 'volfrom' data for the active mode we're interested in - it will be zero
      mode_shift <- mode_shiftT[mode_shiftT$mode!=modes[i],]
      # Relabel the 'vol_shifted' row as the active mode
      mode_shift$mode[1] <- modes[i]
      names(mode_shift)[1] <- "diff"

      # Remove recreational from mode shift distances
      mode_shift$diff <- mode_shift$diff * (1 - prop_rec)
      # Checksum - next line only for checking - should equal 0
      # sum(mode_shift$diff, na.rm=TRUE)


      # Pull out the hot emissions relevant to the traffic conditions
      # Only keep the modes which are associated with emissions (the merge drops any other modes)
      if(refined==TRUE) {
      ef <- rbind(data.frame(mode = "cardriver", ef = background_country$ef_car_rw_pkm * background_country$occ_car,
                                                      year=background_country$year),
                  data.frame(mode = "bus", ef = background_country$ef_bus_pkm, year=background_country$year),
                  data.frame(mode = "motorbike", ef = background_country$ef_moto_pkm, year=background_country$year))
      } else {
      ef <- rbind(data.frame(mode = "car", ef = background_country$ef_car_rw_pkm, year=background_country$year),
                  data.frame(mode = "pt", ef = background_country$ef_bus_pkm *
                                 background_country$pt_bus_rail_split, year=background_country$year))
      }


      # Merge emissions to the mode shift data
      emis_ppk <- merge(ef, mode_shift, by.x = "mode", by.y = "mode")

      # -----------------------------------------------------
      # Cold emissions for cars
      # -----------------------------------------------------

      # Initialise cold start emissions vector
      cold_car_trip <- rep(NA, no_years)

      # TO DO - need to check if trip_length is supplied by users

      trip_length <- df_gen$value[df_gen$parameter_name==paste0("triplength_", modes[i])]
      trips_pp_year <- df_gen$value[df_gen$parameter_name==paste0("trips_pp_year_", modes[i])]

      # Code allows cold trip distance, emissions and cold:hot ratio to vary by year

      row_true <- which(trip_length <= background_country$cold_trip_dist)
      row_false <- which((trip_length <= background_country$cold_trip_dist) == FALSE)

      #

      cold_car_trip[row_true] <- trip_length * emis_ppk[emis_ppk$mode==car_type, "ef"][row_true] *
                                      (background_country$ratiocoldhot[row_true] - 1)

      cold_car_trip[row_false] <- background_country$cold_trip_dist[row_false] / trip_length * trip_length *
                                      emis_ppk[emis_ppk$mode==car_type, "ef"][row_false]  *
                                      (background_country$ratiocoldhot[row_false] - 1)


      change_emis_cold  <- cold_car_trip * trips_pp_year * (1 - prop_rec) *
                            prop_from_car / 1000  * -1


      # Change in direct emissions (in kgCO2e per person per year)
      emis_ppk$change_emis_hot <- emis_ppk$diff * emis_ppk$ef / 1000

      # Change in total emissions for cars
      emis_ppk$change_emis_total[emis_ppk$mode==car_type] <-
              emis_ppk$change_emis_hot[emis_ppk$mode==car_type] + change_emis_cold

      # Change in total emissions for other modes
      emis_ppk$change_emis_total[emis_ppk$mode!=car_type] <-
              emis_ppk$change_emis_hot[emis_ppk$mode!=car_type]

      emis_ppk$change_emis_cold <- emis_ppk$change_emis_total - emis_ppk$change_emis_hot

      change_emis <- emis_ppk[,c("mode","year","change_emis_hot", "change_emis_cold", "change_emis_total")]

    # -----------------------------------------------------------------------------
    # Change in energy supply emissions (in kgCO2e per person per year)
    # Food intake question not implemented in this version of HEAT, so assuming
    # no energy supply emissions for walking, cycling and running
    # -----------------------------------------------------------------------------
    change_supply <- data.frame('year' = rep(seq(start_year+1, end_year, by=1), length(emission_modes)),
                                'mode' = rep(emission_modes, rep(no_years, length(emission_modes))),
                                'change_sup' = rep(NA, no_years*length(emission_modes)))

    if(refined==FALSE) {  # Only two motor modes
      # car well to tank emissions depend on traffic conditions
      change_supply$change_sup[change_supply$mode=="car"] <- background_country$ef_wtt_car *
                              mode_shift$diff[mode_shift$mode=="car"]  / 1000

      # pt
      change_supply$change_sup[change_supply$mode=="pt"] <- (background_country$ef_wtt_bus *
                              background_country$pt_bus_rail_split +
                              background_country$ef_wtt_rail * (1-background_country$pt_bus_rail_split)) *
                              mode_shift$diff[mode_shift$mode=="pt"]  / 1000
    } else { # More refined motor modes

      # car driver - multiply emissions by occupancy rate
      change_supply$change_sup[change_supply$mode=="cardriver"] <- background_country$ef_wtt_cardriver *
                              mode_shift$diff[mode_shift$mode=="cardriver"]  / 1000

      # car passenger - assume emissions factor of zero
      change_supply$change_sup[change_supply$mode=="carpassenger"] <- 0 *
                              mode_shift$diff[mode_shift$mode=="carpassenger"]  / 1000

      # motorbike
      change_supply$change_sup[change_supply$mode=="motorbike"]  <- background_country$ef_wtt_moto *
        mode_shift$diff[mode_shift$mode=="motorbike"]  / 1000

      # bus
      change_supply$change_sup[change_supply$mode=="bus"]  <- background_country$ef_wtt_bus *
                              mode_shift$diff[mode_shift$mode=="bus"]  / 1000
      # rail
      change_supply$change_sup[change_supply$mode=="lightrail"]  <- background_country$ef_wtt_rail *
                                  mode_shift$diff[mode_shift$mode=="lightrail"]  / 1000

      # train - energy supply emissions assumed to be the same as light rail
      change_supply$change_sup[change_supply$mode=="train"]  <- background_country$ef_wtt_rail *
        mode_shift$diff[mode_shift$mode=="train"]  / 1000

      # data for other modes not yet available in background spreadsheet

    } # End populating change in supply emissions data frame

    # -----------------------------------------------------------------------------
    # Change in vehicle lifecycle emissions
    # Assumes no lifecycle emissions for walking or running
    # -----------------------------------------------------------------------------
    change_lc <- data.frame('year' = rep(seq(start_year+1, end_year, by=1), length(emission_modes)),
                                  'mode' = rep(emission_modes, rep(no_years, length(emission_modes))),
                                  'change_lc' = rep(NA, no_years*length(emission_modes)))

    if(refined==FALSE) {  # Only two motor modes
      # car
      change_lc$change_lc[change_lc$mode=="car"] <- background_country$ef_vlca_car *
                               mode_shift$diff[mode_shift$mode=="car"]  / 1000
      # pt
      change_lc$change_lc[change_lc$mode=="pt"] <- (background_country$ef_vlca_bus *
                               background_country$pt_bus_rail_split +
                               background_country$ef_vlca_rail * (1-background_country$pt_bus_rail_split)) *
                               mode_shift$diff[mode_shift$mode=="pt"]  / 1000
    } else {

      # car driver - multiply emissions by occupancy rate
      change_lc$change_lc[change_lc$mode=="cardriver"] <- background_country$ef_vlca_car *
                               background_country$occ_car *
                               mode_shift$diff[mode_shift$mode=="cardriver"]  / 1000

      # car passenger - assume emissions factor of zero
      change_lc$change_lc[change_lc$mode=="carpassenger"] <- 0 *
                               mode_shift$diff[mode_shift$mode=="carpassenger"]  / 1000

      # bus
      change_lc$change_lc[change_lc$mode=="bus"] <- background_country$ef_vlca_bus *
                               mode_shift$diff[mode_shift$mode=="bus"]  / 1000
      # rail
      change_lc$change_lc[change_lc$mode=="lightrail"] <- background_country$ef_vlca_rail *
                               mode_shift$diff[mode_shift$mode=="lightrail"]  / 1000

      # train
      change_lc$change_lc[change_lc$mode=="train"] <- background_country$ef_vlca_rail *
                               mode_shift$diff[mode_shift$mode=="train"]  / 1000
    }


    # Add on bike emissions, if relevant to the use case
    # Need to factor in all of the new distance on bikes here excluding route-shifted (reassigned),
    # so include new and recreational
    # Need to add lifecycle emissions for other active modes if relevant when background data available

    if (modes[i] == "bike") {
        change_lc$change_lc[change_lc$mode=="bike"] <- background_country$ef_vlca_bike *
                               mode_shift$diff[1] / ((1-prop_new) * (1-prop_rec) * 1000)
    }


    # -----------------------------------------------------------------------------
    # Put the results together
    # -----------------------------------------------------------------------------

    # Merge the three types of carbon savings to one data frame
    ac <- merge(change_emis, change_supply, by=c("mode", "year"), all=TRUE)
    all_carbon <- merge(ac, change_lc, by=c("mode", "year"), all=TRUE)

    # Form total of all carbon impacts per person - emissions, supply, lifecycle
    all_carbon$total_per_person <- rowSums(all_carbon[,c("change_emis_total","change_sup","change_lc")], na.rm=TRUE)

    # Total carbon impacts per population
    # TO DO - check the correct population is being accessed
    all_carbon$total_per_population <- all_carbon$total_per_person * df_pop$r_value[df_pop$activemode==modes[i] &
                                                                                      is.na(df_pop$level_varname)]
    # Total carbon impacts valued.  Divide by 1000 to put units in tonnes
    all_carbon$carb_sav_per_pop_usd <- all_carbon$total_per_population / 1000 * background_country$carbon_value_usd
    all_carbon$carb_sav_per_pop_eur <- all_carbon$carb_sav_per_pop_usd * usd_eur

    # Discounted carbon impacts
    # All values discounted to 2017 as year zero
    all_carbon$carb_sav_per_pop_eur_disc <- all_carbon$carb_sav_per_pop_eur /
                                              (1 + df_gen$value[df_gen$parameter_name == "discrate"] / 100) ^
                                              (start_year - 2017 + 1)

    # Get intervention cost
    cost <- as.numeric(df_qual$r_value[df_qual$r_varname==paste0("quali_activemode_", modes[i], "_cost")])

    # Aggregate values for results data frame
    # Note that the maximum impact and maximum money are when the most carbon is saved, i.e. negative numbers

    active_res[i, "activemode"] <- modes[i]

#    active_res[i, "impacttrendco2"] <-
#    active_res[i, "impactcontrastpercentco2"] <-
#    active_res[i, "impactcontrastabsoluteco2"] <-

    active_res[i, "impacttotalco2"] <- sum(all_carbon$total_per_population)/ 1000
    active_res[i, "impactperyearaveco2"] <- sum(all_carbon$total_per_population)/(1000 * no_years)
    active_res[i, "impactperyearmaxco2"] <- min(aggregate(all_carbon$total_per_population, by=list(all_carbon$year), FUN=sum)[,2])/1000
    # Take first year's social value of carbon as scc
    active_res[i, "scc"] <- background_country$carbon_value_usd[1] * usd_eur
    active_res[i, "moneytotal"] <- sum(all_carbon$carb_sav_per_pop_eur)
    active_res[i, "moneyperyear"] <- sum(all_carbon$carb_sav_per_pop_eur)/no_years
    active_res[i, "moneymax"] <- min(aggregate(all_carbon$carb_sav_per_pop_eur, by=list(all_carbon$year), FUN=sum)[,2])
    active_res[i, "yearmoneymax"] <- start_year +
                                which.min(aggregate(all_carbon$carb_sav_per_pop_eur, by=list(all_carbon$year), FUN=sum)[,2])
    active_res[i, "moneyperyeardisc"] <- sum(all_carbon$carb_sav_per_pop_eur_disc)/no_years
    active_res[i, "moneytotaldisc"] <- sum(all_carbon$carb_sav_per_pop_eur_disc)
    active_res[i, "moneymaxdisc"] <- min(aggregate(all_carbon$carb_sav_per_pop_eur_disc, by=list(all_carbon$year), FUN=sum)[,2])
    active_res[i, "cost"] <- cost

    if(cost>0) active_res[i, "bcratio"] <- active_res[i, "impacttotalco2"] / cost *-1

    # Save detailed results by active mode
    # results[[i]] <- all_carbon
    # names(results)[[i]] <- modes[i]



  } # End active mode loop

  active_res  # return the results summary for active modes


} # End function
