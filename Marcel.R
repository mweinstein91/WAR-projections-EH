


#to delete: temporary way of loading and storing original data
hold_df <- read_csv('SkaterWarAll.csv', guess_max = 5000)
hold_df$season <- as.character(hold_df$season)


#main control loop runs through and calls functions that call the various functions
configure_year_plus_one <- function(df) {
  require(tidyverse)
  
  #combine data for seasons where a player switched teams
  double_players <-
    df %>% group_by(player, position, season) %>% filter(n() > 1) %>% summarise(
      GP = sum(GP),
      TOI_all = sum(TOI_all),
      TOI_EV = sum(TOI_EV),
      TOI_PP = sum(TOI_PP),
      TOI_SH = sum(TOI_SH),
      EV_GAR = sum(EV_GAR),
      PP_GAR = sum(PP_GAR),
      SH_GAR = sum(SH_GAR),
      Pens_GAR = sum(Pens_GAR),
      GAR = sum(GAR),
      WAR = sum(WAR)
    )
  
  #gets players bdays 
  birthdays <- get_birthdays()
  
  #adds team switchers, joins birthdays and various positional info, sets up DF to compute correlation coef and running lm to get yearly weights
  test <-
    df %>% group_by(player, position, season) %>% arrange(player, season) %>% filter(n() == 1) %>% bind_rows(double_players) %>% inner_join(
      birthdays %>% select(
        player,
        positioncode,
        shootscatches,
        birthday,
        season,
        season_age,
        current_age
      ),
      by = c("player", "season")
    ) %>% group_by(player) %>% mutate(
      age_plus_one = lead(season_age),
      WAR_per_min = WAR / TOI_all,
      TOI_all_plus_one = lead(TOI_all),
      TOI_all_plus_two = lead(TOI_all, 2),
      TOI_all_plus_three = lead(TOI_all, 3),
      GP_plus_one = lead(GP),
      GP_plus_two = lead(GP, 2),
      GP_plus_three = lead(GP, 3),
      WAR_plus_one = lead(WAR),
      WAR_plus_two = lead(WAR, 2),
      WAR_plus_three = lead(WAR, 3),
      WAR_per_min_plus_one = lead(WAR / TOI_all)
    ) %>% select(-Team) 
  
  #contructs aging curves
  aging_curve <- construct_aging_curve(test)
  
  #gets seasons with null data (sanity check)
  null_seasons <- test %>% filter(!complete.cases(.))
  
  return(test %>% na.omit())
  
}

#finds yty correlation of stats we are trying to project so we can figure out the regression amount 
find_correlation_coef <- function(df) {
  #find correlations
  fit_war <-
    test %>% group_by(position) %>% do(model  = lm(data = ., WAR_plus_one ~ WAR))
  fit_G <-
    test %>% group_by(position) %>% do(model = lm(data = ., GP_plus_one ~ GP))
  fit_TOI <-
    test %>% group_by(position) %>% do(model = lm(data = ., TOI_all_plus_one  ~ TOI_all))
  
  #obtain and combine the various r^2
  cc_war <-
    glance(fit_war, model) %>% select(position, r.squared) %>% mutate(metric = 'WAR')#, cor = sqrt(r.squared))
  cc_g <-
    glance(fit_G, model) %>% select(position, r.squared) %>% mutate(metric = 'GP')#, cor = sqrt(r.squared))
  cc_TOI <-
    glance(fit_TOI, model) %>% select(position, r.squared) %>% mutate(metric = 'TOI_all')#, cor = sqrt(r.squared))
  
  corr<-bind_rows(cc_war, cc_g, cc_TOI) %>% group_by(position) %>% spread(metric, r.squared) %>% rename(reg_GP = GP, reg_TOI_all = TOI_all, reg_WAR = WAR )
  
  return(bind_rows(cc_war, cc_g, cc_TOI) %>% spread(metric, r.squared) %>% rename(reg_GP = GP, reg_TOI_all = TOI_all, reg_WAR = WAR ))
}

#fits linear regressions on previous three year data to obtain beta values, used to weight previous year data
find_prior_year_weightings <- function(df) {
  #only use players that have 4 years of history
  weighter <-
    df %>% filter(!is.na(WAR_plus_three) &
                    !is.na(WAR_plus_two) &
                    !is.na(WAR_plus_one) & !is.na(WAR))
  
  #fit models
  fit_war <-
    weighter %>% group_by(position) %>% do(model  = lm(data = ., WAR_plus_three ~ WAR_plus_two + WAR_plus_one + WAR))
  fit_G <-
    weighter %>% group_by(position) %>% do(model = lm(data = ., GP_plus_three ~ GP_plus_two + GP_plus_one + GP))
  fit_TOI <-
    weighter %>% group_by(position) %>% do(model = lm(
      data = .,
      TOI_all_plus_three ~ TOI_all_plus_two + TOI_all_plus_one + TOI_all
    ))
  
  #obtain estimated betas
  cc_war <-
    tidy(fit_war, model) %>% filter(term %in% c("WAR", "WAR_plus_one", "WAR_plus_two")) %>% select(position, term, estimate)
  
  cc_G <-
    tidy(fit_G, model) %>% filter(term %in% c("GP", "GP_plus_one", "GP_plus_two")) %>% select(position, term, estimate)
  
  cc_TOI <-
    tidy(fit_TOI, model) %>% filter(term %in% c("TOI_all", "TOI_all_plus_one", "TOI_all_plus_two")) %>% select(position, term, estimate)
  
  #combine all into one df that can be joined later
  all_weights <-
    bind_rows(cc_war, cc_G, cc_TOI) %>% spread(term, estimate) %>% rename(
      weight_GP = GP,
      weight_GP_plus_one = GP_plus_one,
      weight_GP_plus_two = GP_plus_two,
      weight_TOI_all = TOI_all,
      weight_TOI_all_plus_one = TOI_all_plus_one,
      weight_TOI_all_plus_two = TOI_all_plus_two,
      weight_WAR = WAR,
      weight_WAR_plus_one = WAR_plus_one,
      weight_WAR_plus_two = WAR_plus_two
    )
  
  return(
    bind_rows(cc_war, cc_G, cc_TOI) %>% spread(term, estimate) %>% rename(
      weight_GP = GP,
      weight_GP_plus_one = GP_plus_one,
      weight_GP_plus_two = GP_plus_two,
      weight_TOI_all = TOI_all,
      weight_TOI_all_plus_one = TOI_all_plus_one,
      weight_TOI_all = TOI_all_plus_two,
      weight_WAR = WAR,
      weight_WAR_plus_one = WAR_plus_one,
      weight_WAR = WAR_plus_two
    )
  )
}

#uses delta method to construct an aging curve (ask Lichtman how to properly smooth out data etc.)
construct_aging_curve <- function(df , birthdays) {
  
  #using couplets of back to back seasons find the difference in stat and weight by the harmonic mean of TOI. Then find the average of players in each age group. Remove last season to adjust for survivorship
  age <-
    test %>% filter(TOI_all > 60) %>% select(
      player,
      position,
      season,
      TOI_all,
      TOI_all_plus_one,
      WAR,
      season_age,
      age_plus_one,
      WAR_plus_one
    ) %>% mutate(
      WAR_diff = WAR_plus_one - WAR,
      age_diff = age_plus_one - season_age,
      TOI_diff = TOI_all_plus_one - TOI_all,
      age_group = paste(season_age, age_plus_one, sep = "-"),
      HM_weight = 2 / ((1 / TOI_all_plus_one) + (1 / TOI_all)),
      total_TOI = TOI_all + TOI_all_plus_one,
      weighted_WAR_diff = (WAR_diff * HM_weight) / total_TOI
    ) %>% filter(age_diff == 1 & row_number() < n())
  
  averages <-
    age %>% group_by(position, age_group, age_plus_one) %>% summarise(
      TOI_all_diff = mean(TOI_diff),
      WAR_diff = mean(weighted_WAR_diff),
      n = n()
    )
  
}

#weight data using lm method and set of weights
configure_past_three_seasons_data <-
  function(df, season_end, weights) {
    
    #get data from previous years necessary to make prediction and 
    minuses <-
      df %>% group_by(player) %>% arrange(player, season) %>% mutate(
        age_plus_one = lead(season_age),
        WAR_per_min = WAR / TOI_all,
        TOI_all_minus_one = lag(TOI_all),
        TOI_all_minus_two = lag(TOI_all, 2),
        TOI_all_minus_three = lag(TOI_all, 3),
        GP_minus_one = lag(GP),
        GP_minus_two = lag(GP, 2),
        GP_minus_three = lag(GP, 3),
        WAR_minus_one = lag(WAR),
        WAR_minus_two = lag(WAR, 2),
        WAR_minus_three = lag(WAR, 3),
        WAR_per_min_minus_one = lag(WAR_per_min),
        WAR_per_min_minus_two = lag(WAR_per_min, 2),
        WAR_per_min_minus_three = lag(WAR_per_min, 3)
      )
    
    #find seasons where players have previous 3 season's worth of data then use weights to obtain weighted average of seasons
    past_three_weighted <-
      minuses %>% group_by(player) %>% filter(!is.na(WAR_minus_one) &
                                                !is.na(WAR_minus_two) &
                                                !is.na(WAR_minus_three)) %>% select(
                                                  player,
                                                  position,
                                                  season,
                                                  TOI_all,
                                                  TOI_all_minus_one,
                                                  TOI_all_minus_two,
                                                  WAR,
                                                  WAR_minus_one,
                                                  WAR_minus_two,
                                                  WAR_per_min,
                                                  WAR_per_min_minus_one,
                                                  WAR_per_min_minus_three
                                                ) %>% inner_join(weights, by = 'position') %>% mutate(
                                                  weighted_TOI_all = (
                                                    TOI_all * weight_TOI_all_plus_two + TOI_all_minus_one * weight_TOI_all_plus_one + TOI_all_minus_two * weight_TOI_all
                                                  ) / (weight_TOI_all + weight_TOI_all_plus_two + weight_TOI_all_plus_one),
                                                  weighted_WAR = (
                                                    WAR * weight_WAR_plus_two + WAR_minus_one * weight_WAR_plus_one + WAR_minus_two * weight_WAR
                                                  ) / (weight_WAR+ weight_WAR_plus_two + weight_WAR_plus_one)
                                                )
    
  return(past_three_weighted %>% select(player, season, position, weighted_TOI_all, weighted_WAR))



  }

regress_data <- function(weighted_years, corr){
  #join on correlation data to know how much to regress
  to_regress<-weighted_years %>% inner_join(corr, by = "position")
  
  ###TODO####: find averages for stats
}

#use EH scraper to ping the NHL API for birthdays 
get_birthdays <- function() {
  require(broom)
  source('~/Documents/Evolving Hockey WAR Proj/EH_scrape_functions.R')
  birthdays <-
    bind_rows(
      sc.player_info_API("20182019"),
      sc.player_info_API("20172018"),
      sc.player_info_API("20162017"),
      sc.player_info_API("20152016"),
      sc.player_info_API("20142015"),
      sc.player_info_API("20132014"),
      sc.player_info_API("20122013"),
      sc.player_info_API("20112012"),
      sc.player_info_API("20102011"),
      sc.player_info_API("20092010"),
      sc.player_info_API("20082009"),
      sc.player_info_API("20072008")
    )
  return(birthdays)
}
