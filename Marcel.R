#to delete: temporary way of loading and storing original data, currently methodology is to filter out the 2012 lockout
hold_df <-
  read_csv('SkaterWarAll.csv', guess_max = 5000) %>% filter(season != 20122013)
hold_df$season <- as.character(hold_df$season)


#main control loop runs through and calls functions that call the various functions
control_loop <- function(df) {
  require(tidyverse)
  
  df <-
    read_csv('SkaterWarAll.csv', guess_max = 5000) %>% filter(season != 20122013)
  df$season <- as.character(df$season)
  
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
      WAR_per_min_plus_one = lead(WAR) / lead(TOI_all),
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
    ) %>% select(-Team)
  
  averages <-
    test %>% group_by(position) %>% summarise(
      average_WAR = mean(WAR),
      average_TOI_all = mean(TOI_all),
      average_WAR_per_minute = sum(WAR) / sum(TOI_all),
      n = n()
    )
  
  #contructs aging curves
  aging_curve <- construct_aging_curve(test)
  
  corr <- find_correlation_coef(test)
  
  weights <- find_prior_year_weightings(test)
  
  weighted_data <-
    configure_past_three_seasons_weighted_data(test, weights)
  
  regressed_data <- regress_data(weighted_data, corr, averages)
  
  
  
  
  #gets seasons with null data (sanity check)
  null_seasons <- !complete_cases(test)
  
  # return(#final_projections)
  
}

#finds yty correlation of stats we are trying to project so we can figure out the regression amount
find_correlation_coef <- function(df) {
  btb <- df %>% filter(age_plus_one - season_age == 1)
  
  #find correlations
  fit_war <-
    btb  %>% group_by(position) %>% do(model  = lm(data = ., WAR_plus_one ~ WAR))
  fit_G <-
    btb  %>% group_by(position) %>% do(model = lm(data = ., GP_plus_one ~ GP))
  fit_TOI <-
    btb  %>% group_by(position) %>% do(model = lm(data = ., TOI_all_plus_one  ~ TOI_all))
  
  fit_war_per_min <-
    btb  %>% group_by(position) %>% do(model = lm(data = ., WAR_per_min_plus_one ~ WAR_per_min))
  
  #obtain and combine the various r^2
  cc_war <-
    glance(fit_war, model) %>% select(position, r.squared) %>% mutate(metric = 'WAR', cor = sqrt(r.squared))
  cc_g <-
    glance(fit_G, model) %>% select(position, r.squared) %>% mutate(metric = 'GP', cor = sqrt(r.squared))
  cc_TOI <-
    glance(fit_TOI, model) %>% select(position, r.squared) %>% mutate(metric = 'TOI_all', cor = sqrt(r.squared))
  
  cc_war_per_min <-
    glance(fit_war_per_min, model) %>% select(position, r.squared) %>% mutate(metric = 'WAR_per_min', cor = sqrt(r.squared))
  
  corr <-
    bind_rows(cc_war, cc_g, cc_TOI, cc_war_per_min) %>% select(-r.squared) %>% spread(metric, cor) %>% rename(
      reg_GP = GP,
      reg_TOI_all = TOI_all,
      reg_WAR = WAR,
      reg_WAR_per_min = WAR_per_min
    )
  
  return(
    bind_rows(cc_war, cc_g, cc_TOI, cc_war_per_min) %>% select(-r.squared) %>% spread(metric, cor) %>% rename(
      reg_GP = GP,
      reg_TOI_all = TOI_all,
      reg_WAR = WAR,
      reg_WAR_per_min = WAR_per_min
    )
  )
}

#fits linear regressions on previous three year data to obtain beta values, used to weight previous year data
find_prior_year_weightings <- function(df) {
  #only use players that have 4 years of history
  weighter <-
    df %>% filter(
      !is.na(WAR_minus_one) &
        !is.na(WAR_minus_two) &
        !is.na(WAR_minus_three) & !is.na(WAR)
    )
  
  #fit models
  fit_war <-
    weighter %>% group_by(position) %>% do(model  = lm(data = ., WAR ~ WAR_minus_one + WAR_minus_two + WAR_minus_three))
  fit_G <-
    weighter %>% group_by(position) %>% do(model = lm(data = ., GP ~ GP_minus_one + GP_minus_two + GP_minus_three))
  fit_TOI <-
    weighter %>% group_by(position) %>% do(
      model = lm(
        data = .,
        TOI_all ~ TOI_all_minus_one + TOI_all_minus_two + TOI_all_minus_three
      )
    )
  
  fit_war_per_TOI <-
    weighter %>% group_by(position) %>% do(
      model  = lm(
        data = .,
        WAR_per_min ~ WAR_per_min_minus_one + WAR_per_min_minus_two + WAR_per_min_minus_three
      )
    )
  
  #obtain estimated betas
  cc_war <-
    tidy(fit_war, model) %>% filter(term %in% c("WAR_minus_one", "WAR_minus_two", "WAR_minus_three")) %>% select(position, term, estimate)
  
  cc_G <-
    tidy(fit_G, model) %>% filter(term %in% c("GP_minus_one", "GP_minus_two", "GP_minus_three")) %>% select(position, term, estimate)
  
  cc_TOI <-
    tidy(fit_TOI, model) %>% filter(term %in% c(
      "TOI_all_minus_one",
      "TOI_all_minus_two",
      "TOI_all_minus_three"
    )) %>% select(position, term, estimate)
  
  cc_war_per_min <-
    tidy(fit_war_per_TOI, model) %>% filter(
      term %in% c(
        "WAR_per_min_minus_one",
        "WAR_per_min_minus_two",
        "WAR_per_min_minus_three"
      )
    ) %>% select(position, term, estimate)
  #combine all into one df that can be joined later
  all_weights <-
    bind_rows(cc_war, cc_G, cc_TOI, cc_war_per_min) %>% spread(term, estimate) %>% rename(
      weight_GP_minus_one = GP_minus_one,
      weight_GP_minus_two = GP_minus_two,
      weight_GP_minus_three = GP_minus_three,
      weight_TOI_all_minus_one = TOI_all_minus_one,
      weight_TOI_all_minus_two = TOI_all_minus_two,
      weight_TOI_all_minus_three = TOI_all_minus_three,
      weight_WAR_minus_one = WAR_minus_one,
      weight_WAR_minus_two = WAR_minus_two,
      weight_WAR_minus_three = WAR_minus_three,
      weight_WAR_per_min_minus_one = WAR_per_min_minus_one,
      weight_WAR_per_min_minus_two = WAR_per_min_minus_two,
      weight_WAR_per_min_minus_three = WAR_per_min_minus_three
    )
  
  return(
    bind_rows(cc_war, cc_G, cc_TOI, cc_war_per_min) %>% spread(term, estimate) %>% rename(
      weight_GP_minus_one = GP_minus_one,
      weight_GP_minus_two = GP_minus_two,
      weight_GP_minus_three = GP_minus_three,
      weight_TOI_all_minus_one = TOI_all_minus_one,
      weight_TOI_all_minus_two = TOI_all_minus_two,
      weight_TOI_all_minus_three = TOI_all_minus_three,
      weight_WAR_minus_one = WAR_minus_one,
      weight_WAR_minus_two = WAR_minus_two,
      weight_WAR_minus_three = WAR_minus_three,
      weight_WAR_per_min_minus_one = WAR_per_min_minus_one,
      weight_WAR_per_min_minus_two = WAR_per_min_minus_two,
      weight_WAR_per_min_minus_three = WAR_per_min_minus_three
    )
  )
}

#uses delta method to construct an aging curve (ask Lichtman how to properly smooth out data etc.)
construct_aging_curve <- function(df) {
  #using couplets of back to back seasons find the difference in stat and weight by the harmonic mean of TOI. Then find the average of players in each age group. Remove last season to adjust for survivorship
  age <-
    df %>% filter(TOI_all > 60) %>% select(
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
configure_past_three_seasons_weighted_data <-
  function(df, weights) {
    #get data from previous years necessary to make prediction and
    # minuses <-
    #   df %>% group_by(player) %>% arrange(player, season) %>% mutate(
    #     age_plus_one = lead(season_age),
    #     WAR_per_min = WAR / TOI_all,
    #     TOI_all_minus_one = lag(TOI_all),
    #     TOI_all_minus_two = lag(TOI_all, 2),
    #     TOI_all_minus_three = lag(TOI_all, 3),
    #     GP_minus_one = lag(GP),
    #     GP_minus_two = lag(GP, 2),
    #     GP_minus_three = lag(GP, 3),
    #     WAR_minus_one = lag(WAR),
    #     WAR_minus_two = lag(WAR, 2),
    #     WAR_minus_three = lag(WAR, 3),
    #     WAR_per_min_minus_one = lag(WAR_per_min),
    #     WAR_per_min_minus_two = lag(WAR_per_min, 2),
    #     WAR_per_min_minus_three = lag(WAR_per_min, 3)
    #   )
    
    #find seasons where players have previous 3 season's worth of data then use weights to obtain weighted average of seasons
    past_three_weighted <-
      df %>% group_by(player) %>% filter(!is.na(WAR_minus_one) &
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
                                             WAR_per_min_minus_two,
                                             WAR_per_min_minus_three
                                           ) %>% inner_join(weights, by = 'position') %>% mutate(
                                             weighted_TOI_all = (
                                               TOI_all * weight_TOI_all_minus_one + TOI_all_minus_one * weight_TOI_all_minus_two + TOI_all_minus_two * weight_TOI_all_minus_three
                                             ) / (
                                               weight_TOI_all_minus_one + weight_TOI_all_minus_two + weight_TOI_all_minus_three
                                             ),
                                             weighted_WAR = (
                                               WAR * weight_WAR_minus_one + WAR_minus_one * weight_WAR_minus_two + WAR_minus_two * weight_WAR_minus_three
                                             ) / (
                                               weight_WAR_minus_one + weight_WAR_minus_two + weight_WAR_minus_three
                                             ),
                                             weighted_WAR_per_min = (
                                               WAR_per_min * weight_WAR_per_min_minus_one + WAR_per_min_minus_two * weight_WAR_per_min_minus_two + WAR_per_min_minus_three * weight_WAR_per_min_minus_three
                                             ) / (
                                               weight_WAR_per_min_minus_one + weight_WAR_per_min_minus_two + weight_WAR_per_min_minus_three
                                             )
                                           )
    
    return(
      past_three_weighted %>% select(
        player,
        season,
        position,
        weighted_TOI_all,
        weighted_WAR,
        weighted_WAR_per_min
      )
    )
    
    
    
  }

regress_data <- function(weighted_years, corr, averages) {
  #join on correlation data to know how much to regress
  to_regress <-
    weighted_years %>% inner_join(corr, by = "position") %>% inner_join(averages %>% select(-n), by = 'position') %>% mutate(
      weighted_WAR_per_TOI = weighted_WAR / weighted_TOI_all,
      reg_constant_WAR_per_min = ((1 - reg_WAR_per_min) * average_TOI_all) / reg_WAR_per_min,
      reg_constant_WAR = (1 - reg_WAR) / reg_WAR,
      regress_WAR_rate = (
        weighted_WAR_per_TOI + (reg_constant_WAR * ((average_WAR) / average_TOI_all)) /
          (weighted_TOI_all + reg_constant_WAR)
      ),
      regressed_TOI = (1 - reg_TOI_all) * average_TOI_all + reg_TOI_all *
        weighted_TOI_all,
      regressed_WAR = (1 - reg_WAR) * average_WAR + reg_WAR * weighted_WAR,
      regressed_total_WAR = regress_WAR_rate * regressed_TOI,
      regressed_WAR_per_min = ((weighted_WAR_per_min * weighted_TOI_all) + (reg_constant_WAR_per_min * ((average_WAR) / average_TOI_all))
      ) / (reg_constant_WAR_per_min + weighted_TOI_all),
      regressed_total_WAR_two = regressed_WAR_per_min * regressed_TOI
    ) %>% arrange(desc(season), desc(regressed_total_WAR))
  
  
  to_regress <-
    weighted_data %>% inner_join(corr, by = "position") %>% inner_join(averages %>% select(-n), by = 'position') %>% mutate(
      avg_TOI_to_add = ((reg_WAR_per_min) / (1 - reg_WAR_per_min)) * average_TOI_all,
      weighted_WAR_per_min = weighted_WAR/weighted_TOI_all,
      regressed_WAR = ((weighted_WAR_per_min * weighted_TOI_all) + (avg_TOI_to_add * average_WAR_per_minute)
      ) /  (weighted_TOI_all + avg_TOI_to_add) * weighted_TOI_all
    )
  
  return(to_regress)
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
