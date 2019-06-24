


configure_year_plus_one <- function(df) {
  require(tidyverse)
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
  
  birthdays <- get_birthdays()
  
  test <-
    df %>% group_by(player, position, season) %>% filter(n() == 1) %>% bind_rows(double_players) %>% inner_join(birthdays %>% select(player, positioncode, shootscatches, birthday, season, season_age, current_age), by = c("player", "season" )) %>% group_by(player) %>% mutate(
      age_plus_one = lead(season_age),
      WAR_per_min = WAR / TOI_all,
      TOI_all_plus_one = lead(TOI_all),
      GP_plus_one = lead(GP),
      WAR_plus_one = lead(WAR),
      WAR_per_min_plus_one = lead(WAR / TOI_all)
    ) %>% select(-Team) %>% arrange(player, season, season_age)
  
  aging_curve<- construct_aging_curve(test)
  
  null_seasons <- test %>% filter(!complete.cases(.))
  
  return(test %>% na.omit())
  
}

find_correlation <- function(df) {
  fit_war <-
    df %>% group_by(position) %>% do(model  = lm(data = ., WAR_plus_one ~ WAR))
  fit_G <-
    df %>% group_by(position) %>% do(model = lm(data = ., GP_plus_one ~ GP))
  fit_TOI <-
    df %>% group_by(position) %>% do(model = lm(data = ., TOI_all_plus_one  ~ TOI_all))
  cc_war <-
    glance(fit_war, model) %>% select(position, r.squared) %>% mutate(metric = 'WAR', cor = sqrt(r.squared))
  cc_g <-
    glance(fit_G, model) %>% select(position, r.squared) %>% mutate(metric = 'GP', cor = sqrt(r.squared))
  cc_TOI <-
    glance(fit_TOI, model) %>% select(position, r.squared) %>% mutate(metric = 'TOI_all', cor = sqrt(r.squared))
  return(bind_rows(cc_war, cc_g, cc_TOI))
}

construct_aging_curve <- function(df , birthdays) {
  age <- test %>% select(player, position, season, TOI_all, TOI_all_plus_one, WAR, season_age, age_plus_one, WAR_plus_one) %>% mutate(WAR_diff = WAR_plus_one - WAR, age_diff = age_plus_one - season_age, TOI_diff = TOI_all_plus_one - TOI_all, age_group = paste(season_age, age_plus_one, sep = "-"), HM_weight = 2/((1/TOI_all_plus_one) + (1/TOI_all)), total_TOI = TOI_all + TOI_all_plus_one, weighted_WAR_diff = (WAR_diff * HM_weight)/total_TOI) %>% filter(age_diff == 1 & row_number() < n())
  
  averages <- age %>% group_by(position, age_group) %>% summarise(TOI_all_diff = mean(TOI_diff), WAR_diff = mean(weighted_WAR_diff), n = n())
  
}

get_birthdays <- function() {
  require(broom)
  source('~/Documents/Evolving Hockey WAR Proj/EH_scrape_functions.R')
  birthdays <-
    bind_rows(
      sc.player_info_API("20182019"),
      sc.player_info_API("20172018"),
      sc.player_info_API("20162017"),
      sc.player_info_API("20152016"),
      sc.player_info_API("20142015")
    )
  return(birthdays)
}
