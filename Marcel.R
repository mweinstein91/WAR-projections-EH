library(broom)


configure_year_plus_one <- function(df) {
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
  
      test<- df %>% group_by(player, position, season) %>% filter(n() == 1) %>% bind_rows(double_players) %>% group_by(player) %>% mutate(
        WAR_per_min = WAR / TOI_all,
        TOI_all_plus_one = lead(TOI_all),
        GP_plus_one = lead(GP),
        WAR_plus_one = lead(WAR),
        WAR_per_min_plus_one = lead(WAR / TOI_all)
      ) %>% select(-Team) 
      
      null_seasons <- test %>% filter(!complete.cases(.))
      
      return(test %>% na.omit())
      
}

find_correlation <- function(df) {
  fit_war <- df %>% group_by(position) %>% do(model  = lm(data = ., WAR_plus_one ~ WAR))
  fit_G <- df %>% group_by(position) %>% do(model = lm(data = ., GP_plus_one ~ GP))
  fit_TOI <- df %>% group_by(position) %>% do(model = lm(data = ., TOI_all_plus_one  ~ TOI_all))
  cc_war <- glance(fit_war, model) %>% select(position, r.squared) %>% mutate(metric = 'WAR', cor = sqrt(r.squared))
  cc_g <- glance(fit_G, model) %>% select(position, r.squared) %>% mutate(metric = 'GP',cor = sqrt(r.squared))
  cc_TOI <- glance(fit_TOI, model) %>% select(position, r.squared) %>% mutate(metric = 'TOI_all',cor = sqrt(r.squared))
  return(bind_rows(cc_war, cc_g, cc_TOI))
}

construct_aging_curve <- function(df ,birthdays){
  
  
  
}

get_birthdays <- function(){
  source('~/Documents/Evolving Hockey WAR Proj/EH_scrape_functions.R')
  birthdays<-bind_rows(sc.player_info_API("20182019"),sc.player_info_API("20172018"), sc.player_info_API("20162017"), sc.player_info_API("20152016"), sc.player_info_API("20142015"))
  return(birthdays)
}

