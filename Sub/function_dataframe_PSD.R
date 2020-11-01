fnc.dataframe_PSD <- function(ZZZ_Hourly_PSD,ZZZ_Weekly_PSD,ZZZ_LoadSteps,ZZZ_VLookUp_Holidays){
  
  vMinDate <- min(ZZZ_Hourly_PSD$Date)
  vMaxDate <- max(ZZZ_Hourly_PSD$Date)
  vRefWDay <- wday(ymd(vMaxDate),week_start = 6)
  vRefEndDate <- min(ymd(ZZZ_Weekly_PSD$End_Date))

  
  # Criteria to filter the starting weekly prices on End_Date vs. vMinDate
  df_W_PSD <- ZZZ_Weekly_PSD %>% filter(End_Date >= vMinDate) %>% 
    group_by(SubMkt, LoadStep) %>% mutate(ID_Week = row_number()) %>% ungroup()
  
  vec.Start_Date <- unique(df_W_PSD$Start_Date)  
  df_H_PSD <- ZZZ_Hourly_PSD %>% mutate(ID_Week = 0)
  
  for(RefDate in vec.Start_Date){
    df_H_PSD <- mutate(.data = df_H_PSD,ID_Week = if_else(Date >= RefDate,ID_Week+1,ID_Week))      
  }      

  df <- df_H_PSD %>% 
    left_join(ZZZ_LoadSteps, by=c('Key'='Key','Date'='Date')) %>%
    left_join(df_W_PSD,by=c('ID_Week'='ID_Week','SubMkt'='SubMkt','LoadStep'='LoadStep')) %>%
    mutate(Year = str_sub(Date,1,4)) %>% 
    rename(timestamp = Key) %>% mutate(`Year/Month` = paste0(Year,'/',str_sub(Date,6,7)))

  df <- df %>%
    mutate(Month = lubridate::month(Date)) %>%
    mutate(Month_label = lubridate::month(Date,label=T,abbr=T)) %>%
    mutate(CurrWDay = wday(Date,label=T,abbr=T,week_start = 6)) %>% 
    
    mutate(WeekDay_LoadStep = case_when(
      str_to_lower(WeekDay_LoadStep) == 'segunda-feira' ~ 'seg',
      str_to_lower(WeekDay_LoadStep) == 'ter�a-feira' ~ 'ter',
      str_to_lower(WeekDay_LoadStep) == 'quarta-feira' ~ 'qua',
      str_to_lower(WeekDay_LoadStep) == 'quinta-feira' ~ 'qui',
      str_to_lower(WeekDay_LoadStep) == 'sexta-feira' ~ 'sex',
      str_to_lower(WeekDay_LoadStep) == 'feriado' ~ 'fer',
      str_to_lower(WeekDay_LoadStep) == 's�bado' ~ 's�b',
      str_to_lower(WeekDay_LoadStep) == 'domingo' ~ 'dom',
    )) %>% 
    mutate(WeekDay_LoadStep = factor(WeekDay_LoadStep,ordered =F,levels=c('s�b','dom','fer','seg','ter','qua','qui','sex'))) %>%          
    #Current WeekDay (without holiday classification).
    mutate(CurrWDay = wday(Date,label=T,abbr=T,week_start = 6)) %>% 
    #Adjustment on the current week days: holiday classification comes from ZZZ_VLookUp_Holidays.
    mutate(WDay = if_else(Date %in% ZZZ_VLookUp_Holidays$Date,'fer',as.character(CurrWDay))) %>%
    #Factor variable is NOT ordered!
    mutate(WDay = factor(WDay,ordered =F,levels=c('s�b','dom','fer','seg','ter','qua','qui','sex'))) %>% 
    
  # %>%          
  #   mutate(Type_LoadStep = if_else(WeekDay_LoadStep %in% '')) %>%
  #   mutate()
    # #Season, order factor by month.
    # mutate(Season = case_when(
    #   #Summer
    #   ((Month >= 1 & Month <= 3)) ~ 'Ver�o - T1',
    #   #Intermediate
    #   ((Month == 4)) ~ 'Intra-Estac. - Abr',
    #   #Winter
    #   Month >= 5 & Month <= 8 ~ 'Inverno',
    #   #Intermediate
    #   ((Month >= 9 & Month <= 10)) ~ 'Intra-Estac. - Set/Out',
    #   #Summer
    #   ((Month >= 11 & Month <= 12)) ~ 'Ver�o - T4',
    #   T ~ 'Erro'),
    #   Season = factor(Season,ordered=T,levels = c(
    #     'Ver�o - T1','Intra-Estac. - Abr',
    #     'Inverno','Intra-Estac. - Set/Out',
    #     'Ver�o - T4')
    #   )
    # )
    # 
    mutate(log_diff = log(PSDh/PSD),diff_perc = (PSDh/PSD)-1,diff_nom = (PSDh-PSD)) %>% 
    dplyr::select(SubMkt,Date,`Year/Month`,Hour_LoadStep,WeekDay_LoadStep,WDay,LoadStep,
           PSD,PSDh,log_diff,diff_perc,diff_nom,everything())
  
  return(df)
}


