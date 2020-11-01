rm(list = ls()); gc();
#### Libraries, Options and Parameters.####
options(java.parameters = "-Xmx48g",scipen=999)

#Calls for libraries (or installs them if missing).
#It is worth noting that this code has to be executed on RStudio IDE due to
#the rstudioapi package. It offers the rstudio::getActiveDocumentContext() that
#allows relative paths.

vec.pkg <- c('MASS',"rstudioapi","lubridate","tidyverse","tictoc",'readxl')
vec.newpkg <- vec.pkg[!(vec.pkg %in% installed.packages()[,"Package"])]
if(length(vec.newpkg)) install.packages(vec.newpkg)
lapply(vec.pkg, require, character.only = TRUE)
rm(vec.pkg,vec.newpkg)
 
#Sets the current folder of the script as the working directory.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



ZZZ_VLookUp_Holidays <- readxl::read_excel('Other_Data/PSD_Holidays.xlsx') %>%
  mutate(Date = ymd(Date))

#### Boundaries (structural (hourly) maximum, maximum and minimum for the PSD). ####
ZZZ_bound <- data.frame(Year = c(2018:2020),
                       PSD_min = c(40.16, 42.35, 39.68),
                       PSD_max = c(505.18, 513.89, 559.75),
                       PSD_max_struc = c(NA,NA,1148.36),
                       stringsAsFactors = F)

#### Files (download via CCEE's website) are on relative paths. ####
# They are treated under the "tidyverse" package and "tidy data philosophy/methodology"
# Worth mentioning that the manual date '2018-09-08' sets the largest period
# of Hourly PSD time series without 'errors and omissions' of CCEE database.
# Luckly, it stars on a saturday, which is essential for the ID_Week "over partition by" calculation
# to get ID_Week on the Hourly PSD dataframe.

ZZZ_Weekly_PSD <- bind_rows(... = 
  lapply(list.files('PSD/',full.names = T),function(x){
    readxl::read_excel(x,trim_ws = T) %>% 
    rename(Year_Weekly = Ano, Month_Weekly = Mês, Week = Semana, 
           Start_Date = `Data Início`,
           End_Date = `Data Fim`) %>% 
    mutate(Start_Date = ymd(Start_Date),End_Date = ymd(End_Date)) %>%   
    gather(data =.,key='SubMkt_LoadStep',value='PSD',-(Year_Weekly:End_Date)) %>%
    separate(SubMkt_LoadStep, c("LoadStep","SubMkt"), sep = " ") %>%
    mutate(SubMkt = factor(SubMkt,labels=c('NE','SE','S','N'),levels=c('NE','SE','S','N')),
           #Ordered factor.
           LoadStep = factor(LoadStep,ordered=T,levels=c('Leve','Médio','Pesado')),
           PSD = as.numeric(PSD))
  })
) %>% arrange(Start_Date)

ZZZ_Hourly_PSD <- bind_rows(... = 
  lapply(list.files('PSDh/',full.names = T),function(x){
    readxl::read_excel(x,trim_ws = T) %>% rename(Hour_Hourly = `...1`,SubMkt=`...2`) %>% 
      
      filter(Hour_Hourly != 'Hora') %>% gather(data =.,key='Date',value='PSDh',-Hour_Hourly,-SubMkt) %>%
      filter(!is.na(PSDh)) %>% 
      mutate(PSDh = as.numeric(PSDh),SubMkt = as.factor(SubMkt),
             Hour_Hourly=as.numeric(Hour_Hourly),
             Date = as.Date(as.numeric(Date),origin='1899-12-30'),
             
             SubMkt = case_when(
               toupper(SubMkt) == 'SUDESTE' ~ 'SE',
               toupper(SubMkt) == 'NORTE' ~ 'N',
               toupper(SubMkt) == 'SUL' ~ 'S',
               toupper(SubMkt) == 'NORDESTE' ~ 'NE',
               T ~ 'Erro'),
             SubMkt = factor(SubMkt,labels=c('NE','SE','S','N'),levels=c('NE','SE','S','N'))
             ) %>%
      mutate(Key = as.POSIXct(strptime(paste0(Date,' 00:00'),"%Y-%m-%d %H:%M"),tz = "America/Buenos_Aires") + (Hour_Hourly*1*60*60))
  }) 
) %>% #filter(Date >= ymd('2018-10-01')) %>% 
  arrange(Date,Hour_Hourly)

if(ZZZ_Hourly_PSD %>% group_by(Date,Hour_Hourly,SubMkt) %>% filter(n()!=1) %>% nrow() != 0){
  print('Number of rows different from expected quantity of hours!')
}else{} 

# Commented piece: calculates time interval between two dates.
# T_length <- time_length(
#   interval(
#     min(ZZZ_Hourly_PSD$Date),
#     max(ZZZ_Hourly_PSD$Date)
#   ),"day")

# Commented piece: equally lengthed partition; time-vector.
# vec.tempo.teste <- lubridate::ymd(min(ZZZ_Hourly_PSD$Date)) %m+% days(0:763)

# After the proper data cleaning of the Weekly and Hourly PSD's, 
# the argument of fnc.VLookUp_Weekly_vs_Hourly(df_Days_PSDh) is settled.

#### LoadSteps per hours, season and type: bind_rows through years. ####
ZZZ_LoadSteps <- bind_rows(... = 
  lapply(list.files('Other_Data/',pattern = 'LoadStep_*',full.names = T),
    function(x){
      x <- readxl::read_excel(x,trim_ws = T)
      colnames(x) <- str_to_upper(colnames(x))
      return(x %>% mutate_all(as.character))
      })) %>% 
  mutate(Date = as.Date(DIA,origin='1899-12-30'), Hour_LoadStep = str_sub(HORA,-8,-4),
         Week_LoadStep = str_sub(SEMANA,-1,-1), 
         WeekDay_LoadStep = `DIA SEMANA`,  Type_LoadStep = TIPO) %>% 
  mutate(DayLightType = str_to_lower(`TIPO HORÁRIO`)) %>% 
  mutate(DayLightType = if_else(is.na(DayLightType),'horário normal',DayLightType)) %>% 
  mutate(LoadStep = PATAMAR) %>%
  mutate(LoadStep = case_when(
    str_to_upper(LoadStep) == 'LEVE' ~ 'Leve',
    str_to_upper(LoadStep) == 'MÉDIO' ~ 'Médio',
    str_to_upper(LoadStep) == 'PESADO' ~ 'Pesado',
    T ~ 'Erro'
  )) %>%  mutate(LoadStep = factor(LoadStep,ordered=T,levels=c('Leve','Médio','Pesado'))) %>% 
  dplyr::select((Date:LoadStep)) %>% 
  #Strictly necessary: there is an unexplicable behavior under 'America/Sao_Paulo' time zone.
  mutate(Key = as.POSIXct(strptime(paste0(Date,' ',Hour_LoadStep),"%Y-%m-%d %H:%M"),tz = "America/Buenos_Aires"))

# Test to verify which month is lacking days on a Hourly PSD basis.
# View(
#   ZZZ_Hourly_PSD %>% select(Date) %>% 
#     mutate(`Year/Month` = paste0(str_sub(Date,1,4),'/',str_sub(Date,6,7))) %>% distinct() %>% 
#     mutate(fnc_Days = days_in_month(Date)) %>% 
#     group_by(fnc_Days,`Year/Month`) %>% mutate(Days_in_Month = n()) %>%
#     ungroup() %>% filter(Days_in_Month != fnc_Days)
#   )
#       

source('Sub/function_dataframe_PSD.R')

dataframe_PSD <- fnc.dataframe_PSD(
  ZZZ_Hourly_PSD=ZZZ_Hourly_PSD,
  ZZZ_Weekly_PSD=ZZZ_Weekly_PSD,
  ZZZ_LoadSteps=ZZZ_LoadSteps,
  ZZZ_VLookUp_Holidays=ZZZ_VLookUp_Holidays
  )

writexl::write_xlsx(x = dataframe_PSD,'../Stochastic_Processes_PSD_Output/dataframe_PSD.xlsx')


df_SE_PSD <- dataframe_PSD %>% filter(SubMkt == 'SE')

df_SE_PSD %>% ggplot(data=.,aes(x = log_diff,fill=LoadStep)) + 
  geom_histogram(aes(y=..density..)) +
#  geom_histogram(aes(y=(..count..)*100/(sum(..count..)))) + 
  facet_grid(rows=vars(Type_LoadStep),cols=vars(LoadStep,DayLightType))


hist(df_SE_PSD$diff_nom)
hist(df_SE_PSD$diff_perc[df_SE_PSD$LoadStep=='Leve'])
hist(df_SE_PSD$diff_perc[df_SE_PSD$LoadStep=='Médio'])
hist(df_SE_PSD$diff_perc[df_SE_PSD$LoadStep=='Pesado'])

x <- fitdistr(x = exp(df_SE_PSD$log_diff[df_SE_PSD$LoadStep=='Leve']),densfun = 'gamma')

h <- hist((df_SE_PSD$log_diff[df_SE_PSD$LoadStep=='Leve']), freq = F, plot = T)
bins_ret <- h$mids
?dlnorm
fit_N <- dlnorm(bins_ret,shape = x$estimate[1],scale = x$estimate[2])
hist((df_SE_PSD$log_diff[df_SE_PSD$LoadStep=='Leve']), freq = F, plot = T)
lines(x = bins_ret, y = fit_N, col = "green", lty = 2, lwd = 2)
fit_N
fit_N <- dweibull(bins_ret,shape = x$estimate[1],scale = x$estimate[2])

?fitdistr
exp(.25)

log(52/50)
(52/50)-1
exp(log(52/50))
