
df_pw <- rgdx.param(data_path,"out_ele_h") %>% 
  filter(Sr %in% Region,Sy==Year) %>% 
  filter(Sc !="historical") %>% 
  select(-Sr) %>% 
  mutate(Sc = as.character(Sc)) %>% 
  mutate(Sc=recode(Sc,!!!Sc_recode)) %>% 
  rename(Value=out_ele_h)

df_pw_cur <- rgdx.param(data_path,"ele_curtail") %>%
  filter(Sr %in% Region,Sy==Year) %>%
  filter(Sc !="historical") %>%
  select(-Sr) %>%
  mutate(Sc = as.character(Sc)) %>%
  mutate(Sc=recode(Sc,!!!Sc_recode)) %>%
  rename(Value=ele_curtail)
if (nrow(df_pw_cur) > 0){
  df_pw_cur$PW <- "Curtailment"
} 

df_pw_hyd <- rgdx.param(data_path,"ele2hyd_h") %>%
  filter(Sr %in% Region,Sy==Year) %>% 
  filter(Sc !="historical") %>% 
  select(-Sr) %>% 
  mutate(Sc = as.character(Sc)) %>%
  mutate(Sc=recode(Sc,!!!Sc_recode)) %>%
  rename(Value=ele2hyd_h)
if (nrow(df_pw_cur) > 0){
  df_pw_hyd$PW <- "Electrolysis"
} 

df_pw_sto <- rgdx.param(data_path,"ele_charge") %>%
  filter(Sr %in% Region,
         PW %in% c('STR_CAS','STR_SPHS','STR_DPHS','STR_BAT'),
         Sy==Year) %>% 
  filter(Sc !="historical") %>% 
  select(-Sr) %>% 
  mutate(Sc = as.character(Sc)) %>%
  mutate(Sc=recode(Sc,!!!Sc_recode)) %>%
  rename(Value=ele_charge)

df_pw_merged <- full_join(df_pw_cur,df_pw_hyd) %>%
  full_join(df_pw_sto) %>% 
  full_join(df_pw) %>% 
  select(-Sy)

df_pw_merged <- df_pw_merged %>%
  mutate(SS = as.character(SS)) %>%
  mutate(SS = recode(SS,!!!SS_recode))
df_pw_merged$TS <- gsub("t(\\d{2})", "\\1", df_pw_merged$TS)
df_pw_merged$TS <- as.numeric(df_pw_merged$TS)


df_pw_dem <- rgdx.param(data_path,"eledem_h") %>%
  filter(Sr %in% Region,Sy==Year) %>% 
  filter(Sc !="historical") %>% 
  select(-Sr) %>% 
  rename(Value=eledem_h)%>%
  mutate(Sc = as.character(Sc)) %>%
  mutate(Sc=recode(Sc,!!!Sc_recode)) %>%
  mutate(SS = as.character(SS)) %>%
  mutate(SS = recode(SS,!!!SS_recode))
df_pw_dem$PW <- "Demand"
df_pw_dem$TS <- gsub("t(\\d{2})", "\\1", df_pw_dem$TS)
df_pw_dem$TS <- as.numeric(df_pw_dem$TS)

#データの整形
plt <- set_plot(PW_list)

df_pw_24h <- df_pw_merged %>% 
  filter(PW %in% PW_list$PW) %>%
  filter(Sc == plot_Sc) %>% 
  group_by(SS, PW, TS) %>% 
  summarise(Value = sum(Value)) %>% 
  ungroup() %>% 
  mutate(SS = factor(SS, levels = SS_list)) %>% 
  mutate(PW = factor(PW, levels = PW_list$PW))

df_pw_dem <- df_pw_dem %>% 
  filter(Sc == plot_Sc) %>% 
  mutate(SS = factor(SS, levels = SS_list))
