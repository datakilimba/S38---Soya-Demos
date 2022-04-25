library(tidyverse)
library(httr)
library(RPostgres)

## general theme
theme_set(theme_void())

theme_update(
  axis.text.x = element_text(color = "black", face = "bold", 
                             margin = margin(t = 6)),
  axis.text.y = element_text(color = "black", hjust = 1, 
                             margin = margin(r = 6)),
  axis.line.x = element_line(color = "black", size = 1),
  panel.grid.major.y = element_line(color = "grey90", size = .6),
  plot.background = element_rect(fill = "white", color = "white"),
  plot.margin = margin(rep(20, 4))
)


## theme for horizontal charts
theme_flip <-
  theme(
    axis.text.x = element_text(face = "plain"),
    axis.text.y = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.major.y = element_blank(),
    legend.position = "top", 
    legend.title = element_text(face = "bold", margin = margin(b = 25))
  )



#message(params$year)
con = dbConnect(odbc::odbc(), "postgreSAKiRP")

district = dbReadTable(con,"districts")
village = dbReadTable(con,"villages")
ward = dbReadTable(con,"wards")
waeo = dbReadTable(con,"waeos")

soya_demo_url = "https://kc.humanitarianresponse.info/api/v1/data/1029872.csv"
soya_demo_rawdata = GET(soya_demo_url,authenticate(Sys.getenv("sakirp_user"),Sys.getenv("sakirp_pw")))

soya_demo_content = content(soya_demo_rawdata,"raw",encoding="UTF-8")
soya_demo_data_raw = read_csv(soya_demo_content) %>%
  janitor::clean_names()

soya_demo_data = soya_demo_data_raw %>% 
  select(
    District = ends_with("district"),
    Ward = ends_with("Ward"),
    Village = ends_with("Village"),
    Rain = ends_with("days_rain"),
    yield_1 = ends_with("yield_1"),
    yield_2 = ends_with("yield_2"),
    yield_3 = ends_with("yield_3"),
    yield_4 = ends_with("yield_4"),
    yield_5 = ends_with("yield_5"),
    yield_6 = ends_with("yield_6"),
    yield_7 = ends_with("yield_7"),
    yield_8 = ends_with("yield_8"),
    yield_9 = ends_with("yield_9"),
    yield_10 = ends_with("yield_10"),
    yield_11 = ends_with("yield_11"),
    yield_12 = ends_with("yield_12"),
    yield_13 = ends_with("yield_13"),
    yield_14 = ends_with("yield_14"),
    yield_15 = ends_with("yield_15"),
    yield_16 = ends_with("yield_16"),
    podcount_1 = ends_with("podcount_1"),
    podcount_2 = ends_with("podcount_2"),
    podcount_3 = ends_with("podcount_3"),
    podcount_4 = ends_with("podcount_4"),
    podcount_5 = ends_with("podcount_5"),
    podcount_6 = ends_with("podcount_6"),
    podcount_7 = ends_with("podcount_7"),
    podcount_8 = ends_with("podcount_8"),
    podcount_9 = ends_with("podcount_9"),
    podcount_10 = ends_with("podcount_10"),
    podcount_11 = ends_with("podcount_11"),
    podcount_12 = ends_with("podcount_12"),
    podcount_13 = ends_with("podcount_13"),
    podcount_14 = ends_with("podcount_14"),
    podcount_15 = ends_with("podcount_15"),
    podcount_16 = ends_with("podcount_16")
  ) 

soya_yield_data_long = soya_demo_data %>% 
  select(District:yield_16) %>% 
  pivot_longer(cols = yield_1:yield_16, names_to = "treatment",values_to = "yield") %>% 
  mutate(
    yield = case_when(
      yield %in% c(999,99999) ~ 0,
      TRUE ~ yield
    ),
    treatment = stringr::str_replace(treatment,"yield_","")
  )

soya_podcount_data_long = soya_demo_data %>% 
  select(District:Village,podcount_1:podcount_16) %>% 
  pivot_longer(cols = podcount_1:podcount_16, names_to = "treatment",
               values_to = "count") %>% 
  mutate(
    count = case_when(
      count %in% c(999,0) ~ 0,
      TRUE ~ count
    ),
    treatment = stringr::str_replace(treatment,"podcount_","")
  )

# Create dummy variables
soya_yield_data_dummy = soya_yield_data_long %>% 
  mutate(
    spike = case_when(
      treatment %in% c("2","8","10","16") ~ 1,
      TRUE ~ 0
    ),
    maksoy = case_when(
      treatment %in% c("4","6","12","14") ~ 1,
      TRUE ~ 0
    ),
    uyole = case_when(
      treatment %in% c("3","5","11","13") ~ 1,
      TRUE ~ 0
    ),
    local = case_when(
      treatment %in% c("1","7","9","15") ~ 1,
      TRUE ~ 0
    ),
    DAP = case_when(
      treatment %in% c("5","6","7","8") ~ 1,
      TRUE ~ 0
    ),
    NPK = case_when(
      treatment %in% c("9","10","11","12",
                       "13","14","15","16") ~ 1,
      TRUE ~ 0
    ),
    unfertilized = case_when(
      treatment %in% c("1","2","3","4") ~ 1,
      TRUE ~ 0
    ),
    inoculant = case_when(
      treatment %in% c("1","2","3","4",
                       "9", "10", "11","12") ~ 1,
      TRUE ~ 0
    )
  )

soya_podcount_data_dummy = soya_podcount_data_long %>% 
  mutate(
    spike = case_when(
      treatment %in% c("2","8","10","16") ~ 1,
      TRUE ~ 0
    ),
    maksoy = case_when(
      treatment %in% c("4","6","12","14") ~ 1,
      TRUE ~ 0
    ),
    uyole = case_when(
      treatment %in% c("3","5","11","13") ~ 1,
      TRUE ~ 0
    ),
    local = case_when(
      treatment %in% c("1","7","9","15") ~ 1,
      TRUE ~ 0
    ),
    DAP = case_when(
      treatment %in% c("5","6","7","8") ~ 1,
      TRUE ~ 0
    ),
    NPK = case_when(
      treatment %in% c("9","10","11","12",
                       "13","14","15","16") ~ 1,
      TRUE ~ 0
    ),
    unfertilized = case_when(
      treatment %in% c("1","2","3","4") ~ 1,
      TRUE ~ 0
    ),
    inoculant = case_when(
      treatment %in% c("1","2","3","4",
                       "9", "10", "11","12") ~ 1,
      TRUE ~ 0
    )
  )

soya_yield_podcount = soya_yield_data_dummy %>% 
  left_join(soya_podcount_data_dummy)


soya_yield_model = lm(yield ~ spike + maksoy + uyole + 
                        #local +
                        DAP + NPK + 
                        #unfertilized + 
                        inoculant,data = soya_yield_data_dummy)

soya_yield_model_interactions = lm(yield ~ spike*maksoy*uyole*DAP*NPK*inoculant,
                                   data = soya_yield_data_dummy)

summary(soya_yield_model)
summary(soya_yield_model_interactions)

yield_plot = ggplot(data = soya_yield_data_long,aes(fct_reorder(treatment,yield)
                                                    ,yield,fill=treatment )) +
  geom_boxplot() +
  theme(legend.position = "none")

ggsave(
  filename = "yield_plot.png",
  plot = last_plot(), 
  device = "png",
  path = here::here("R","images"),
  width = 6,
  height = 5,
  units = "in"
)

podcount_plot = ggplot(data = soya_podcount_data_long,aes(fct_reorder(treatment,count),count,fill=treatment )) +
  geom_boxplot() +
  theme(legend.position = "none")

ggsave(
  filename = "count_plot.png",
  plot = last_plot(), 
  device = "png",
  path = here::here("R","images"),
  width = 6,
  height = 5,
  units = "in"
)

yield_table_data = soya_yield_podcount %>% 
  group_by(treatment,spike,maksoy,uyole,local,DAP,NPK,unfertilized,inoculant) %>% 
  summarise(
    median_yield = median(yield),
    median_podcount = median(count)
  ) %>% 
  ungroup() %>% 
  mutate(across(c(spike,maksoy,uyole,local,DAP,NPK,unfertilized,inoculant),~ case_when(
    .==1 ~ "Yes",
    TRUE ~ "No"
  ))) %>% 
  select(
    Treatment = treatment,
    Spike = spike,
    Maksoy = maksoy,
    Uyole = uyole,
    Local = local,
    DAP,NPK,Unfertilized = unfertilized,
    Inoculant = inoculant,
    `Median Yield (g)` = median_yield,
    `Median Podcount` = median_podcount
  ) %>% arrange(-`Median Yield (g)`)

prettyfied_data = yield_table_data %>% 
  mutate(
    Spike = kableExtra::cell_spec(
      Spike, background = ifelse(Spike == "Yes","#D3D3D3",
                                 "white")),
    Maksoy = kableExtra::cell_spec(
      Maksoy, background = ifelse(Maksoy == "Yes","#D3D3D3",
                                  "white")),
    Uyole = kableExtra::cell_spec(
      Uyole, background = ifelse(Uyole == "Yes","#D3D3D3",
                                 "white")),
    Local = kableExtra::cell_spec(
      Local, background = ifelse(Local == "Yes","#D3D3D3",
                                 "white")),
    DAP = kableExtra::cell_spec(
      DAP, background = ifelse(DAP == "Yes","#D3D3D3",
                               "white")),
    NPK = kableExtra::cell_spec(
      NPK, background = ifelse(NPK == "Yes","#D3D3D3",
                               "white")),
    Unfertilized = kableExtra::cell_spec(
      Unfertilized, background = ifelse(Unfertilized == "Yes","#D3D3D3",
                                        "white")),
    Inoculant = kableExtra::cell_spec(
      Inoculant, background = ifelse(Inoculant == "Yes","#D3D3D3",
                                     "white")),
    `Kg/Acre` = round((4046.86/(2.5^2))*(`Median Yield (g)`/1000),0)
  ) %>% 
  kableExtra::kbl(escape = FALSE,format = "latex", booktabs = T) %>%
  kableExtra::kable_styling(latex_options=c("scale_down","float_left"))

highest_yield_df = yield_table_data %>% 
  filter(`Median Yield (g)` == max(`Median Yield (g)`))

highest_yield = highest_yield_df %>% 
  pull(`Median Yield (g)`)


highest_podcount_df = yield_table_data %>% 
  filter(`Median Podcount` == max(`Median Podcount`))

highest_podcount = highest_yield_df %>% 
  pull(`Median Podcount`)

variety_highest_podcount = highest_podcount_df %>% 
  select(Spike:Local,`Median Podcount`) %>% 
  pivot_longer(cols = Spike:Local) %>% 
  filter(value=="Yes")

variety_highest_podcount_name = variety_highest_podcount %>% 
  pull(name)

variety_highest_podcount_value = variety_highest_podcount %>% 
  pull(`Median Podcount`)

podcount_highest_yielding_name = highest_yield_df %>% 
  select(Spike:Local,`Median Podcount`) %>% 
  pivot_longer(cols = Spike:Local) %>% 
  filter(value=="Yes") %>% 
  pull(name)

podcount_highest_yielding_value = highest_yield_df %>% 
  select(Spike:Local,`Median Podcount`) %>% 
  pivot_longer(cols = Spike:Local) %>% 
  filter(value=="Yes") %>% 
  pull(`Median Podcount`)
