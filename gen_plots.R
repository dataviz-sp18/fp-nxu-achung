library(tidyverse)
library(knitr)
library(broom)
library(stringr)
library(modelr)
library(forcats)
library(tidytext)
library(reshape2)
library(readstata13)
library(stargazer)
library(gtools)
library(SparseM)

options(digits = 3)
set.seed(1234)
theme_set(theme_minimal())

df2010<-read.dta13('data/data_2010.dta',
                   missing.type = T,
                   nonint.factors = T,
                   generate.factors = T) %>%
  mutate_each(funs(replace(., . == "Not applicable", NA))) %>%
  mutate_each(funs(replace(., . == "Not applicable_(-8)", NA))) %>%
  mutate_each(funs(replace(., . == "Unknown", NA))) 

df2014<-read.dta13('data/data_2014.dta',
                   missing.type = T,
                   nonint.factors = T,
                   generate.factors = T) %>%
  mutate_each(funs(replace(., . == "Not applicable", NA))) %>%
  mutate_each(funs(replace(., . == "Not applicable_(-8)", NA))) %>%
  mutate_each(funs(replace(., . == "Unknown", NA)))


# dat <- smartbind(df2010, df2014)

dat <- df2010 %>%
  inner_join(df2014, by = "pid", suffix = c("_2010", "_2014")) %>%
  filter(!is.na(income_2014) & !is.na(income_2010) & ca5_2010 != "Other" & ca5_2014 != "Other" & ca6_2010 != "Other" & ca6_2014 != "Other")

joined <- dat %>% 
  mutate(income_2014 = as.numeric(as.character(income_2014)),
         income_2010 = as.numeric(as.character(income_2010)),
         income_diff = income_2014 - income_2010,
         age_2014 = as.numeric(as.character(age_2014)),
         age_2010 = as.numeric(as.character(age_2010)),
         gender_2014 = ifelse(gender_2014 == "Male", 
                              "Male", "Female"),
         gender_2010 = ifelse(gender_2010 == "Male", "Male", "Female"),
         educ_2010 = as.numeric(as.character(educ_2010)),
         educ_2014 = as.numeric(as.character(educ_2014))) %>%
  mutate(
    convenience_diff = as.numeric(as.character(ca301_a_1_2014)) - as.numeric(as.character(ca301_a_1_2010)),
    kindergarten_diff = as.numeric(as.character(ca301_a_2_2014)) - as.numeric(as.character(ca301_a_2_2010)),
    primsch_diff = as.numeric(as.character(ca301_a_3_2014)) - as.numeric(as.character(ca301_a_3_2010)),
    hospital_diff = as.numeric(as.character(ca301_a_4_2014)) - as.numeric(as.character(ca301_a_4_2010)),
    drugstore_diff = as.numeric(as.character(ca301_a_5_2014)) - as.numeric(as.character(ca301_a_5_2010)),
    temple_diff = as.numeric(as.character(ca301_a_6_2014)) - as.numeric(as.character(ca301_a_6_2010)),
    ancestral_diff = as.numeric(as.character(ca301_a_7_2014)) - as.numeric(as.character(ca301_a_7_2010)),
    church_diff = as.numeric(as.character(ca301_a_8_2014)) - as.numeric(as.character(ca301_a_8_2010)),
    actvty_diff = as.numeric(as.character(ca301_a_9_2014)) - as.numeric(as.character(ca301_a_9_2010)),
    nursing_diff = as.numeric(as.character(ca301_a_10_2014)) - as.numeric(as.character(ca301_a_10_2010)),
    physfacty_diff = as.numeric(as.character(ca301_a_11_2014)) - as.numeric(as.character(ca301_a_11_2010)),
    playgrd_diff = as.numeric(as.character(ca301_a_12_2014)) - as.numeric(as.character(ca301_a_12_2010)),
    letterbox_diff = as.numeric(as.character(ca301_a_14_2014)) - as.numeric(as.character(ca301_a_14_2010)),
    comm_website = as.numeric(as.character(ca301_a_15_2014)) - as.numeric(as.character(ca301_a_15_2010)),
    areasize_2010 = as.numeric(as.character(ca4r_2010)),
    areasize_2014 = as.numeric(as.character(ca4r_2014)),
    watersource_change = ifelse(as.numeric(ca5_2014) == as.numeric(ca5_2010), 1, 0),
    cooking_change = ifelse(as.numeric(ca6_2014) == as.numeric(ca6_2010), 1, 0),
    pop_change = as.numeric(as.character(cb2_2014)) - as.numeric(as.character(cb2_2010)),
    elec_access_change = 1 - ifelse(as.numeric(as.character(ca301_a_1_change))==2010 & as.numeric(as.character(ca301_a_1_change)) >0, 1, 0),
    cableradio_access_change = 1 - ifelse(as.numeric(as.character(ca301_a_2_change))==2010 & as.numeric(as.character(ca301_a_2_change)) >0, 1, 0),
    cabletv_access_change = 1 - ifelse(as.numeric(as.character(ca301_a_3_change))==2010 & as.numeric(as.character(ca301_a_3_change)) >0, 1, 0),
    postal_access_change = 1 - ifelse(as.numeric(as.character(ca301_a_4_change))==2010 & as.numeric(as.character(ca301_a_4_change)) >0, 1, 0),
    tele_access_change = 1 - ifelse(as.numeric(as.character(ca301_a_5_change))==2010 & as.numeric(as.character(ca301_a_5_change)) >0, 1, 0),
    cell_access_change = 1 - ifelse(as.numeric(as.character(ca301_a_6_change))==2010 & as.numeric(as.character(ca301_a_6_change)) >0, 1, 0),
    road_access_change = 1 - ifelse(as.numeric(as.character(ca301_a_7_change))==2010 & as.numeric(as.character(ca301_a_7_change)) >0, 1, 0),
    rail_access_change = 1 - ifelse(as.numeric(as.character(ca301_a_8_change))==2010 & as.numeric(as.character(ca301_a_8_change)) >0, 1, 0),
    tap_access_change = 1 - ifelse(as.numeric(as.character(ca301_a_9_change))==2010 & as.numeric(as.character(ca301_a_9_change)) >0, 1, 0),
    gas_access_change = 1 - ifelse(as.numeric(as.character(ca301_a_10_change))==2010 & as.numeric(as.character(ca301_a_10_change)) >0, 1, 0),
    localenterprise_access_change = 1 - ifelse(as.numeric(as.character(ca301_a_11_change))==2010 & as.numeric(as.character(ca301_a_11_change)) >0, 1, 0),
    commelect_access_change = 1 - ifelse(as.numeric(as.character(ca301_a_12_change))==2010 & as.numeric(as.character(ca301_a_12_change)) >0, 1, 0),
    transit_access_change = 1 - ifelse(as.numeric(as.character(ca301_a_13_change))==2010 & as.numeric(as.character(ca301_a_13_change)) >0, 1, 0),
    bus_access_change = 1 - ifelse(as.numeric(as.character(ca301_a_14_change))==2010 & as.numeric(as.character(ca301_a_14_change)) >0, 1, 0),
    subway_access_change = 1 - ifelse(as.numeric(as.character(ca301_a_15_change))==2010 & as.numeric(as.character(ca301_a_15_change)) >0, 1, 0),
    time_committe_nearesttown_change = as.numeric(as.character(cg101hr)) - as.numeric(as.character(cg101)),
    time_committe_countycap_change = as.numeric(as.character(cg201_2014)) - as.numeric(as.character(cg201_2010)),
    time_committe_provcap_change = as.numeric(as.character(cg301_2014)) - as.numeric(as.character(cg301_2010)),
    whether_mine_change = as.numeric(cg4_2014) - as.numeric(cg4_2010),
    agri_gdp_change = as.numeric(as.character(ch3_2014)) - as.numeric(as.character(ch3_2010)),
    nonagri_gdp_change = as.numeric(as.character(ch4_2014)) - as.numeric(as.character(ch4_2010)),
    income_percap_change = as.numeric(as.character(ch5_2014)) - as.numeric(as.character(ch5_2010))) %>%
  mutate(numconvenience_2014 = as.numeric(as.character(ca301_a_1_2014)),
         numconvenience_2010 = as.numeric(as.character(ca301_a_1_2014)),
         numprimarysch_2014 = as.numeric(as.character(ca301_a_3_2014)),
         numprimarysch_2010 = as.numeric(as.character(ca301_a_3_2010)),
         community_2014 = as.numeric(as.character(cid10)),
         community_2010 = as.numeric(as.character(cid_2010)),
         numhospital_2014 = as.numeric(as.character(ca301_a_4_2014)),
         numhospital_2010 = as.numeric(as.character(ca301_a_4_2010)),
         watersource_2014 = ca5_2014,
         watersource_2010 = ca5_2010,
         pop_2014 = as.numeric(as.character(cb2_2014)),
         pop_2010 = as.numeric(as.character(cb2_2010)),
         timecommittenearesttown_2014 = as.numeric(as.character(cg101hr)),
         timecommittenearesttown_2010 = as.numeric(as.character(cg101)),
         timecommittecountycap_2014 = as.numeric(as.character(cg201_2014)),
         timecommittecountycap_2010 = as.numeric(as.character(cg201_2010)),
         agrigdp_2014 = as.numeric(as.character(ch3_2014)),
         agrigdp_2010 = as.numeric(as.character(ch3_2010)),
         nonagrigdp_2014 = as.numeric(as.character(ch4_2014)),
         nonagrigdp_2010 = as.numeric(as.character(ch4_2010)),
         incpercap_2014 = as.numeric(as.character(ch5_2014)),
         incpercap_2010 = as.numeric(as.character(ch5_2010)))

joined <- joined[!is.na(joined$income_diff) & !is.na(joined$provcd_2014) & !is.na(joined$urban_2014) & !is.na(joined$cid10), ]

income_q75 <- quantile(joined$income_2010, 0.75)
income_q99 <- quantile(joined$income_2010, 0.99)

plots_df <- NULL

temp <- joined %>%
  select(community_2010, community_2014,
         age_2010, age_2014) %>%
  filter(!is.na(age_2010) & !is.na(age_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "age_2010", "age_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time))

plots_df[["dist"]][["age"]] <- 
  ggplot(data = temp, aes(x = age)) +
  geom_line(stat = "density", aes(color = time)) +
  labs(title = "Distribution of Age",
       x = "Age",
       y = "Density",
       color = "Year",
       caption = "Source: China Family Panel Studies")

plots_df[["inc"]][["age"]] <- joined %>%
  select(income_2010, income_2014,
         age_2010, age_2014) %>%
  filter(!is.na(age_2010) & !is.na(age_2014) & 
           !is.na(income_2010) & !is.na(income_2014)) %>%
  reshape(direction = "long",
          varying = c("income_2010", "income_2014",
                      "age_2010", "age_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time)) %>%
  group_by(age, time) %>%
  summarise(mean_inc = mean(income, na.rm = T)) %>%
  ggplot(aes(x = age, y=mean_inc, color = time)) +
  geom_point(alpha = 0.2) +
  geom_smooth(alpha=0.3, span=0.3) +
  labs(title = "Personal Income vs. Age",
       x = "Age",
       y = "Personal Income",
       color = "Year",
       caption = "Source: China Family Panel Studies")

plots_df[["dist"]][["age square"]] <- joined %>%
  select(community_2010, community_2014,
         age_2010, age_2014) %>%
  filter(!is.na(age_2010) & !is.na(age_2014)) %>%
  mutate(agesquare_2010 = age_2010 ^ 2,
         agesquare_2014 = age_2014^2) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "agesquare_2010", "agesquare_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time)) %>%
  ggplot(aes(x = agesquare)) +
  geom_line(stat = "density", aes(color = time)) +
  labs(title = "Distribution of Age-Square",
       x = "Age^2",
       y = "Density",
       color = "Year",
       caption = "Source: China Family Panel Studies")

plots_df[["inc"]][["age square"]] <- joined %>%
  select(income_2010, income_2014,
         age_2010, age_2014) %>%
  filter(!is.na(age_2010) & !is.na(age_2014) & 
           !is.na(income_2010) & !is.na(income_2014)) %>%
  mutate(agesquare_2010 = age_2010 ^ 2,
         agesquare_2014 = age_2014^2) %>%
  reshape(direction = "long",
          varying = c("income_2010", "income_2014",
                      "age_2010", "age_2014",
                      "agesquare_2010", "agesquare_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time)) %>%
  group_by(agesquare, time) %>%
  summarise(mean_inc = mean(income, na.rm = T)) %>%
  ggplot(aes(x = agesquare, y=mean_inc, color = time)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", alpha=0.3, span=0.3) +
  labs(title = "Personal Income vs. Age^2",
       x = "Age^2",
       y = "Personal Income",
       color = "Year",
       caption = "Source: China Family Panel Studies")

plots_df[["dist"]][["gender"]] <- joined %>%
  select(community_2010, community_2014,
         gender_2010, gender_2014) %>%
  filter(!is.na(gender_2010) & !is.na(gender_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "gender_2010", "gender_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time),
         gender = as.factor(gender)) %>%
  ggplot(aes(x = gender)) +
  geom_bar(position = "dodge", aes(fill = time),
           width = 0.5) +
  labs(title = "Distribution of Gender",
       x = "Gender",
       y = "Count",
       color = "Year",
       caption = "Source: China Family Panel Studies")

plots_df[["inc"]][["gender"]] <- joined %>%
  select(income_2010, income_2014,
         gender_2010, gender_2014) %>%
  filter(!is.na(gender_2010) & !is.na(gender_2014) & 
           !is.na(income_2010) & !is.na(income_2014)) %>%
  reshape(direction = "long",
          varying = c("income_2010", "income_2014",
                      "gender_2010", "gender_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time),
         gender = as.factor(gender)) %>%
  ggplot(aes(x = gender, y=income, color = time)) +
  geom_boxplot(position="dodge",
               alpha = 0.7) +
  scale_y_continuous(limits = c(0, income_q99)) +
  labs(title = "Personal Income vs. Gender",
       subtitle = "excluding top 1% income",
       x = "Gender",
       y = "Personal Income",
       color = "Year",
       caption = "Source: China Family Panel Studies")

plots_df[["dist"]][["education"]] <- joined %>%
  select(community_2010, community_2014,
         educ_2010, educ_2014) %>%
  filter(!is.na(educ_2010) & !is.na(educ_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "educ_2010", "educ_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time)) %>%
  ggplot(aes(x = educ)) +
  geom_line(stat = "density", aes(color = time)) +
  labs(title = "Distribution of Education",
       x = "Education Years",
       y = "Density",
       color = "Year",
       caption = "Source: China Family Panel Studies")

plots_df[["inc"]][["education"]] <- joined %>%
  select(income_2010, income_2014,
         educ_2010, educ_2014) %>%
  filter(!is.na(educ_2010) & !is.na(educ_2014) & 
           !is.na(income_2010) & !is.na(income_2014)) %>%
  reshape(direction = "long",
          varying = c("income_2010", "income_2014",
                      "educ_2010", "educ_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time)) %>%
  group_by(educ, time) %>%
  summarise(mean_inc = mean(income, na.rm = T)) %>%
  ggplot(aes(x = educ, y=mean_inc, color = time)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm",alpha=0.3, span=0.3) +
  labs(title = "Personal Income vs. Education",
       x = "Education",
       y = "Personal Income",
       color = "Year",
       caption = "Source: China Family Panel Studies")

# varls <- c("convenience_diff", "kindergarten_diff", "primsch_diff", "hospital_diff", "drugstore_diff", "temple_diff", "ancestral_diff", "church_diff", "actvty_diff", "nursing_diff", "physfacty_diff", "playgrd_diff", "letterbox_diff", "watersource_change", "cooking_change", "pop_change", "elec_access_change", "cableradio_access_change", "cabletv_access_change", "postal_access_change", "tele_access_change", "cell_access_change", "road_access_change", "rail_access_change", "tap_access_change", "gas_access_change", "localenterprise_access_change", "commelect_access_change", "transit_access_change", "bus_access_change", "subway_access_change", "time_committe_nearesttown_change", "time_committe_countycap_change", "time_committe_provcap_change", "whether_mine_change", "agri_gdp_change", "nonagri_gdp_change", "income_percap_change")
# 
# 
# for (var in varls) {
#   print(var)
#   print(sum(is.na(joined[[var]])) > 1/2*nrow(joined))
#   fit <- lm(income_diff ~ get(var), data = joined)
#   print(summary(fit))
# }
plots_df[["dist"]][["number of convenience stores"]] <- joined %>%
  select(community_2010, community_2014, 
         numconvenience_2010, numconvenience_2014) %>%
  filter(!is.na(numconvenience_2010) & !is.na(numconvenience_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "numconvenience_2010", "numconvenience_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time)) %>%
  ggplot(aes(x = numconvenience)) +
  geom_line(stat = "density", aes(linetype = time, color = time)) +
  labs(title = "Distribution of Number of Convenience Stores in Communities",
       x = "Number of Convenience Stores",
       y = "Density",
       color = "Year",
       caption = "Source: China Family Panel Studies")

plots_df[["inc"]][["number of convenience stores"]] <- joined %>%
  select(community_2010, community_2014, 
         income_2010, income_2014,
         numconvenience_2010, numconvenience_2014) %>%
  filter(!is.na(numconvenience_2010) & !is.na(numconvenience_2014) & 
           !is.na(income_2010) & !is.na(income_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "income_2010", "income_2014",
                      "numconvenience_2010", "numconvenience_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time)) %>%
  ggplot(aes(x = numconvenience, y=income, color = time)) +
  geom_quantile(method = "rqss",quantiles = c(0.25, 0.5, 0.75)) +
  labs(title = "Personal Income vs. Number of Convenience Stores in Communities",
       x = "Number of Convenience Stores",
       y = "Personal Income",
       color = "Year",
       caption = "Source: China Family Panel Studies")


########################################################

plots_df[["dist"]][["number of primary schools"]] <- joined  %>%
  select(community_2010, community_2014, 
         numprimarysch_2010, numprimarysch_2014) %>%
  filter(!is.na(numprimarysch_2010) & !is.na(numprimarysch_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "numprimarysch_2010", "numprimarysch_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time)) %>%
  ggplot(aes(x = numprimarysch)) +
  geom_bar(position="dodge",aes(fill = time)) +
  labs(title = "Distribution of Number of Primary Schools in Communities",
       x = "Number of Primary Schools",
       y = "Count",
       color = "Year",
       caption = "Source: China Family Panel Studies")

plots_df[["inc"]][["number of primary schools"]] <- joined %>%
  select(community_2010, community_2014, 
         income_2010, income_2014,
         numprimarysch_2010, numprimarysch_2014) %>%
  filter(!is.na(numprimarysch_2010) & !is.na(numprimarysch_2014) & 
           !is.na(income_2010) & !is.na(income_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "income_2010", "income_2014",
                      "numprimarysch_2010", "numprimarysch_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time),
         numprimarysch = as.factor(numprimarysch)) %>%
  ggplot(aes(x = numprimarysch, y=income, color = time)) +
  geom_boxplot(position="dodge",
               varwidth = T,
               alpha = 0.5) +
  scale_y_continuous(limits = c(0, income_q99)) +
  labs(title = "Personal Income vs. Number of Primary Schools in Communities",
       subtitle = "excluding top 1% income, box width representing group size",
       x = "Number of Primary Schools",
       y = "Personal Income",
       color = "Year",
       caption = "Source: China Family Panel Studies")

########################################################

plots_df[["dist"]][["water source"]] <- joined  %>%
  select(community_2010, community_2014, 
         watersource_2010, watersource_2014) %>%
  filter(!is.na(watersource_2010) & !is.na(watersource_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "watersource_2010", "watersource_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time)) %>%
  ggplot(aes(x = watersource)) +
  geom_bar(position="dodge",aes(fill = time)) +
  labs(title = "Types of Water Sources in Communities",
       x = "Type of Water Sources",
       y = "Count",
       color = "Year",
       caption = "Source: China Family Panel Studies")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plots_df[["inc"]][["water source"]] <- joined %>%
  select(community_2010, community_2014, 
         income_2010, income_2014,
         watersource_2010, watersource_2014) %>%
  filter(!is.na(watersource_2010) & !is.na(watersource_2014) & 
           !is.na(income_2010) & !is.na(income_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "income_2010", "income_2014",
                      "watersource_2010", "watersource_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time)) %>%
  ggplot(aes(x = watersource, y=income, color = time)) +
  geom_boxplot(position="dodge",
               varwidth = T,
               alpha = 0.3) +
  scale_y_continuous(limits = c(0, income_q99)) +
  labs(title = "Personal Income vs. Types of Water Sources in Communities",
       subtitle = "excluding top 1% income, box width representing group size",
       x = "Type of Water Sources",
       y = "Personal Income",
       color = "Year",
       caption = "Source: China Family Panel Studies")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

###########################################################

plots_df[["dist"]][["number of hospitals"]] <- joined %>%
  select(community_2010, community_2014,
         numhospital_2010, numhospital_2014) %>%
  filter(!is.na(numhospital_2010) & !is.na(numhospital_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "numhospital_2010", "numhospital_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time)) %>%
  ggplot(aes(x = numhospital)) +
  geom_line(stat = "density", aes(color = time), alpha = 0.7) +
  labs(title = "Distribution of Number of Hospitals in Communities",
       x = "Number of Hospitals",
       y = "Count",
       color = "Year",
       caption = "Source: China Family Panel Studies")

plots_df[["inc"]][["number of hospitals"]] <- joined %>%
  select(community_2010, community_2014, 
         income_2010, income_2014,
         numhospital_2010, numhospital_2014) %>%
  filter(!is.na(numhospital_2010) & !is.na(numhospital_2014) & 
           !is.na(income_2010) & !is.na(income_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "income_2010", "income_2014",
                      "numhospital_2010", "numhospital_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time),
         numhospital = as.factor(numhospital)) %>%
  ggplot(aes(x = numhospital, y=income, color = time)) +
  geom_boxplot(position="dodge", 
               alpha = 0.7) +
  scale_y_continuous(limits = c(0, income_q99)) +
  labs(title = "Personal Income vs. Number of Hospitals in Communities",
       subtitle = "excluding top 1% income",
       x = "Number of Hospitals",
       y = "Personal Income",
       color = "Year",
       caption = "Source: China Family Panel Studies")

###########################################################

plots_df[["dist"]][["community population"]] <- joined %>%
  select(community_2010, community_2014,
         pop_2010, pop_2014) %>%
  filter(!is.na(pop_2010) & !is.na(pop_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "pop_2010", "pop_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time)) %>%
  ggplot(aes(x = pop)) +
  geom_line(stat = "density", aes(color = time)) +
  labs(title = "Distribution of Population in Communities",
       x = "Population",
       y = "Density",
       color = "Year",
       caption = "Source: China Family Panel Studies")

plots_df[["inc"]][["community population"]] <- joined %>%
  select(community_2010, community_2014, 
         income_2010, income_2014,
         pop_2010, pop_2014) %>%
  filter(!is.na(pop_2010) & !is.na(pop_2014) & 
           !is.na(income_2010) & !is.na(income_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "income_2010", "income_2014",
                      "pop_2010", "pop_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time)) %>%
  group_by(community, time) %>%
  summarise(mean_inc = mean(income, na.rm = T),
            mean_pop = mean(pop, na.rm=T)) %>%
  ggplot(aes(x = mean_pop, y=mean_inc, color = time)) +
  geom_quantile(method = "rqss",) + geom_point(alpha = 0.1) +
  labs(title = "Personal Income vs. Population in Communities",
       x = "Population",
       y = "Personal Income",
       color = "Year",
       caption = "Source: China Family Panel Studies")

###########################################################

plots_df[["dist"]][["travel time to nearest town"]] <- joined %>%
  select(community_2010, community_2014,
         timecommittenearesttown_2010, timecommittenearesttown_2014) %>%
  filter(!is.na(timecommittenearesttown_2010) & !is.na(timecommittenearesttown_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "timecommittenearesttown_2010", "timecommittenearesttown_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time)) %>%
  ggplot(aes(x = timecommittenearesttown)) +
  geom_line(stat = "density", aes(color = time)) +
  labs(title = "Travel Time from County Committe to Nearest Town in Communities",
       x = "Travel Time (hours)",
       y = "Density",
       color = "Year",
       caption = "Source: China Family Panel Studies")

plots_df[["inc"]][["travel time to nearest town"]] <- joined %>%
  select(community_2010, community_2014, 
         income_2010, income_2014,
         timecommittenearesttown_2010, timecommittenearesttown_2014) %>%
  filter(!is.na(timecommittenearesttown_2010) & !is.na(timecommittenearesttown_2014) & 
           !is.na(income_2010) & !is.na(income_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "income_2010", "income_2014",
                      "timecommittenearesttown_2010", "timecommittenearesttown_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time)) %>%
  group_by(community, time) %>%
  summarise(mean_inc = mean(income, na.rm = T),
            mean_time = mean(timecommittenearesttown, na.rm=T)) %>%
  ggplot(aes(x = mean_time, y=mean_inc, color = time)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", alpha=0.3, span=0.3) +
  labs(title = "Personal Income vs. Travel Time to Nearest Town in Communities",
       x = "Travel Time from County Committe to Nearest Town",
       y = "Personal Income",
       color = "Year",
       caption = "Source: China Family Panel Studies")

###########################################################

plots_df[["dist"]][["travel time to county seat"]] <- joined %>%
  select(community_2010, community_2014,
         timecommittecountycap_2010, timecommittecountycap_2014) %>%
  filter(!is.na(timecommittecountycap_2010) & !is.na(timecommittecountycap_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "timecommittecountycap_2010", "timecommittecountycap_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time)) %>%
  ggplot(aes(x = timecommittecountycap)) +
  geom_line(stat = "density", aes(color = time)) +
  labs(title = "Travel Time to County Seat in Communities",
       x = "Travel Time (hours)",
       y = "Density",
       color = "Year",
       caption = "Source: China Family Panel Studies")

plots_df[["inc"]][["travel time to county seat"]] <- joined %>%
  select(community_2010, community_2014, 
         income_2010, income_2014,
         timecommittecountycap_2010, timecommittecountycap_2014) %>%
  filter(!is.na(timecommittecountycap_2010) & !is.na(timecommittecountycap_2014) & 
           !is.na(income_2010) & !is.na(income_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "income_2010", "income_2014",
                      "timecommittecountycap_2010", "timecommittecountycap_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time),
         timecommittecountycap = as.numeric(timecommittecountycap)) %>%
  group_by(community, time) %>%
  summarise(mean_inc = mean(income, na.rm = T),
            mean_time = mean(timecommittecountycap, na.rm=T)) %>%
  ggplot(aes(x = mean_time, y=mean_inc, color = time)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", alpha=0.3, span=0.3) +
  labs(title = "Personal Income vs. Travel Time to County Seat in Communities",
       x = "Travel Time to County Seat",
       y = "Personal Income",
       color = "Year",
       caption = "Source: China Family Panel Studies")

###########################################################

plots_df[["dist"]][["agricultural GDP"]] <- joined %>%
  select(community_2010, community_2014,
         agrigdp_2010, agrigdp_2014) %>%
  filter(!is.na(agrigdp_2010) & !is.na(agrigdp_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "agrigdp_2010", "agrigdp_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time)) %>%
  ggplot(aes(x = agrigdp)) +
  geom_line(stat = "density", aes(color = time)) +
  labs(title = "Distribution of Agricultural GDP in Communities",
       x = "Agricultural GDP",
       y = "Density",
       color = "Year",
       caption = "Source: China Family Panel Studies")

plots_df[["inc"]][["agricultural GDP"]] <- joined %>%
  select(community_2010, community_2014, 
         income_2010, income_2014,
         agrigdp_2010, agrigdp_2014) %>%
  filter(!is.na(agrigdp_2010) & !is.na(agrigdp_2014) & 
           !is.na(income_2010) & !is.na(income_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "income_2010", "income_2014",
                      "agrigdp_2010", "agrigdp_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time),
         agrigdp = as.numeric(agrigdp)) %>%
  group_by(community, time) %>%
  summarise(mean_inc = mean(income, na.rm = T),
            mean_agrgdp = mean(agrigdp, na.rm=T)) %>%
  ggplot(aes(x = mean_agrgdp, y=mean_inc, color = time)) +
  geom_point(alpha = 0.1) +
  geom_smooth(alpha=0.3, span=0.3) +
  labs(title = "Personal Income vs. Agricultural GDP in Communities",
       x = "Agricultural GDP",
       y = "Personal Income",
       color = "Year",
       caption = "Source: China Family Panel Studies")

###########################################################

plots_df[["dist"]][["non-agricultural GDP"]] <- joined %>%
  select(community_2010, community_2014,
         nonagrigdp_2010, nonagrigdp_2014) %>%
  filter(!is.na(nonagrigdp_2010) & !is.na(nonagrigdp_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "nonagrigdp_2010", "nonagrigdp_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time)) %>%
  ggplot(aes(x = nonagrigdp)) +
  geom_line(stat = "density", aes(color = time)) +
  labs(title = "Distribution of Non-Agricultural GDP in Chinese Communities",
       x = "Non-Agricultural GDP",
       y = "Density",
       color = "Year",
       caption = "Source: China Family Panel Studies")

plots_df[["inc"]][["non-agricultural GDP"]] <- joined %>%
  select(community_2010, community_2014, 
         income_2010, income_2014,
         nonagrigdp_2010, nonagrigdp_2014) %>%
  filter(!is.na(nonagrigdp_2010) & !is.na(nonagrigdp_2014) & 
           !is.na(income_2010) & !is.na(income_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "income_2010", "income_2014",
                      "nonagrigdp_2010", "nonagrigdp_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time),
         nonagrigdp = as.numeric(nonagrigdp)) %>%
  group_by(community, time) %>%
  summarise(mean_inc = mean(income, na.rm = T),
            mean_agrgdp = mean(nonagrigdp, na.rm=T)) %>%
  ggplot(aes(x = mean_agrgdp, y=mean_inc, color = time)) +
  geom_quantile(method = "rqss",) +
  labs(title = "Personal Income vs. Non-Agricultural GDP in Communities",
       x = "Non-Agricultural GDP",
       y = "Personal Income",
       color = "Year",
       caption = "Source: China Family Panel Studies")

###########################################################

plots_df[["dist"]][["income per capita"]] <- joined %>%
  select(community_2010, community_2014,
         incpercap_2010, incpercap_2014) %>%
  filter(!is.na(incpercap_2010) & !is.na(incpercap_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "incpercap_2010", "incpercap_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time)) %>%
  ggplot(aes(x = incpercap)) +
  geom_line(stat = "density", aes(color = time)) +
  labs(title = "Distribution of Income per capita in Communities",
       x = "Income per capita",
       y = "Density",
       color = "Year",
       caption = "Source: China Family Panel Studies")

plots_df[["inc"]][["income per capita"]] <- joined %>%
  select(community_2010, community_2014, 
         income_2010, income_2014,
         incpercap_2010, incpercap_2014) %>%
  filter(!is.na(incpercap_2010) & !is.na(incpercap_2014) & 
           !is.na(income_2010) & !is.na(income_2014)) %>%
  reshape(direction = "long",
          varying = c("community_2010", "community_2014",
                      "income_2010", "income_2014",
                      "incpercap_2010", "incpercap_2014"),
          sep = "_") %>%
  mutate(time = as.factor(time),
         incpercap = as.numeric(incpercap)) %>%
  group_by(community, time) %>%
  summarise(mean_inc = mean(income, na.rm = T),
            mean_agrgdp = mean(incpercap, na.rm=T)) %>%
  ggplot(aes(x = mean_agrgdp, y=mean_inc, color = time)) +
  geom_point(alpha = 0.1) +
  geom_smooth(alpha=0.3, span=0.3) +
  labs(title = "Personal Income vs. Income per capita in Communities",
       x = "Income per capita",
       y = "Personal Income",
       color = "Year",
       caption = "Source: China Family Panel Studies")

save(plots_df,
     file = "./plots.RData")