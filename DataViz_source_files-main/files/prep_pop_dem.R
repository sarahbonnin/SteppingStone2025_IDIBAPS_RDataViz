# read in files
library(readr)

age_str_female <- readr::read_csv("population-and-demography_age_structure_female.csv")
# Long format
age_str_f_long <- age_str_female %>%
  tidyr::pivot_longer(cols = c(-`Country name`, -`Year`), names_to = "Age_group", values_to="Population") %>%
  mutate(Sex="Female") %>%
  rename(Country=`Country name`)

age_str_male <- readr::read_csv("population-and-demography_age_structure_male.csv")
# Long format
age_str_m_long <- age_str_male %>%
  tidyr::pivot_longer(cols = c(-`Country name`, -`Year`), names_to = "Age_group", values_to="Population") %>%
  mutate(Sex="Male") %>%
  rename(Country=`Country name`)

# Concatenate Male and Female
age_str_long <- dplyr::bind_rows(age_str_f_long, age_str_m_long)

# Order age groups
age_str_long <- age_str_long %>% 
  mutate(Age_group=fct_relevel(Age_group, c("Under-5s", "5-14 years", "15-24 years", "25-64 years", "65+ years")))

# Write to file
readr::write_csv(age_str_long, "population_age_group_per_sex_long_format.csv")

# Try some plots
ggplot(age_str_long, aes(x=Year, y=Population)) + geom_bar(stat="identity")
ggplot(age_str_long, aes(x=Year, y=Population, fill=Sex)) + geom_bar(stat="identity")
ggplot(age_str_long, aes(x=Year, y=Population, fill=Sex)) + geom_bar(stat="identity", position="dodge")

# Select one country
ggplot(age_str_long %>% filter(Country=="Spain"), aes(x=Year, y=Population, fill=Sex)) + 
  geom_bar(stat="identity", position="dodge")

ggplot(age_str_long %>% filter(Country=="Spain"), aes(x=Year, y=Population, fill=Age_group)) + 
  geom_bar(stat="identity", position="dodge")
ggplot(age_str_long %>% filter(Country=="Spain"), aes(x=Year, y=Population, fill=Age_group)) + 
  geom_bar(stat="identity", position="stack")
ggplot(age_str_long %>% filter(Country=="Spain"), aes(x=Year, y=Population, fill=Age_group)) + 
  geom_bar(stat="identity", position="fill")

ggplot(age_str_long %>% filter(Country=="Spain"), aes(x=Year, y=Population, fill=Age_group)) + 
  geom_bar(stat="identity", position="fill") +
  theme_bw()

ggplot(age_str_long %>% filter(Country=="Spain"), aes(x=Year, y=Population, fill=Age_group)) + 
  geom_bar(stat="identity", position="fill") +
  theme_bw() +
  scale_fill_brewer()

ggplot(age_str_long %>% filter(Country=="Spain"), aes(x=Year, y=Population, fill=Age_group)) + 
  geom_bar(stat="identity", position="fill") +
  theme_bw(base_size = 15) +
  scale_fill_brewer()

ggplot(age_str_long %>% filter(Country=="Spain"), aes(x=Year, y=Population, fill=Age_group)) + 
  geom_bar(stat="identity", position="fill") +
  theme_bw(base_size = 15) +
  scale_fill_brewer(palette="Set1")

ggplot(age_str_long %>% filter(Country=="Spain"), aes(x=Year, y=Population, fill=Age_group)) + 
  geom_bar(stat="identity", position="fill") +
  theme_bw(base_size = 15) +
  scale_fill_brewer(palette="Set1") +
  ggtitle("Per-age group Spanish population from 1950 to 2020")

ggplot(age_str_long %>% filter(Country=="India"), aes(x=Year, y=Population, fill=Age_group)) + 
  geom_bar(stat="identity", position="fill") +
  theme_bw(base_size = 15) +
  scale_fill_brewer(palette="Set1") +
  ggtitle("Per-age group Indian population from 1950 to 2020")

