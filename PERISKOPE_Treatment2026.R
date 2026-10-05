#----ADD PACKAGES
install.packages("pacman")
library(pacman)

p_load(rio, 
       tidyverse,
       ggplot2,
       dplyr,
       broom,
       skimr,
       janitor,
       gtsummary,
       Hmisc,
       rms,
       ggpubr,
       forcats,
       gt,
       webshot2,
       flextable,
       countrycode,
       officer)


getwd()


#----IMPORT DATA

raw_periskope_data <- import("periskope_data_demo_18dec2025.csv")
skim(raw_periskope_data)

#---CLEAN DATA

head(raw_periskope_data)                           

periskope_data <- raw_periskope_data %>% 
  rename(risk           = 'risk score 2y',
         birth_country  = 'country of birth.',
         referral      =  'refferal') %>% 
  mutate(
    region_of_birth = countrycode(
      sourcevar   = birth_country,
      origin      = "country.name",
      destination = "un.region.name")) %>% 
  
  mutate(treatment = tolower(treatment)) %>%  # Convert "treatment" to lowercase
  mutate(treatment = if_else(treatment == "yes", 1, 0)) %>%  # Recode "yes" to 1 and others to 0
  mutate(risk = as.numeric(risk), 
         treatment = as.numeric(treatment),
         quantiferon = as.numeric(quantiferon)) %>% # Convert both columns to numeric
  mutate# Convert both columns to numeric

colnames(raw_periskope_data)

table(periskope_data$region_of_birth)
summary(periskope_data$risk)
skim(periskope_data)

#---REGRESSION

peri_lr <- glm(treatment ~ risk, data = periskope_data, family = binomial)
summary(peri_lr)

tidy_results <- tidy(peri_lr, conf.int = TRUE, exponentiate = TRUE)
tidy_results

periskope_data$predicted_prob <- predict(peri_lr, type = "response")

table(periskope_data$region)

#---PLOTS---#
#############

#LR PLOT

ggplot(periskope_data, aes(x = risk, y = treatment)) +
  geom_point(aes(color = treatment), linewidth = 2, alpha = 0.7, shape = 16) +  # Adjusted transparency and shape
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, 
              color = "darkblue", size = 1, fill = "lightblue", alpha = 0.3) +  # Thicker curve with a subtle CI
  scale_color_gradient(low = "lightblue", high = "darkblue") +  # Custom color gradient
  labs(title = "Predicted Probability of Treatment by Risk Score",
       x = "Periskope Risk Score",
       y = "Predicted Probability of Treatment")+
  theme_minimal() 

tidy

#DENSITY PLOT

periskope_data_density <- periskope_data %>% 
  mutate(treatment = as.factor(treatment)) %>% 
  mutate(treatment = if_else(treatment == "1", "yes", "no")) %>% 
  mutate(treatment = )
  mutate(risk = as.numeric(risk)) 


density_plot <- ggplot(periskope_data_density, aes(x = risk, fill = treatment, color = treatment)) +
    geom_density(alpha = 0.4, linewidth = 0.9, adjust = 2) +
    labs(
      x = "PERISKOPE-TB Risk (% over 2 years)",
      y = "Density",
      fill = "Treatment\nAcceptance",
      color = "Treatment\nAcceptance"
    ) +
    theme_minimal() +
    scale_fill_manual(
      values = c("yes" = "#56B4E9", "no" = "#E69F00"),
      labels = c("no" = "No", "yes" = "Yes")
    ) +
    scale_color_manual(
      values = c("yes" = "#56B4E9", "no" = "#E69F00"),
      labels = c("no" = "No", "yes" = "Yes")
    ) +
    theme(legend.position = "right")


ggsave("density_plot_22aug2025.png",
       density_plot,
       width = 6, 
       height = 4, 
       dpi = 300,
       bg  = "white")


ggsave(
  filename = "density_plot.tiff",
  plot = density_plot,
  device = "tiff",
  width = 180,
  height = 120,
  units = "mm",
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

#BOX PLOT

ggplot(periskope_data_density, aes(x = treatment, y = risk, fill = treatment)) +  # Use treatment as a factor
  geom_boxplot(alpha = 0.9) + 
  geom_jitter(width = 0.2, alpha = 0.2, color = "black") +  # Adds jittered points for individual data points
  labs(
    x = "Treatment Acceptance", 
    y = "PERISKOPE Risk Score",
    title = "Boxplot Comparing PERISKOPE-TB Risk Score by Treatment Acceptance Outcome"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("yes" = "#56B4E9", "no" = "#E69F00")) +  # Colors for each treatment outcome
  theme(legend.position = "none")

ggsave("box_plot.png", plot = box_plot, width = 6, height = 4, dpi = 300)




################### NON LINEAR ANALYSIS ###########################
###################################################################




#001 analysis

library(tidyverse)
library(rms)
library(ggpubr)


# Set datadist for RMS package

dd <- datadist(periskope_data); options(datadist = "dd")

# Linear model---

fit_lin <- lrm(treatment ~ risk, data = periskope_data, x = TRUE, y = TRUE)

plot_lin <- ggplot(Predict(fit_lin, fun = plogis)) +   theme_pubr() +  xlab("Predicted risk") +  ylab("Probability of accepting preventive treatment")

plot_lin

# Non-linear model using restricted cubic splines---

fit_rcs <- lrm(treatment ~ rcs(risk, 3), data = periskope_data, x = TRUE, y = TRUE)

plot_rcs <- ggplot(Predict(fit_rcs, fun = plogis)) +   theme_pubr() +  xlab("Predicted risk") +  ylab("Probability of accepting preventive treatment")

plot_rcs






#Convert Predict() output to a clean data frame
pred_rcs <- as.data.frame(Predict(fit_rcs, fun = plogis))

#plot
non_linear_plot <- ggplot(pred_rcs, aes(x = risk, y = yhat)) +
  geom_line(color = "#56B4E9", linewidth = 0.8) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill = "#56B4E9", alpha = 0.2, color = NA) +
  labs(x = "PERISKOPE-TB Risk (% over 2 years)", 
       y = "Probability of accepting preventive treatment") +
  theme_minimal() +
  theme(legend.position = "none")


#export

ggsave(
  filename = "non_linear_plot.tiff",
  plot = non_linear_plot,
  device = "tiff",
  width = 180,
  height = 120,
  units = "mm",
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

table(periskope_data$region_of_birth)

####################### DEMOGRAPHICS. #########################################
###############################################################################


demo_table <- periskope_data %>%
  rename(Age = "age",
         'Country of Birth' = "birth_country",
         Referral      = "referral",
         Quantiferon  = "quantiferon",
         'Region of Birth' = region_of_birth) %>% 
  mutate(
    sex = recode(sex,
                 "m" = "Male",
                 "f" = "Female"),
    treatment = factor(if_else(treatment == 1, "Yes", "No"), levels = c("Yes", "No")),
    Referral = recode(Referral,
                      "contact" = "Contact",
                      "migrant" = "Migrant",
                      "occ"     = "Occupational Health",
                      "refugee" = "Refugee")) %>% 
  rename('Sex at Birth' = sex) %>% 
  mutate(Referral = fct_infreq(Referral),
         `Region of Birth` = fct_infreq(factor(`Region of Birth`))) %>% 
  
  select(Age, 'Sex at Birth', 'Region of Birth', Referral, Quantiferon, treatment) %>%
  tbl_summary(
    by = treatment,
    type = list(
      Quantiferon ~ "continuous"   # keep numeric; summarize as continuous
    ),
    statistic = list(
      Age ~ "{median} ({p25}, {p75})",
      Quantiferon ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"),
    percent = "row",
    digits = list(
      Age ~ 0,
      Quantiferon ~ 1
    ),
    missing = "ifany",
    missing_text = "Not Available"
  ) %>%
  bold_labels()

# Convert to a Word-native table
ft <- demo_table %>% as_flex_table() %>% autofit()

# Write to a Word document
doc <- read_docx() %>%
  body_add_par("Demographics by Treatment Outcome", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "Demographics_by_Treatment_Outcome.docx")

#export


# Save the table as an image
gtsave(demo_table, "demographics_19jan2026.png")



country_plot <- periskope_data %>% 
  mutate(
    birth_country = fct_infreq(birth_country)
  ) %>% 
  ggplot(aes(x = birth_country)) +
  geom_bar(fill = "steelblue") +
  coord_flip() +
  labs(
    x = "Country of Birth",
    y = "Number of Participants",
    title = "Distribution of Country of Birth"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7)  # Adjust the size here (e.g., 8)
  )


#export

ggsave("country_plot_19jan2026.png",
       country_plot,
       width = 6, 
       height = 5, 
       dpi = 300,
       bg  = "white")


























