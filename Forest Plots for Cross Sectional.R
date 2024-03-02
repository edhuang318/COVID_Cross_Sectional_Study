library(tidyverse)
library(forestploter)


#start with overall cases, age group
#create the data frame
overall_case_age <- data.frame(
  Age = c("0-17", "18-49", "50-64", "65+"),
  Cases = c(1968275, 6088447	, 1979092	, 1209469),
  Controls = c(7024157, 11392219, 5333284	, 4460410),
  ` ` = paste(rep(" ", 20), collapse = " "),
  smd = c(1, 1.9072, 1.3243, 0.9677),
  CI_Lower = c(1, 1.9037, 1.3213, 0.9652),
  CI_Upper = c(1, 1.9108, 1.3273, 0.9701),
  `OR(95% CI)` = c("1 (Reference)","1.91 (1.90,1.91)","1.32 (1.32,1.33)","0.968 (0.965,0.970)"),
  check.names = FALSE
)
#create plot


# Final data manipulation part 
overall_case_age$CI_Upper <- as.numeric(overall_case_age$CI_Upper)
overall_case_age$CI_Lower <- as.numeric(overall_case_age$CI_Lower)
overall_case_age$smd <- as.numeric(overall_case_age$smd)


tm <- forest_theme(base_size = 10,
                   # Graphical parameters of confidence intervals
                   ci_pch = 15,
                   ci_col = "#0e8abb",
                   ci_fill = "red",
                   ci_alpha = 1,
                   ci_lty = 1,
                   ci_lwd = 2,
                   ci_Theight = 0.2,
                   # Graphical parameters of reference line
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Graphical parameters of vertical line
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20")

# Forest plot
overall_case_age_plot <- forest(overall_case_age[,c(1:4, 8)],
             est = overall_case_age$smd,
             lower = overall_case_age$CI_Lower, 
             upper = overall_case_age$CI_Upper,
             sizes = 0.8,
             ci_column = 4,
             ref_line = 1,
             arrow_lab = c("Favors Reference Group", "Favors Other Group"),
             xlim = c(0, 3),
             ticks_at = c(0, 0.5, 1, 2, 3),
             xlab = "Odds Ratio",
             theme = tm)

plot(overall_case_age_plot)

#overall cases, race group
#create the data frame
overall_case_race <- data.frame(
  Race = c("White", "Black", "Asian", "American Indian", "Hawaiian/Pacific Islander", "Other", "Multi-Race", "Hispanic/Latino"),
  Cases = c(2203601, 469635, 894314, 39778, 65623, 1006880, 85459,3873732),
  Controls = c(11905696, 1763623,4993082,320829,82655, 5029985, 4149758,11720055),
  ` ` = paste(rep(" ", 20), collapse = " "),
  smd = c(1, 1.4387,0.9677,0.6699, 4.2895, 1.0815, 0.1113,1.7858),
  CI_Lower = c(1, 1.4337, 0.9651, 0.6629, 4.2454, 1.0787,0.1105, 1.7825),
  CI_Upper = c(1, 1.4438, 0.9703,0.6770, 4.3341, 1.0843,0.1120, 1.7890),
  `OR(95% CI)` = c("1 (Reference)", "1.44 (1.43, 1.44)", "0.968 (0.965, 0.970)", "0.670 (0.663, 0.677)", "4.29 (4.25, 4.33)", "1.08 (1.08, 1.08)", "0.111 (0.111, 0.112)", "1.79 (1.78, 1.79)"),
  check.names = FALSE
)
#create plot


# Final data manipulation part 
overall_case_race$CI_Upper <- as.numeric(overall_case_race$CI_Upper)
overall_case_race$CI_Lower <- as.numeric(overall_case_race$CI_Lower)
overall_case_race$smd <- as.numeric(overall_case_race$smd)


tm <- forest_theme(base_size = 10,
                   # Graphical parameters of confidence intervals
                   ci_pch = 15,
                   ci_col = "#0e8abb",
                   ci_fill = "red",
                   ci_alpha = 1,
                   ci_lty = 1,
                   ci_lwd = 2,
                   ci_Theight = 0.2,
                   # Graphical parameters of reference line
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Graphical parameters of vertical line
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20")

# Forest plot
overall_case_race_plot <- forest(overall_case_race[,c(1:4, 8)],
                                est = overall_case_race$smd,
                                lower = overall_case_race$CI_Lower, 
                                upper = overall_case_race$CI_Upper,
                                sizes = 0.8,
                                ci_column = 4,
                                ref_line = 1,
                                arrow_lab = c("Favors Reference Group", "Favors Other Group"),
                                xlim = c(0, 5),
                                ticks_at = c(0, 0.5, 1, 2, 3, 4, 5),
                                xlab = "Odds Ratio",
                                theme = tm)
plot(overall_case_race_plot)


#do overall cases, sex group
#create the data frame
overall_case_sex <- data.frame(
  Age = c("Female", "Male"),
  Cases = c(5555978,4989905),
  Controls = c(14185331,14724139),
  ` ` = paste(rep(" ", 20), collapse = " "),
  smd = c(1, 0.8652),
  CI_Lower = c(1,0.8640),
  CI_Upper = c(1,0.8665),
  `OR(95% CI)` = c("1 (Reference)", "0.865 (0.864, 0.867)"),
  check.names = FALSE
)
#create plot


# Final data manipulation part 
overall_case_sex$CI_Upper <- as.numeric(overall_case_sex$CI_Upper)
overall_case_sex$CI_Lower <- as.numeric(overall_case_sex$CI_Lower)
overall_case_sex$smd <- as.numeric(overall_case_sex$smd)


tm <- forest_theme(base_size = 10,
                   # Graphical parameters of confidence intervals
                   ci_pch = 15,
                   ci_col = "#0e8abb",
                   ci_fill = "red",
                   ci_alpha = 1,
                   ci_lty = 1,
                   ci_lwd = 2,
                   ci_Theight = 0.2,
                   # Graphical parameters of reference line
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Graphical parameters of vertical line
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20")

# Forest plot
overall_case_sex_plot <- forest(overall_case_sex[,c(1:4, 8)],
                                est = overall_case_sex$smd,
                                lower = overall_case_sex$CI_Lower, 
                                upper = overall_case_sex$CI_Upper,
                                sizes = 0.8,
                                ci_column = 4,
                                ref_line = 1,
                                arrow_lab = c("Favors Reference Group", "Favors Other Group"),
                                xlim = c(0, 2),
                                ticks_at = c(0, 0.5, 1, 2),
                                xlab = "Odds Ratio",
                                theme = tm)

plot(overall_case_sex_plot)


#overall deaths, by age
#create dataframe
overall_deaths_age <- data.frame(
  Age = c("0-17", "18-49", "50-64", "65+"),
  Deaths = c(89,7749,19826,69324),
  Controls = c(8992343,17472917,7292550,5600555),
  ` ` = paste(rep(" ", 20), collapse = " "),
  smd = c(1,44.8088,274.6873,1250.6489),
  CI_Lower = c(1,36.3596,223.0530,1015.8958),
  CI_Upper = c(1,55.2214,338.2743,1539.6487),
  `OR(95% CI)` = c("1(Reference)", "44.8 (36.4, 55.2)","275 (223, 338)","1250 (1020, 1540)"),
  check.names = FALSE
)
#create plot


# Final data manipulation part 
overall_deaths_age$CI_Upper <- as.numeric(overall_deaths_age$CI_Upper)
overall_deaths_age$CI_Lower <- as.numeric(overall_deaths_age$CI_Lower)
overall_deaths_age$smd <- as.numeric(overall_deaths_age$smd)


tm <- forest_theme(base_size = 10,
                   # Graphical parameters of confidence intervals
                   ci_pch = 15,
                   ci_col = "#0e8abb",
                   ci_fill = "red",
                   ci_alpha = 1,
                   ci_lty = 1,
                   ci_lwd = 2,
                   ci_Theight = 0.2,
                   # Graphical parameters of reference line
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Graphical parameters of vertical line
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20")

# Forest plot
overall_deaths_age_plot <- forest(overall_deaths_age[,c(1:4, 8)],
                                est = overall_deaths_age$smd,
                                lower = overall_deaths_age$CI_Lower, 
                                upper = overall_deaths_age$CI_Upper,
                                sizes = 0.8,
                                ci_column = 4,
                                ref_line = 1,
                                arrow_lab = c("Favors Reference Group", "Favors Other Group"),
                                xlim = c(0, 1500),
                                ticks_at = c(0, 500, 1000),
                                xlab = "Odds Ratio",
                                theme = tm)

plot(overall_deaths_age_plot)

#overall deaths, race group
#create the data frame
overall_deaths_race <- data.frame(
  Race = c("White", "Black", "Asian", "American Indian", "Hawaiian/Pacific Islander", "Other", "Multi-Race", "Hispanic/Latino"),
  Deaths = c(34483,6808,10675,468,595,893,1465,41064),
  Controls = c(14074814,2226450,5876721,360139,147683,6035972,4233752,15552723),
  ` ` = paste(rep(" ", 20), collapse = " "),
  smd = c(1,1.2481,0.7414,0.5304,1.6445,0.0604,0.1412,1.0777),
  CI_Lower = c(1,1.2160,0.7255,0.4841,1.5162,0.0565,0.1340,1.0623),
  CI_Upper = c(1,1.2810,0.7577,0.5811,1.7836,0.0645,0.1488,1.0932),
  `OR(95% CI)` = c("1 (Reference)","1.25 (1.22, 1.28)","0.741 (0.726, 0.758)","0.530 (0.484, 0.581)","1.64 (1.52, 1.78)","0.0604 (0.0565, 0.0645)","0.141 (0.134, 0.149)","1.08 (1.06, 1.09)"),
  check.names = FALSE
)
#create plot


# Final data manipulation part 
overall_deaths_race$CI_Upper <- as.numeric(overall_deaths_race$CI_Upper)
overall_deaths_race$CI_Lower <- as.numeric(overall_deaths_race$CI_Lower)
overall_deaths_race$smd <- as.numeric(overall_deaths_race$smd)


tm <- forest_theme(base_size = 10,
                   # Graphical parameters of confidence intervals
                   ci_pch = 15,
                   ci_col = "#0e8abb",
                   ci_fill = "red",
                   ci_alpha = 1,
                   ci_lty = 1,
                   ci_lwd = 2,
                   ci_Theight = 0.2,
                   # Graphical parameters of reference line
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Graphical parameters of vertical line
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20")

# Forest plot
overall_deaths_race_plot <- forest(overall_deaths_race[,c(1:4, 8)],
                                 est = overall_deaths_race$smd,
                                 lower = overall_deaths_race$CI_Lower, 
                                 upper = overall_deaths_race$CI_Upper,
                                 sizes = 0.8,
                                 ci_column = 4,
                                 ref_line = 1,
                                 arrow_lab = c("Favors Reference Group", "Favors Other Group"),
                                 xlim = c(0, 2),
                                 ticks_at = c(0, 0.5, 1, 2),
                                 xlab = "Odds Ratio",
                                 theme = tm)
plot(overall_deaths_race_plot)

#do overall deaths, sex group
#create the data frame
overall_deaths_sex <- data.frame(
  Age = c("Female", "Male"),
  Deaths = c(40319,56392),
  Controls = c(19700990,19657652),
  ` ` = paste(rep(" ", 20), collapse = " "),
  smd = c(1, 1.4017),
  CI_Lower = c(1,1.3839),
  CI_Upper = c(1,1.4198),
  `OR(95% CI)` = c("1 (Reference)", "1.4017 (1.3839, 1.4198)"),
  check.names = FALSE
)
#create plot


# Final data manipulation part 
overall_deaths_sex$CI_Upper <- as.numeric(overall_deaths_sex$CI_Upper)
overall_deaths_sex$CI_Lower <- as.numeric(overall_deaths_sex$CI_Lower)
overall_deaths_sex$smd <- as.numeric(overall_deaths_sex$smd)


tm <- forest_theme(base_size = 10,
                   # Graphical parameters of confidence intervals
                   ci_pch = 15,
                   ci_col = "#0e8abb",
                   ci_fill = "red",
                   ci_alpha = 1,
                   ci_lty = 1,
                   ci_lwd = 2,
                   ci_Theight = 0.2,
                   # Graphical parameters of reference line
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Graphical parameters of vertical line
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20")

# Forest plot
overall_deaths_sex_plot <- forest(overall_deaths_sex[,c(1:4, 8)],
                                est = overall_deaths_sex$smd,
                                lower = overall_deaths_sex$CI_Lower, 
                                upper = overall_deaths_sex$CI_Upper,
                                sizes = 0.8,
                                ci_column = 4,
                                ref_line = 1,
                                arrow_lab = c("Favors Reference Group", "Favors Other Group"),
                                xlim = c(0, 2),
                                ticks_at = c(0, 0.5, 1, 2),
                                xlab = "Odds Ratio",
                                theme = tm)

plot(overall_deaths_sex_plot)