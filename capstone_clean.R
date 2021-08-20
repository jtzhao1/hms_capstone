##

# load packages
library(data.table)
library(haven)
library(magrittr)
library(splines)
library(sf)
library(ggplot2)
library(BAMMtools)

# useful things
"%unlike%" <- Negate("%like%")
"%ni%" <- Negate("%in%")

# files
data_dir <- "FILEPATH"
lead_files <- list.files(data_dir, pattern = "lead")
demog_files <- list.files(data_dir, pattern = "demog")
house_files <- list.files(data_dir, pattern = "house")

# functions to compile files
read.lead <- function(file) {
  dt <- data.table(read_xpt(file.path(data_dir, file)))
  if ("LBXBPB" %in% names(dt)) {
    setnames(dt, "LBXBPB", "blood_lead")
  } else if ("LB2BPB" %in% names(dt)) {
    setnames(dt, "LB2BPB", "blood_lead")
  } else stop("no blood lead data")
  
  dt <- dt[!is.na(blood_lead), .(SEQN, blood_lead)]
  
  year <- substr(file, 8, 9)
  if (as.numeric(year) == 99) {
    year <- 1999
  } else {
    year <- as.numeric(paste0(20, year))
  }
  
  dt[, year := year]
  return(dt)
}

read.demog <- function(file) {
  dt <- data.table(read_xpt(file.path(data_dir, file)))
  
  year <- substr(file, 8, 9)
  if (as.numeric(year) == 99) {
    year <- 1999
  } else {
    year <- as.numeric(paste0(20, year))
  }
  
  dt[, year := year]
  
  # only relevant for 2017-2018 NHANES
  if ("DMDHREDZ" %in% names(dt)) { 
    dt[DMDHREDZ == 3, DMDHREDZ := 5] # "college or above"; all other NHANES have this coded as 5
    dt[DMDHREDZ == 2, DMDHREDZ := 3] # "high school grad/GED or some college/AA degree"; this is a composite of 3 (HS grad/GED) & 4 (some college/AA) from other NHANES
    # arbitrarily changing the code to 3 here
    
    setnames(dt, "DMDHREDZ", "DMDHREDU")
  }
  
  # edit education variable
  dt[RIDAGEYR < 20, educ_num := DMDHREDU] # for respondents 0-19, use HH ref person's education level (DMDHREDU)
  dt[RIDAGEYR >= 20, educ_num := DMDEDUC2] # for respondents 20+ use their own education level (DMDEDUC2)
  
  # edit categorical variable values
  dt[, sex := ifelse(RIAGENDR == 1, "Male", "Female")]
  
  dt[RIDRETH1 == 1, race_ethnicity := "Mexican American"]
  dt[RIDRETH1 == 2, race_ethnicity := "Other Hispanic"]
  dt[RIDRETH1 == 3, race_ethnicity := "Non-Hispanic White"]
  dt[RIDRETH1 == 4, race_ethnicity := "Non-Hispanic Black"]
  dt[RIDRETH1 == 5, race_ethnicity := "Other Race"]
  
  dt[educ_num == 1, education := "Less than 9th grade"]
  dt[educ_num == 2, education := "9-11th grade"]
  dt[educ_num == 3, education := "High school grad"]
  dt[educ_num == 4, education := "Some college"]
  dt[educ_num == 5, education := "College graduate"]
  dt[educ_num == 7, education := "Refused"]
  dt[educ_num == 9, education := "Don't know"]
  
  # edit variable names
  setnames(dt, c("RIDAGEYR", "INDFMPIR"), c("age", "inc_pov_ratio"))
  
  # get rid of "refused" & "don't know" answers
  dt <- dt[education %ni% c("Refused", "Don't know")]
  
  setcolorder(dt, c("SEQN", "year"))
  return(dt)
}

read.house <- function(file) {
  dt <- data.table(read_xpt(file.path(data_dir, file)))
  
  if ("HOD040" %in% names(dt)) {
    setnames(dt, "HOD040", "house_year_num")
  } else if ("HOQ040" %in% names(dt)) {
    setnames(dt, "HOQ040", "house_year_num")
  }
  
  dt[house_year_num == 1, house_year := "1990 to present"]
  dt[house_year_num == 2, house_year := "1978 to 1989"]
  dt[house_year_num == 3, house_year := "1960 to 1977"]
  dt[house_year_num == 4, house_year := "1950 to 1959"]
  dt[house_year_num == 5, house_year := "1940 to 1949"]
  dt[house_year_num == 6, house_year := "Before 1940"]
  dt[house_year_num == 77, house_year := "Refused"]
  dt[house_year_num == 99, house_year := "Don't know"]
  
  year <- substr(file, 8, 9)
  if (as.numeric(year) == 99) {
    year <- 1999
  } else {
    year <- as.numeric(paste0(20, year))
  }
  
  dt[, year := year]
  
  # get rid of "refused" & "don't know" answers
  dt <- dt[house_year %ni% c("Refused", "Don't know")]
  
  return(dt)
}

lead_data <- rbindlist(lapply(lead_files, read.lead), use.names = T)
demog_data <- rbindlist(lapply(demog_files, read.demog), fill = T)
house_data <- rbindlist(lapply(house_files, read.house), fill = T)

dt <- merge(lead_data, demog_data, by = c("SEQN", "year"), all = T)
dt <- merge(dt, house_data, by = c("SEQN", "year"), all = T)

# edit housing age variable
# change all the responses for years after 2009 to "question not asked"
dt[year > 2009, house_year := "Question not asked"]

# save
write.csv(dt, file.path(data_dir, "continuous_nhanes.csv"), row.names = F)

# load in
dt <- fread(file.path(data_dir, "continuous_nhanes.csv"))
# edit factor variable levels (for some reason these don't get saved as factors, so need to run this every time the data get loaded in)
dt[, race_ethnicity := factor(race_ethnicity, levels = c("Non-Hispanic White", "Non-Hispanic Black", "Mexican American", "Other Hispanic", "Other Race"))]
dt[, sex := factor(sex, levels = c("Female", "Male"))]
dt[, education := factor(education, levels = c("College graduate", "Some college", "High school grad", "9-11th grade", "Less than 9th grade"))]
dt[, house_year := factor(house_year, levels = c("Question not asked", "1990 to present", "1978 to 1989", "1960 to 1977", "1950 to 1959", "1940 to 1949", "Before 1940"))]

###### visualize relationships #####
viz.lead <- function(variable, data = dt, method = NULL, continuous = TRUE, add_data = FALSE, x_axis_name = NULL) {
  
  if (continuous) {
    if (add_data) {
      ggplot(data = data, aes(x = get(variable), y = log(blood_lead))) + 
        geom_smooth(method = method) + geom_point() +
        theme_bw() + theme(panel.grid.minor = element_blank()) + 
        labs(x = ifelse(is.null(x_axis_name), variable, x_axis_name), y = "Log Blood Lead")
    } else if (!add_data) {
      ggplot(data = data, aes(x = get(variable), y = log(blood_lead))) + 
        geom_smooth(method = method) + 
        theme_bw() + theme(panel.grid.minor = element_blank()) + 
        labs(x = ifelse(is.null(x_axis_name), variable, x_axis_name), y = "Log Blood Lead")
    }
  } else if (!continuous) {
    if (add_data) {
      ggplot(data = data, aes(x = get(variable), y = log(blood_lead))) + geom_smooth(method = method) + geom_point() +
        theme_bw() + theme(panel.grid.minor = element_blank()) + 
        labs(x = ifelse(is.null(x_axis_name), variable, x_axis_name), y = "Log Blood Lead")
    } else if (!add_data) {
      ggplot(data = data, aes(x = get(variable), y = log(blood_lead))) + geom_smooth(method = method) +
        theme_bw() + theme(panel.grid.minor = element_blank()) + 
        labs(x = ifelse(is.null(x_axis_name), variable, x_axis_name), y = "Log Blood Lead")
    }
  }
}

dt[, mean_lead_year := mean(blood_lead, na.rm = T), by = year]
ggplot(data = dt, aes(x = year, y = mean_lead_year)) + geom_line(size = 2) +
  theme_bw() + theme(panel.grid = element_blank()) +
  labs(y = "Blood Lead (µg/dL)")
dt[, mean_lead_age := mean(blood_lead, na.rm = T), by = age]
ggplot(data = dt, aes(x = age, y = mean_lead_age)) + geom_line(size = 2) +
  theme_bw() + theme(panel.grid = element_blank()) +
  labs(y = "Blood Lead (µg/dL)", x = "Age")

viz.lead("age", x_axis_name = "Age in years") # age in years
viz.lead("RIAGENDR", method = "glm") # male (1) or female (2)
viz.lead("inc_pov_ratio", x_axis_name = "Family poverty-income ratio") # family poverty-income ratio
viz.lead("RIDRETH1", method = "glm") # race/ethnicity [1|Mexican American; 2|Other Hispanic; 3|Non-Hispanic White; 4|Non-Hispanic Black; 5|Other]
viz.lead("educ_num", method = "lm") # education level
viz.lead("house_year_num", method = "glm")
viz.lead("year")

# save plots
pdf("FILEPATH/descriptive_plots.pdf", width = 10, height = 5)
viz.lead("age", x_axis_name = "Age in years")
viz.lead("inc_pov_ratio", x_axis_name = "Family poverty-income ratio")
viz.lead("educ_num", method = "lm", x_axis_name = "Education level")
dev.off()

##### run models #####

# dt_clean <- dt[, .(SEQN, year, blood_lead, age, sex, race_ethnicity, inc_pov_ratio, education, house_year)] %>% na.omit
dt_clean <- dt[, .(SEQN, year, blood_lead, age, sex, race_ethnicity, inc_pov_ratio, education)] %>% na.omit
# save
write.csv(dt_clean, "FILEPATH/data/continuous_nhanes_clean.csv", row.names = F)

# randomly choose row numbers
set.seed(429)
split <- sample(1:nrow(dt_clean), size = round(0.8*nrow(dt_clean)), replace = FALSE) # 80% of data for training, 20% for testing
# split into training & test sets
train <- dt_clean[split]
test <- dt_clean[-split]

# # spline on age, income-poverty ratio, & year
# model1 <- lm(log(blood_lead) ~ bs(age) + sex + race_ethnicity + bs(inc_pov_ratio) + education + house_year + bs(year), data = train) # all cubic splines
# model2 <- lm(log(blood_lead) ~ bs(age) + sex + race_ethnicity + bs(inc_pov_ratio, degree = 2) + education + house_year + bs(year), data = train) # 2nd degree for IPR
# model3 <- lm(log(blood_lead) ~ bs(age, degree = 4) + sex + race_ethnicity + bs(inc_pov_ratio) + education + house_year + bs(year), data = train) # 4th degree for age
# model4 <- lm(log(blood_lead) ~ bs(age, degree = 4) + sex + race_ethnicity + bs(inc_pov_ratio, degree = 2) +
#                education + house_year + bs(year), data = train) # 4th degree for age, 2nd degree for IPR
# model5 <- lm(log(blood_lead) ~ bs(age, degree = 4) + sex + race_ethnicity + bs(inc_pov_ratio, degree = 2) +
#                education + house_year + bs(year) + year:age, data = train) # 4th degree for age, 2nd degree for IPR
# removing housing age
model6 <- lm(log(blood_lead) ~ bs(age) + sex + race_ethnicity + bs(inc_pov_ratio) + education + bs(year), data = train) # all cubic splines
model7 <- lm(log(blood_lead) ~ bs(age) + sex + race_ethnicity + bs(inc_pov_ratio, degree = 2) + education + bs(year), data = train) # 2nd degree for IPR
model8 <- lm(log(blood_lead) ~ bs(age, degree = 4) + sex + race_ethnicity + bs(inc_pov_ratio) + education + bs(year), data = train) # 4th degree for age
model9 <- lm(log(blood_lead) ~ bs(age, degree = 4) + sex + race_ethnicity + bs(inc_pov_ratio, degree = 2) + 
               education + bs(year), data = train) # 4th degree for age, 2nd degree for IPR
model10 <- lm(log(blood_lead) ~ bs(age, degree = 4) + sex + race_ethnicity + bs(inc_pov_ratio, degree = 2) + 
                education + bs(year) + year:age, data = train) # 4th degree for age, 2nd degree for IPR, year:age interaction
model11 <- lm(log(blood_lead) ~ bs(age, degree = 4)*bs(year) + sex + race_ethnicity + bs(inc_pov_ratio, degree = 2) + 
                education, data = train) # 4th degree for age, 2nd degree for IPR, year*age interaction on splines
model12 <- lm(log(blood_lead) ~ age + sex + race_ethnicity + inc_pov_ratio + education + year, data = train) # linear everything

# save final model
saveRDS(model11, "FILEPATH/results/final_model.RDS")
# read in
model11 <- readRDS("FILEPATH/results/final_model.RDS")

# check age/year pattern to see if it makes sense
age_year <- data.table(expand.grid(year = 2000:2020, age = 1:85))
age_year[, `:=` (sex = "Female", race_ethnicity = "Non-Hispanic White", inc_pov_ratio = 5, education = "College graduate")]
age_year[, bll_pred11 := exp(predict(model11, newdata = age_year))]

pdf("FILEPATH/results/cohort_effect.pdf", width = 10, height = 6)
ggplot(data = age_year) + geom_tile(aes(x = age, y = year, fill = bll_pred11)) + scale_fill_fermenter(palette = "YlOrRd", direction = 1) +
  theme_bw() + theme(panel.grid = element_blank()) + labs(fill = "Predicted BLL (µg/dL)", x = "Age", y = "Year")
dev.off()

# calc MSE on test data
for (i in 6:12) {
  test[, paste0("bll_model", i) := exp(predict(get(paste0("model", i)), newdata = test[, .(SEQN, year, blood_lead, age, sex, race_ethnicity, inc_pov_ratio, education)]))]
}

for (i in 6:12) {
  test[, paste0("model", i, "_mse") := (get(paste0("bll_model", i)) - blood_lead)^2]
}

for (i in 6:12) {
  message(paste0("model", i))
  print(test[, mean(get(paste0("model", i, "_mse")), na.rm = T)])
}

###### maps ######
read.shp <- function(state) {
  state_folder <- file.path("FILEPATH/puma", state)
  state_files <- list.files(state_folder)
  shp_file <- state_files[state_files %like% ".shp" & state_files %unlike% ".xml"]
  
  shp <- st_read(file.path(state_folder, shp_file), quiet = T)
  
  # capitalize state names
  if (state == "district of columbia") { # DC
    shp$state <- "District of Columbia"
    
  } else if (length(strsplit(state, " ")[[1]]) == 2) { # two-named states
    state1 <- strsplit(state, " ")[[1]][1]
    state1 <- paste0(toupper(substr(state1, 1, 1)), substr(state1, 2, nchar(state1)))
    state2 <- strsplit(state, " ")[[1]][2]
    state2 <- paste0(toupper(substr(state2, 1, 1)), substr(state2, 2, nchar(state2)))
    state <- paste(state1, state2)
    
    shp$state <- state
    
  } else { # one-named states
    shp$state <- paste0(toupper(substr(state, 1, 1)), substr(state, 2, nchar(state)))
  }
  
  return(shp)
}

shapefile_dir <- "FILEPATH/puma"
states <- list.files(shapefile_dir)[list.files(shapefile_dir) %unlike% ".zip"]
for (x in states) {
  var_name <- gsub(" ", "_", x)
  assign(paste0(var_name, "_shp"), read.shp(x))
}

# entire USA
usa_shp <- data.frame()
usa_shp_vars <- ls()[ls() %in% paste0(gsub(" ", "_", states), "_shp")]

for (i in 1:length(usa_shp_vars)) {
  usa_shp <- rbind(usa_shp, get(usa_shp_vars[i]))
}

usa_shp$STATEFP10 <- as.numeric(usa_shp$STATEFP10)
usa_shp$PUMACE10 <- as.numeric(usa_shp$PUMACE10)

# save
saveRDS(usa_shp, "FILEPATH/puma/usa_shp.RDS")

ggplot(data = usa_shp) + geom_sf(color = "black", aes(fill = state)) + theme(legend.position = "bottom") +
  coord_sf(xlim = c(-180, -65))

# continental USA
cont_usa_shp <- data.frame()
cont_usa_vars <- usa_shp_vars[usa_shp_vars %ni% c("alaska_shp", "hawaii_shp")]

for (i in 1:length(cont_usa_vars)) {
  cont_usa_shp <- rbind(cont_usa_shp, get(cont_usa_vars[i]))
}

cont_usa_shp$STATEFP10 <- as.numeric(cont_usa_shp$STATEFP10)
cont_usa_shp$PUMACE10 <- as.numeric(cont_usa_shp$PUMACE10)

# save
saveRDS(cont_usa_shp, "FILEPATH/puma/cont_usa_shp.RDS")

ggplot(data = cont_usa_shp) + geom_sf(color = "black", aes(fill = state)) + theme(legend.position = "bottom")

#### USA state FIPS codes
fips_codes <- data.table(state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
                                   "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
                                   "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                                   "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                                   "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
                                   "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                                   "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
                         fips = c(1:2, 4:6, 8:13, 15:42, 44:51, 53:56))
write.csv(fips_codes, "FILEPATH/data/fips_codes.csv", row.names = F)

##### ipums #####
ipums <- fread("FILEPATH/data/ipums_raw.csv")
ipums_rep_wgts <- fread("FILEPATH/data/ipums_rep_wgts.csv")

## edit variables
clean.ipums <- function(data) {
  dt <- copy(data)
  dt[, names(dt) := lapply(.SD, as.numeric), .SDcols = names(dt)]
  
  # sex
  dt[, sex := ifelse(SEX == 1, "Male", "Female")]
  dt[, sex := factor(sex, levels = c("Female", "Male"))]
  # age
  dt[AGE <= 85, age := AGE]
  dt[AGE > 85, age := 85] # NHANES age cap is 85
  # race/ethnicity
  dt[HISPAN == 0 & RACE == 1, race_ethnicity := "Non-Hispanic White"]
  dt[HISPAN == 0 & RACE == 2, race_ethnicity := "Non-Hispanic Black"]
  dt[HISPAN == 1, race_ethnicity := "Mexican American"]
  dt[HISPAN %in% 2:4, race_ethnicity := "Other Hispanic"]
  dt[is.na(race_ethnicity), race_ethnicity := "Other Race"]
  dt[, race_ethnicity := factor(race_ethnicity, levels = c("Non-Hispanic White", "Non-Hispanic Black", "Mexican American", "Other Hispanic", "Other Race"))]
  # education
  dt[EDUCD == 1, education := as.character(NA)]
  dt[EDUCD %in% 2:26, education := "Less than 9th grade"] # no schooling completed to grade 8
  dt[EDUCD %in% c(30, 40, 50), education := "9-11th grade"] # grades 9, 10, 11
  dt[EDUCD %in% c(61, 63, 64), education := "High school grad"] # 12th grade, regular high school diploma, and GED or alternative credential
  dt[EDUCD %in% c(65, 71, 81), education := "Some college"] # some college but less than 1 year, 1 or more years of college but no degree, and associate's degree
  dt[EDUCD %in% c(101, 114:116), education := "College graduate"] # bachelor's, master's, professional degree beyond a bachelor's, and doctorate
  dt[, education := factor(education, levels = c("College graduate", "Some college", "High school grad", "9-11th grade", "Less than 9th grade"))]
  # income-poverty ratio
  dt[, inc_pov_ratio := POVERTY/100]
  dt[POVERTY == 0, inc_pov_ratio := NA]
  # year
  setnames(dt, "MULTYEAR", "year")
  dt[, YEAR := NULL]
  
  return(dt)
}

ipums <- clean.ipums(ipums)
write.csv(ipums, "FILEPATH/data/ipums_clean.csv", row.names = F)

ipums_rep_wgts <- clean.ipums(ipums_rep_wgts)
write.csv(ipums_rep_wgts, "FILEPATH/data/ipums_rep_wgts_clean.csv", row.names = F)

# read in
ipums_rep_wgts <- fread("FILEPATH/data/ipums_rep_wgts_clean.csv")

### predict
ipums_rep_wgts[, bll_pred := exp(predict(model11, newdata = ipums_rep_wgts[, .(year, sex, age, inc_pov_ratio, race_ethnicity, education)]))] # predicted BLL from model 11
ipums_rep_wgts_clean <- ipums_rep_wgts[!is.na(bll_pred)] # subset to non-NA rows
# save
write.csv(ipums_rep_wgts_clean, "FILEPATH/data/acs_data_clean.csv", row.names = F)

### calculate summaries
## all usa total
ipums_usa_total <- copy(ipums_rep_wgts_clean)
ipums_usa_total[, bll_wgted := sum((bll_pred*PERWT)/sum(PERWT)), by = c("year")]
# calculate uncertainty
ipums_usa_total[, paste0("rep_bll_pred_", 1:80) := lapply(.SD, function(x) sum((bll_pred*x)/sum(x))), 
                .SDcols = paste0("REPWTP", 1:80), by = c("year")]
ipums_usa_total[, paste0("sq_diff_", 1:80) := lapply(.SD, function(x) (x - bll_wgted)^2), .SDcols = paste0("rep_bll_pred_", 1:80)]
ipums_usa_total[, variance := (4/80)*rowSums(.SD), .SDcols = paste0("sq_diff_", 1:80)]
# clean up
ipums_usa_total <- unique(ipums_usa_total[, .(year, bll_wgted, variance)])
ipums_usa_total[, se := sqrt(variance)]
setnames(ipums_usa_total, "bll_wgted", "bll_mean")
ipums_usa_total[, `:=` (bll_lower = bll_mean - se*qnorm(0.975), bll_upper = bll_mean + se*qnorm(0.975))]

## by age total
ipums_age_total <- copy(ipums_rep_wgts_clean)
# create categories
ipums_age_total[age < 25, age_cat := "Under 25"]
ipums_age_total[age >= 25 & age <= 50, age_cat := "25 to 50"]
ipums_age_total[age > 50, age_cat := "Over 50"]
ipums_age_total[, bll_wgted := sum((bll_pred*PERWT)/sum(PERWT)), by = c("year", "age_cat")]
# calculate uncertainty
ipums_age_total[, paste0("rep_bll_pred_", 1:80) := lapply(.SD, function(x) sum((bll_pred*x)/sum(x))), 
                .SDcols = paste0("REPWTP", 1:80), by = c("year", "age_cat")]
ipums_age_total[, paste0("sq_diff_", 1:80) := lapply(.SD, function(x) (x - bll_wgted)^2), .SDcols = paste0("rep_bll_pred_", 1:80)]
ipums_age_total[, variance := (4/80)*rowSums(.SD), .SDcols = paste0("sq_diff_", 1:80)]
# clean up
ipums_age_total <- unique(ipums_age_total[, .(year, age_cat, bll_wgted, variance)])
ipums_age_total[, se := sqrt(variance)]
setnames(ipums_age_total, "bll_wgted", "bll_mean")
ipums_age_total[, `:=` (bll_lower = bll_mean - se*qnorm(0.975), bll_upper = bll_mean + se*qnorm(0.975))]

## by race/ethnicity total
ipums_race_total <- copy(ipums_rep_wgts_clean)
ipums_race_total[race_ethnicity %in% c("Mexican American", "Other Hispanic"), race_ethnicity := "Hispanic"]
ipums_race_total[, bll_wgted := sum((bll_pred*PERWT)/sum(PERWT)), by = c("year", "race_ethnicity")]
# calculate uncertainty
ipums_race_total[, paste0("rep_bll_pred_", 1:80) := lapply(.SD, function(x) sum((bll_pred*x)/sum(x))), 
                 .SDcols = paste0("REPWTP", 1:80), by = c("year", "race_ethnicity")]
ipums_race_total[, paste0("sq_diff_", 1:80) := lapply(.SD, function(x) (x - bll_wgted)^2), .SDcols = paste0("rep_bll_pred_", 1:80)]
ipums_race_total[, variance := (4/80)*rowSums(.SD), .SDcols = paste0("sq_diff_", 1:80)]
# clean up
ipums_race_total <- unique(ipums_race_total[, .(year, race_ethnicity, bll_wgted, variance)])
ipums_race_total[, se := sqrt(variance)]
setnames(ipums_race_total, "bll_wgted", "bll_mean")
ipums_race_total[, `:=` (bll_lower = bll_mean - se*qnorm(0.975), bll_upper = bll_mean + se*qnorm(0.975))]

## by poverty level total
ipums_ipr_total <- copy(ipums_rep_wgts_clean)
# create categories
ipums_ipr_total[inc_pov_ratio < 1, ipr_cat := "<1"]
ipums_ipr_total[inc_pov_ratio >= 1 & inc_pov_ratio < 2, ipr_cat := "1-2"]
ipums_ipr_total[inc_pov_ratio >= 2 & inc_pov_ratio < 3, ipr_cat := "2-3"]
ipums_ipr_total[inc_pov_ratio >= 3 & inc_pov_ratio < 4, ipr_cat := "3-4"]
ipums_ipr_total[inc_pov_ratio >= 4 & inc_pov_ratio <= 5, ipr_cat := "4-5"]
ipums_ipr_total[inc_pov_ratio > 5, ipr_cat := ">5"]
ipums_ipr_total[, bll_puma_ipr_wgted := sum((bll_pred*PERWT)/sum(PERWT)), by = c("year", "ipr_cat")] # for each level of IPR within each PUMA, calculate the weighted mean BLL
# calculate uncertainty
ipums_ipr_total[, paste0("rep_bll_pred_", 1:80) := lapply(.SD, function(x) sum((bll_pred*x)/sum(x))), 
                .SDcols = paste0("REPWTP", 1:80), by = c("year", "ipr_cat")]
ipums_ipr_total[, paste0("sq_diff_", 1:80) := lapply(.SD, function(x) (x - bll_puma_ipr_wgted)^2), .SDcols = paste0("rep_bll_pred_", 1:80)]
ipums_ipr_total[, variance := (4/80)*rowSums(.SD), .SDcols = paste0("sq_diff_", 1:80)]
# clean up
ipums_ipr_total <- unique(ipums_ipr_total[, .(year, ipr_cat, bll_puma_ipr_wgted, variance)])
ipums_ipr_total[, se := sqrt(variance)]
setnames(ipums_ipr_total, "bll_puma_ipr_wgted", "bll_mean")
ipums_ipr_total[, `:=` (bll_lower = bll_mean - se*qnorm(0.975), bll_upper = bll_mean + se*qnorm(0.975))]
ipums_ipr_total[, ipr_cat := factor(ipr_cat, levels = c("<1", "1-2", "2-3", "3-4", "4-5", ">5"))]

## all usa
ipums_usa <- copy(ipums_rep_wgts_clean)
ipums_usa[, bll_puma_wgted := sum((bll_pred*PERWT)/sum(PERWT)), by = c("year", "STATEFIP", "PUMA")] # for each PUMA, calculate the weighted mean BLL
# calculate uncertainty using steps from https://www2.census.gov/programs-surveys/acs/tech_docs/pums/ACS2015_2019_PUMS_README.pdf (pg 12)
ipums_usa[, paste0("rep_bll_pred_", 1:80) := lapply(.SD, function(x) sum((bll_pred*x)/sum(x))), 
          .SDcols = paste0("REPWTP", 1:80), by = c("year", "STATEFIP", "PUMA")]
ipums_usa[, paste0("sq_diff_", 1:80) := lapply(.SD, function(x) (x - bll_puma_wgted)^2), .SDcols = paste0("rep_bll_pred_", 1:80)]
ipums_usa[, variance := (4/80)*rowSums(.SD), .SDcols = paste0("sq_diff_", 1:80)]
# clean up
ipums_usa <- unique(ipums_usa[, .(STATEFIP, PUMA, year, bll_puma_wgted, variance)])
ipums_usa[, se := sqrt(variance)]
setnames(ipums_usa, "bll_puma_wgted", "bll_mean")
ipums_usa[, `:=` (bll_lower = bll_mean - se*qnorm(0.975), bll_upper = bll_mean + se*qnorm(0.975))]
# save
write.csv(ipums_usa, "FILEPATH/data/bll_ipums_usa.csv", row.names = F)

## by race/ethnicity
ipums_race <- copy(ipums_rep_wgts_clean) 
ipums_race[race_ethnicity %in% c("Mexican American", "Other Hispanic"), race_ethnicity := "Hispanic"] # combine mex am & other hisp
ipums_race[, bll_puma_race_wgted := sum((bll_pred*PERWT)/sum(PERWT)), by = c("year", "STATEFIP", "PUMA", "race_ethnicity")] # for each race/eth within each PUMA, calculate the weighted mean BLL
# calculate uncertainty
ipums_race[, paste0("rep_bll_pred_", 1:80) := lapply(.SD, function(x) sum((bll_pred*x)/sum(x))), 
           .SDcols = paste0("REPWTP", 1:80), by = c("year", "STATEFIP", "PUMA", "race_ethnicity")]
ipums_race[, paste0("sq_diff_", 1:80) := lapply(.SD, function(x) (x - bll_puma_race_wgted)^2), .SDcols = paste0("rep_bll_pred_", 1:80)]
ipums_race[, variance := (4/80)*rowSums(.SD), .SDcols = paste0("sq_diff_", 1:80)]
# clean up
ipums_race <- unique(ipums_race[, .(STATEFIP, PUMA, year, race_ethnicity, bll_puma_race_wgted, variance)])
ipums_race[, se := sqrt(variance)]
setnames(ipums_race, "bll_puma_race_wgted", "bll_mean")
ipums_race[, `:=` (bll_lower = bll_mean - se*qnorm(0.975), bll_upper = bll_mean + se*qnorm(0.975))]
# save
write.csv(ipums_race, "FILEPATH/data/bll_ipums_race.csv", row.names = F)

## by income-poverty ratio
ipums_ipr <- copy(ipums_rep_wgts_clean)
# create categories
ipums_ipr[inc_pov_ratio < 1, ipr_cat := "<1"]
ipums_ipr[inc_pov_ratio >= 1 & inc_pov_ratio < 2, ipr_cat := "1-2"]
ipums_ipr[inc_pov_ratio >= 2 & inc_pov_ratio < 3, ipr_cat := "2-3"]
ipums_ipr[inc_pov_ratio >= 3 & inc_pov_ratio < 4, ipr_cat := "3-4"]
ipums_ipr[inc_pov_ratio >= 4 & inc_pov_ratio <= 5, ipr_cat := "4-5"]
ipums_ipr[inc_pov_ratio > 5, ipr_cat := ">5"]
ipums_ipr[, bll_puma_ipr_wgted := sum((bll_pred*PERWT)/sum(PERWT)), by = c("year", "STATEFIP", "PUMA", "ipr_cat")] # for each level of IPR within each PUMA, calculate the weighted mean BLL
# calculate uncertainty
ipums_ipr[, paste0("rep_bll_pred_", 1:80) := lapply(.SD, function(x) sum((bll_pred*x)/sum(x))), 
          .SDcols = paste0("REPWTP", 1:80), by = c("year", "STATEFIP", "PUMA", "ipr_cat")]
ipums_ipr[, paste0("sq_diff_", 1:80) := lapply(.SD, function(x) (x - bll_puma_ipr_wgted)^2), .SDcols = paste0("rep_bll_pred_", 1:80)]
ipums_ipr[, variance := (4/80)*rowSums(.SD), .SDcols = paste0("sq_diff_", 1:80)]
# clean up
ipums_ipr <- unique(ipums_ipr[, .(STATEFIP, PUMA, year, ipr_cat, bll_puma_ipr_wgted, variance)])
ipums_ipr[, se := sqrt(variance)]
setnames(ipums_ipr, "bll_puma_ipr_wgted", "bll_mean")
ipums_ipr[, `:=` (bll_lower = bll_mean - se*qnorm(0.975), bll_upper = bll_mean + se*qnorm(0.975))]
# save
write.csv(ipums_ipr, "FILEPATH/data/bll_ipums_ipr.csv", row.names = F)

## by income-poverty ratio and race/ethnicity
ipums_ipr_race <- copy(ipums_rep_wgts_clean)
ipums_ipr_race[race_ethnicity %in% c("Mexican American", "Other Hispanic"), race_ethnicity := "Hispanic"] # combine mex am & other hisp
# create categories
ipums_ipr_race[inc_pov_ratio < 1, ipr_cat := "<1"]
ipums_ipr_race[inc_pov_ratio >= 1 & inc_pov_ratio < 2, ipr_cat := "1-2"]
ipums_ipr_race[inc_pov_ratio >= 2 & inc_pov_ratio < 3, ipr_cat := "2-3"]
ipums_ipr_race[inc_pov_ratio >= 3 & inc_pov_ratio < 4, ipr_cat := "3-4"]
ipums_ipr_race[inc_pov_ratio >= 4 & inc_pov_ratio <= 5, ipr_cat := "4-5"]
ipums_ipr_race[inc_pov_ratio > 5, ipr_cat := ">5"]
ipums_ipr_race[, bll_puma_ipr_wgted := sum((bll_pred*PERWT)/sum(PERWT)), by = c("year", "ipr_cat", "race_ethnicity")] # for each level of IPR within each race/eth, calculate the weighted mean BLL
# calculate uncertainty
ipums_ipr_race[, paste0("rep_bll_pred_", 1:80) := lapply(.SD, function(x) sum((bll_pred*x)/sum(x))), 
               .SDcols = paste0("REPWTP", 1:80), by = c("year", "ipr_cat", "race_ethnicity")]
ipums_ipr_race[, paste0("sq_diff_", 1:80) := lapply(.SD, function(x) (x - bll_puma_ipr_wgted)^2), .SDcols = paste0("rep_bll_pred_", 1:80)]
ipums_ipr_race[, variance := (4/80)*rowSums(.SD), .SDcols = paste0("sq_diff_", 1:80)]
# clean up
ipums_ipr_race <- unique(ipums_ipr_race[, .(year, ipr_cat, race_ethnicity, bll_puma_ipr_wgted, variance)])
ipums_ipr_race[, se := sqrt(variance)]
setnames(ipums_ipr_race, "bll_puma_ipr_wgted", "bll_mean")
ipums_ipr_race[, `:=` (bll_lower = bll_mean - se*qnorm(0.975), bll_upper = bll_mean + se*qnorm(0.975))]
ipums_ipr_race[, ipr_cat := factor(ipr_cat, levels = c("<1", "1-2", "2-3", "3-4", "4-5", ">5"))]

ipums_ipr_race_clean <- ipums_ipr_race[year == 2019][order(race_ethnicity, ipr_cat)]
ipums_ipr_race_clean[, c("year", "variance", "se") := NULL]
ipums_ipr_race_clean[, c("bll_mean", "bll_lower", "bll_upper") := lapply(.SD, function (x) round(x, 3)), .SDcols = c("bll_mean", "bll_lower", "bll_upper")]
ipums_ipr_race_clean[, BLL := paste0(bll_mean, " (", bll_lower, "-", bll_upper, ")")]
ipums_ipr_race_clean[, c("bll_mean", "bll_lower", "bll_upper") := NULL]
ipums_ipr_race_clean <- dcast(ipums_ipr_race_clean, ipr_cat ~ race_ethnicity, value.var = "BLL")
write.csv(ipums_ipr_race_clean, "FILEPATH/results/ipr_race.csv", row.names = F)

## by education
ipums_educ <- copy(ipums_rep_wgts_clean)
ipums_educ[, bll_puma_educ_wgted := sum((bll_pred*PERWT)/sum(PERWT)), by = c("year", "STATEFIP", "PUMA", "education")] # for each education level within each PUMA, calculate the weighted mean BLL
# calculate uncertainty
ipums_educ[, paste0("rep_bll_pred_", 1:80) := lapply(.SD, function(x) sum((bll_pred*x)/sum(x))), 
           .SDcols = paste0("REPWTP", 1:80), by = c("year", "STATEFIP", "PUMA", "education")]
ipums_educ[, paste0("sq_diff_", 1:80) := lapply(.SD, function(x) (x - bll_puma_educ_wgted)^2), .SDcols = paste0("rep_bll_pred_", 1:80)]
ipums_educ[, variance := (4/80)*rowSums(.SD), .SDcols = paste0("sq_diff_", 1:80)]
# clean up
ipums_educ <- unique(ipums_educ[, .(STATEFIP, PUMA, year, education, bll_puma_educ_wgted, variance)])
ipums_educ[, se := sqrt(variance)]
setnames(ipums_educ, "bll_puma_educ_wgted", "bll_mean")
ipums_educ[, `:=` (bll_lower = bll_mean - se*qnorm(0.975), bll_upper = bll_mean + se*qnorm(0.975))]
# save
write.csv(ipums_educ, "FILEPATH/data/bll_ipums_educ.csv", row.names = F)

## by age
ipums_age <- copy(ipums_rep_wgts_clean)
# create categories
ipums_age[age < 25, age_cat := "Under 25"]
ipums_age[age >= 25 & age <= 50, age_cat := "25 to 50"]
ipums_age[age > 50, age_cat := "Over 50"]
ipums_age[, bll_puma_age_wgted := sum((bll_pred*PERWT)/sum(PERWT)), by = c("year", "STATEFIP", "PUMA", "age_cat")] # for each age group within each PUMA, calculate the weighted mean BLL
# calculate uncertainty
ipums_age[, paste0("rep_bll_pred_", 1:80) := lapply(.SD, function(x) sum((bll_pred*x)/sum(x))), 
          .SDcols = paste0("REPWTP", 1:80), by = c("year", "STATEFIP", "PUMA", "age_cat")]
ipums_age[, paste0("sq_diff_", 1:80) := lapply(.SD, function(x) (x - bll_puma_age_wgted)^2), .SDcols = paste0("rep_bll_pred_", 1:80)]
ipums_age[, variance := (4/80)*rowSums(.SD), .SDcols = paste0("sq_diff_", 1:80)]
# clean up
ipums_age <- unique(ipums_age[, .(STATEFIP, PUMA, year, age_cat, bll_puma_age_wgted, variance)])
ipums_age[, se := sqrt(variance)]
setnames(ipums_age, "bll_puma_age_wgted", "bll_mean")
ipums_age[, `:=` (bll_lower = bll_mean - se*qnorm(0.975), bll_upper = bll_mean + se*qnorm(0.975))]
# save
write.csv(ipums_age, "FILEPATH/data/bll_ipums_age.csv", row.names = F)

## maps
# read in shapefiles
cont_usa_shp <- readRDS("FILEPATH/puma/cont_usa_shp.RDS")
# set break points for maps
map_breaks <- round(quantile(ipums_usa[year == 2019, bll_mean], seq(0.2, 0.8, 0.2)), 2) # overall quantiles
map_breaks <- round(quantile(ipums_race$bll_mean, seq(0.2, 0.8, 0.2)), 2) # race quantiles
map_breaks <- round(quantile(ipums_ipr$bll_mean, seq(0.2, 0.8, 0.2)), 2) # poverty quantiles

# USA all (minus Alaska & Hawaii)
usa_shp_all <- merge(cont_usa_shp, ipums_usa, by.x = c("STATEFP10", "PUMACE10"), by.y = c("STATEFIP", "PUMA"))

map_usa_all <- ggplot(data = usa_shp_all[usa_shp_all$year == 2019,]) + geom_sf(color = "black", aes(fill = bll_mean)) + theme_bw() +
  scale_fill_fermenter(palette = "YlOrRd", direction = 1, breaks = map_breaks, labels = map_breaks) + 
  labs(fill = "BLL (µg/dL)", title = "Mean blood lead levels, 2019") + theme(panel.grid = element_blank())

# by race/ethnicity
usa_shp_race <- merge(cont_usa_shp, ipums_race, by.x = c("STATEFP10", "PUMACE10"), by.y = c("STATEFIP", "PUMA"))

map_white <- ggplot(data = usa_shp_race[usa_shp_race$year == 2019 & usa_shp_race$race_ethnicity == "Non-Hispanic White",]) + 
  geom_sf(color = "black", aes(fill = bll_mean)) + theme_bw() + theme(panel.grid = element_blank()) +
  scale_fill_fermenter(palette = "YlOrRd", direction = 1, breaks = map_breaks, labels = map_breaks) +
  labs(fill = "BLL (µg/dL)", title = "Mean blood lead levels, 2019, Non-Hispanic White")

map_black <- ggplot(data = usa_shp_race[usa_shp_race$year == 2019 & usa_shp_race$race_ethnicity == "Non-Hispanic Black",]) + 
  geom_sf(color = "black", aes(fill = bll_mean)) + theme_bw() + theme(panel.grid = element_blank()) +
  scale_fill_fermenter(palette = "YlOrRd", direction = 1, breaks = map_breaks, labels = map_breaks) +
  labs(fill = "BLL (µg/dL)", title = "Mean blood lead levels, 2019, Non-Hispanic Black")

map_hisp <- ggplot(data = usa_shp_race[usa_shp_race$year == 2019 & usa_shp_race$race_ethnicity == "Hispanic",]) + 
  geom_sf(color = "black", aes(fill = bll_mean)) + theme_bw() + theme(panel.grid = element_blank()) +
  scale_fill_fermenter(palette = "YlOrRd", direction = 1, breaks = map_breaks, labels = map_breaks) +
  labs(fill = "BLL (µg/dL)", title = "Mean blood lead levels, 2019, Hispanic")

map_other <- ggplot(data = usa_shp_race[usa_shp_race$year == 2019 & usa_shp_race$race_ethnicity == "Other Race",]) + 
  geom_sf(color = "black", aes(fill = bll_mean)) + theme_bw() + theme(panel.grid = element_blank()) +
  scale_fill_fermenter(palette = "YlOrRd", direction = 1, breaks = map_breaks, labels = map_breaks) +
  labs(fill = "BLL (µg/dL)", title = "Mean blood lead levels, 2019, Other Race")

# by income-poverty ratio
usa_shp_ipr <- merge(cont_usa_shp, ipums_ipr, by.x = c("STATEFP10", "PUMACE10"), by.y = c("STATEFIP", "PUMA"))

map_over_5 <- ggplot(data = usa_shp_ipr[usa_shp_ipr$year == 2019 & usa_shp_ipr$ipr_cat == ">5",]) + 
  geom_sf(color = "black", aes(fill = bll_mean)) + theme_bw() + theme(panel.grid = element_blank()) +
  scale_fill_fermenter(palette = "YlOrRd", direction = 1, breaks = map_breaks, labels = map_breaks) +
  labs(fill = "BLL (µg/dL)", title = "Mean blood lead levels, 2019, Income-poverty ratio > 5")

map_under_1 <- ggplot(data = usa_shp_ipr[usa_shp_ipr$year == 2019 & usa_shp_ipr$ipr_cat == "<1",]) + 
  geom_sf(color = "black", aes(fill = bll_mean)) + theme_bw() + theme(panel.grid = element_blank()) +
  scale_fill_fermenter(palette = "YlOrRd", direction = 1, breaks = map_breaks, labels = map_breaks) +
  labs(fill = "BLL (µg/dL)", title = "Mean blood lead levels, 2019, Income-poverty ratio < 1")

# by age
usa_shp_age <- merge(cont_usa_shp, ipums_age, by.x = c("STATEFP10", "PUMACE10"), by.y = c("STATEFIP", "PUMA"))

map_under_25 <- ggplot(data = usa_shp_age[usa_shp_age$year == 2019 & usa_shp_age$age_cat == "Under 25",]) + 
  geom_sf(color = "black", aes(fill = bll_mean)) + theme_bw() + theme(panel.grid = element_blank()) +
  scale_fill_fermenter(palette = "YlOrRd", direction = 1, breaks = map_breaks, labels = map_breaks) +
  labs(fill = "BLL (µg/dL)", title = "Mean blood lead levels, 2019, Under 25")

map_25_50 <- ggplot(data = usa_shp_age[usa_shp_age$year == 2019 & usa_shp_age$age_cat == "25 to 50",]) + 
  geom_sf(color = "black", aes(fill = bll_mean)) + theme_bw() + theme(panel.grid = element_blank()) +
  scale_fill_fermenter(palette = "YlOrRd", direction = 1) +
  labs(fill = "BLL (µg/dL)", title = "Mean blood lead levels, 2019, 25 to 50")

map_over_50 <- ggplot(data = usa_shp_age[usa_shp_age$year == 2019 & usa_shp_age$age_cat == "Over 50",]) + 
  geom_sf(color = "black", aes(fill = bll_mean)) + theme_bw() + theme(panel.grid = element_blank()) +
  scale_fill_fermenter(palette = "YlOrRd", direction = 1) +
  labs(fill = "BLL (µg/dL)", title = "Mean blood lead levels, 2019, Over 50")


pdf("/FILEPATH/bll_maps.pdf", width = 15, height = 10)
print(map_usa_all)
print(map_white)
print(map_black)
print(map_hisp)
print(map_other)
print(map_over_5)
print(map_under_1)
dev.off()


# age standardize race/ethnicity BLLs
ipums_age_std_race <- copy(ipums_rep_wgts_clean)
ipums_age_std_race[race_ethnicity %in% c("Mexican American", "Other Hispanic"), race_ethnicity := "Hispanic"]
ipums_age_std_race[age < 25, age_cat := "Under 25"]
ipums_age_std_race[age >= 25 & age <= 50, age_cat := "25 to 50"]
ipums_age_std_race[age > 50, age_cat := "Over 50"]

ipums_age_std_race[, age_std_bll := sum((bll_pred*PERWT)/sum(PERWT)), by = c("age_cat", "race_ethnicity", "year")] # for each age group within each PUMA, calculate the weighted mean BLL
# calculate uncertainty
ipums_age_std_race[, paste0("rep_bll_pred_", 1:80) := lapply(.SD, function(x) sum((bll_pred*x)/sum(x))), 
                   .SDcols = paste0("REPWTP", 1:80), by = c("age_cat", "race_ethnicity", "year")]
ipums_age_std_race[, paste0("sq_diff_", 1:80) := lapply(.SD, function(x) (x - age_std_bll)^2), .SDcols = paste0("rep_bll_pred_", 1:80)]
ipums_age_std_race[, variance := (4/80)*rowSums(.SD), .SDcols = paste0("sq_diff_", 1:80)]
# clean up
ipums_age_std_race <- unique(ipums_age_std_race[, .(race_ethnicity, year, age_cat, age_std_bll, variance)])
ipums_age_std_race[, se := sqrt(variance)]
setnames(ipums_age_std_race, "age_std_bll", "bll_mean")
ipums_age_std_race[, `:=` (bll_lower = bll_mean - se*qnorm(0.975), bll_upper = bll_mean + se*qnorm(0.975))]
ipums_age_std_race[, age_cat := factor(age_cat, levels = c("Under 25", "25 to 50", "Over 50"))]

# age standardize BLLs
ipums_age_std <- copy(ipums_rep_wgts_clean)
ipums_age_std[age < 25, age_cat := "Under 25"]
ipums_age_std[age >= 25 & age <= 50, age_cat := "25 to 50"]
ipums_age_std[age > 50, age_cat := "Over 50"]

ipums_age_std[, age_std_bll := sum((bll_pred*PERWT)/sum(PERWT)), by = c("age_cat", "year")] # for each age group within each PUMA, calculate the weighted mean BLL
# calculate uncertainty
ipums_age_std[, paste0("rep_bll_pred_", 1:80) := lapply(.SD, function(x) sum((bll_pred*x)/sum(x))), 
              .SDcols = paste0("REPWTP", 1:80), by = c("age_cat", "year")]
ipums_age_std[, paste0("sq_diff_", 1:80) := lapply(.SD, function(x) (x - age_std_bll)^2), .SDcols = paste0("rep_bll_pred_", 1:80)]
ipums_age_std[, variance := (4/80)*rowSums(.SD), .SDcols = paste0("sq_diff_", 1:80)]
# clean up
ipums_age_std <- unique(ipums_age_std[, .(year, age_cat, age_std_bll, variance)])
ipums_age_std[, se := sqrt(variance)]
setnames(ipums_age_std, "age_std_bll", "bll_mean")
ipums_age_std[, `:=` (bll_lower = bll_mean - se*qnorm(0.975), bll_upper = bll_mean + se*qnorm(0.975))]
ipums_age_std[, age_cat := factor(age_cat, levels = c("Under 25", "25 to 50", "Over 50"))]
