# Libraries ---
library(ggplot2)
source("consulting05-2025-mv/code/load_data_config.R")
source("consulting05-2025-mv/code/load_data_functions.R")

lct <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", lct)

################################################## ICU ################################################## 
icu = haven::read_sas(paste0(sas_data_path, "icuinfo.sas7bdat"), encoding = sas_encoding) 
    # # Fix colnames
    # rename_cols(rename_list = patient_rename_list) |> 
    # # Fix data encoding
    # encode_cols(encoding_triplets = patient_encoding_triplets)

################################################## Patient ################################################## 
# Following fully based on preproc-data Script. 
# NOTE: This is not a general pipeline.
    # That is, e.g. I removed parts of the sanity checks where the original code could not find any cases. 

patient = haven::read_sas(paste0(sas_data_path, "patientinfo.sas7bdat"), encoding = sas_encoding) |> 
    # Fix colnames
    rename_cols(rename_list = patient_rename_list) |> 
    # Fix data encoding
    encode_cols(encoding_triplets = patient_encoding_triplets)
# In case you want to check csv file containing the encodings (which is more easily to read)
# describe_variables(df = patient, df_name = "patient", output_file = "describe_patient.csv")

# Sanity Checks ---
## Patient - Data Set
# Initially: 21_100 rows = patients
removed_patient = tibble::tibble(
    combined_id = character(),
    cause = character(),
    comment = character()
)
# TODO: Out of interest: Check if there are duplicates in removed_patient i.e. multiple reasons for a patient to be removed. Curious.
# Remove Patients with td_vars_new incosistencies 
# 1) MV discontinuation before or equal to MV start
removed_patient = patient |> 
    # dplyr::filter(MechVent >= ventDisTime, !is.na(MechVent >= ventDisTime))
    dplyr::filter(mv_start_date >= mv_discontinued_date) |> 
    add_to_be_removed(
        df_base = removed_patient, 
        cause = "inconsistent", 
        comment = "mv discontinuation before or equal to mv start"
    )

# 2) ICU admission before hospital admission
# -> 4 patient which we just remove
removed_patient = patient |> 
    dplyr::filter(icu_admission_date < hospital_admission_date) |> 
    add_to_be_removed(
        df_base = removed_patient, 
        cause = "inconsistent", 
        comment = "icu admission before hospital admission"
    )

# 3) hospital discharge before icu discharge?
# 5 patients discharged from hospital before being discharged from icu.
# 2 seem to be just inaccurateley timed in the system. Keep them.
patient |> 
    dplyr::filter(hospital_discharge_date < icu_discharge_date) |> 
    dplyr::select(combined_id, hospital_discharge_date, icu_discharge_date) |> 
    dplyr::mutate(time_diff = icu_discharge_date - hospital_discharge_date) 

removed_patient = patient |> 
    dplyr::filter(hospital_discharge_date < icu_discharge_date) |> 
    dplyr::select(combined_id, hospital_discharge_date, icu_discharge_date) |> 
    dplyr::mutate(time_diff = icu_discharge_date - hospital_discharge_date) |> 
    dplyr::filter(time_diff > 3) |> 
    add_to_be_removed(
        df_base = removed_patient, 
        cause = "inconsistent", 
        comment = "hospital discharge before icu discharge"
    )

# 4) MV discontinuation before admission?
removed_patient = patient |> 
    dplyr::filter(mv_discontinued_date < icu_admission_date) |> 
    add_to_be_removed(
        df_base = removed_patient, 
        cause = "inconsistent", 
        comment = "mv discontinued before admission"
    )

# 5) Remove patients with missing indication of death/survival
patient |> 
    dplyr::filter(is.na(has_died))

# 6) Missing BMI
# TODO: Apply if necessary 
patient |> 
    dplyr::filter(is.na(bmi))

# 7) Missing AII Score
# TODO: Apply if necessary
patient |> 
    dplyr::filter(is.na(apache_score))

# 8) Missing Sex (only 3 patient have missing sex. Just remove them.)
removed_patient = patient |> 
    dplyr::filter(is.na(sex)) |> 
    add_to_be_removed(
        df_base = removed_patient, 
        cause = "incomplete", 
        comment = "missing gender"
    )

### End Sanity Checks

# Filter all patients to be removed:
patient = patient |> 
    dplyr::filter(!combined_id %in% removed_patient$combined_id) 


# Create own new variables
patient = patient |> 
    dplyr::mutate(
        # Calculate survival time:
            # ! Only available if patient died
        # We define discharge from hospital as the LATEST measured discharge event
        # in case there were discrepancies. However discharge from hospital must
        # have been tracked.
        # survival_time = death_date - icu_admission_date,
        # Surv0To60 = PatientDeathTime - AdmissionDate, # NOTE: Admission date is date of admission IN ICU!! 

        # Calculate discharge time from icu:
            # This is discharge FROM ICU! 
            # Note: Patient may have died but have been discharge from ICU prior to their death
        
        # Disc0To60 = icuDischargeDate - AdmissionDate,

        # Calculate time until death in icu: 
        # TODO: Following is copied from old code. Encoding is shit tho... Fix later

        # subjects that die after discharge from ICU are considered censored w.r.t. to death on ICU
        # release from ICU will be coded as cause =1; death in ICU as cause = 2
        # if subjects remain in ICU for t > 60, they will be consored

        # if death time is given and < 60 days, these are deaths, cause will be set to 2
        # death time > 60 days will be administratively censored

        # patient discharged from ICU before death are considered a competing risk for death
        # the time until event is set to discharge time and cause to 1

        # subjects where discharge time from icu is given but no information on death time
        # time until event is set to discharge time, cause is set to 1

        # remaining NAs are assumed to not have been discharged from ICU during the observation period
        # time until discharge will be set to 61, cause will be set to 0, i.e. censoring

        # in case there are further observations with time until event > 60
        # set time until event to 61, cause to 0, i.e. censored
        
        # Think all the info is already in the data set:
        # lol...

        # Discharged from icu:
        # TODO: sas-data contains has_died_in_icu; compare
            # -> Would like to rely on own variable because I do not fully understand sas variable. 
        died_in_icu = icu_discharge_date < death_date,  # If NA, then Person died in icu --> no discharge date
        icu_days = dplyr::if_else(died_in_icu == TRUE, death_date - icu_admission_date, icu_discharge_date - icu_admission_date),
        icu_calender_days = floor(icu_days), 
        # surv_icu0to60 = icudays, 
        # Alternative already contained in data:
        # surv_icu0to60 = ICUDAYS,  # already has 60+ transformed to 61 (i.e. loss of info. Note sure if I like)
        # Compare information: patient |> dplyr::select(ICUDAYS, icudays, diedInIcu) |> dplyr::mutate(diff = ICUDAYS - icudays) |> ggplot() + geom_point(aes(x = ICUDAYS, y = icudays))

        has_died_icu_status = dplyr::case_when(
          # Only observe event if patient died within icu AND within 60 days
          died_in_icu == TRUE & icu_days < 61 ~ TRUE,
          TRUE ~ FALSE
        ),
        # surv_icu_status = dplyr::case_when(
        #   # If survived: censored 
        #   PatientDied == 0 ~ 0, 
        #   # If discharged within 60 day: censored
        #   ICUEvent == 1 ~ 0, 

        #   # Note: Now, only patients left that died and not discharge from icu within 60 days
        #   # Of these, if discharge date is NA, they have died in ICU:
        #   is.na(icuDischargeDate) ~ 1, 
        #   # There are some patient without NA in discharge date. That is, they have been discharged, but ICUEvent is NA
        #   # It does look like these are cases where patient were certain to die and discharged of ICU for visiting reasons or something?
        #   # Weird that ICUEvent is not 1 here..
        #   # TODO: Check with other: How to procede? Should we set ICUEvent to 1 where there has been icu discharge? 
        #   !is.na(icuDischargeDate) ~ 2 
        # ),
        # Date of beginning of MV is NA if prior to icu MV already applied. 
        # Following previous code, set date of MV to admission date then

        mv_start_date = dplyr::if_else(has_mv_before_icu == 1, icu_admission_date, mv_start_date),

        # TODO: Some ppl do not have a ventDisTime, but a MechVent time. Is this censored bc at end of study still MV? 
          # But weird bc MV started 2007
          # Check: > patient |> dplyr::filter(MechVentPrior == 1) |> dplyr::select(AdmissionDate, MechVent, ventDisTime) |> dplyr::filter(is.na(ventDisTime))
          # Ppl who never were MV: table(is.na(patient$MechVent)) --> Those with no date at all
        mv_duration = difftime(mv_discontinued_date, mv_start_date, units = "days"),
        # TODO: Check if duration is set to 0 if NA 
        # mvDuration = dplyr::if_else(is.na(mvDuration), .0, mvDuration),
        mv_duration_calender_days= floor(mv_duration)
    ) 

# Check if MV duration is correct:
# TODO: Discuss this approach with the others! 
patient |> 
  dplyr::mutate(issue = mv_duration_calender_days > icu_calender_days) |> 
  ggplot() + 
  geom_point(aes(x = mv_duration_calender_days, y = icu_calender_days, color = issue)) 
# Problem: ppl may be longer ventilated than they are on icu. 
# Solution? Set ventDisTime to Discharge date and set vent days to icu days? 
patient |> 
  dplyr::mutate(issue = mv_duration_calender_days > icu_calender_days) |> 
  dplyr::filter(issue == T) |>  
  dplyr::select(icu_admission_date, mv_start_date, icu_discharge_date, mv_discontinued_date, icu_calender_days, mv_duration) |> 
  View()

# Check how many missing events exist, i.e. for how many ppl we neither have a date of icu discharge nor a time of death
patient |> 
  dplyr::filter(is.na(death_date), is.na(icu_discharge_date)) |> 
  nrow()
# --> We have 1163 observations where patient has neither been discharge nor died
# NOTE: Old data set was 1117. This should be due to different filter methods. 
# TODO: Check when time

# For all, we find a missing discharge date, however, we know that ALL survived. ---> TODO: What does that even mean??
# TODO: Move this logic into the mutate logic from above after fully understanding it first tho
patient = patient |> 
  dplyr::mutate(
    icu_days = dplyr::if_else(is.na(death_date) & is.na(icu_discharge_date), lubridate::ddays(61), icu_days) # 61 bc we censor after 60 days
  )  

#check
table(patient$has_died, patient$has_died_icu_status)
# 1248 patients died but were released from icu prior to death -> status = 1
# 3771 patients died while at icu -> status 2
# 5 patients who died but after 60 days -> admistrative censoring

# TODO: No idea what previous logic does. lol. 
# There should not be any events (deah in icu) if patient does not die. This is not the case for our data, but old data seems to have it? 
# We now have 24 patient that were censored due to 60 day limit (insead of previous 5):

# Final cleaning or trafos
# TODO: Filter if necessary:
patient |> 
  dplyr::filter(apache_score > 71) |> 
  add_to_be_removed(
      df_base = removed_patient, 
      cause = "unrealistic", 
      comment = "ApachgeIIScore > 71"
  )

# TODO: Fix encoding:
# patient$AdmCatID <- as.factor(patient$AdmCatID)
# patient$Gender <- as.factor(patient$Gender)
# patient$AdCens <- as.factor(patient$AdCens)
# patient$AdmCatID <- as.character(patient$AdmCatID)
# patient$AdmCatID[patient$AdmCatID == "1"] <- "Medical"
# patient$AdmCatID[patient$AdmCatID == "2"] <- "Surgical/Elective"
# patient$AdmCatID[patient$AdmCatID == "3"] <- "Surgical/Emeregency"
# patient$AdmCatID <- as.factor(patient$AdmCatID)
# patient$DiagID2 <- relevel(patient$DiagID2, ref = "Other")
# patient$Gender <- as.character(patient$Gender)
# patient$Gender[patient$Gender == "1"] <- "Male"
# patient$Gender[patient$Gender == "2"] <- "Female"
# patient$Gender <- as.factor(patient$Gender)
# patient$Year <- factor(as.character(patient$Year))
# patient$Year <- relevel(patient$Year, ref = "2007")
# patient$CombinedicuID <- as.factor(patient$CombinedicuID)

# Set parameters for exclusion
# parameters regarding time scale of survival (days after ICU admission)
mindays.surv  = 5 # minimal number of days after ICU admission survived
mindays.icu   = 4 # minimal number of days spent on ICU after ICU admission
## apply following exclusion criteria
# - survival time shorter than 4 days or discharged alive from ICU within 4 days
# - patients that have not reached age of at least 18

# Filter those who did not spend at least 4 days in icu
removed_patient = patient |> 
  dplyr::filter(icu_calender_days < 4) |> 
  add_to_be_removed(
      df_base = removed_patient, 
      cause = "excluded", 
      comment = "number of days in icu below 4 days"
  )

# Filter those who died within the first 5 days of icu admission
# i.e. Who died AND did not spend at least 5 days in icu
removed_patient = patient |> 
  dplyr::filter(has_died == TRUE) |> 
  dplyr::filter(icu_calender_days < 5) |> 
  add_to_be_removed(
      df_base = removed_patient, 
      cause = "excluded", 
      comment = "number of days in icu below 4 days"
  )

# Filter under age patients (none)
patient |> 
  dplyr::filter(age < 18)

# Remove: 
patient = patient |> 
    dplyr::filter(!combined_id %in% removed_patient$combined_id) 

saveRDS(patient, file = "consulting05-2025-mv/data/patient.Rds")
saveRDS(removed_patient, file = "consulting05-2025-mv/data/removed_patient.Rds")
