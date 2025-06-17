# Libraries ---
library(ggplot2)

# Parameters ---
sas_data_path = "sasdata/"
sas_encoding = "latin1"
out_data_path = "consulting05-2025-mv/data/"
sistersite_codes = list(
    "1" = "Canada",
    "2" = "Austrialia and New Zealand",
    "3" = "USA",
    "4" = "Europe",
    "5" = "Latin America",
    "6" = "Asia"
)

# Functions ---
add_to_be_removed = function(df_to_add, df_base, cause, comment = NULL){
    return(
        rbind(
            df_base,
            df_to_add |> 
                dplyr::select(CombinedID) |> 
                dplyr::mutate(
                    cause = cause,
                    comment = comment
                )
        )
    )
}

# Note:
# icuid: Unique ICU registration identifier. Same ICU will have different values on different years
# combined_icu_id: ICU identifier that were same ICU has same values across years
# sister_side: Indentifies geographic region of ICU (1. Canada, 2.Austrialia and New Zealand, 3. USA, 4.Europe, 5.Latin America and 5.Asia)

# ICU seems less interesting, but maybe analysis by icuid interesting? 
# Note: City names not properly read
icu = haven::read_sas(paste0(sas_data_path, "icuinfo.sas7bdat"), encoding = sas_encoding) |> 
    dplyr::mutate(
        sistersite_cntry = as.factor(sistersite_codes[sistersite] |> unlist() |> unname())
    )

# # Correct types ---
# # lookup_list = NULL --> Leave variable itself unchanged, but change type
# triplets = list(
#     list(col = "icuid", type = "factor", lookup_list = NULL),
#     list(col = "combinedicuid", type = "factor", lookup_list = NULL),
#     list(col = "CombinedID", type = "factor", lookup_list = NULL),
#     list(col = "sistersite", type = "factor", lookup_list = NULL),
#     list(col = "sex", type = "factor", lookup_list = list("1" = "male", "2" = "female")),
#     list(col = "mv_start_yn", type = "factor", lookup_list = list("1" = TRUE, "0" = FALSE)),
#     list(col = "admission_type", type = "factor", lookup_list = list("1" = "medical", "2" = "surgical elective", "3" = "surgical emergency")), # TODO Change to: 1 - Medical, 2 - Surgical Elective, 3 - Surgical Emergency
#     list(col = "DiagID", type = "factor", lookup_list = NULL), # Primary ICU diagnosis. Not sure what values are possible... bzw. to what they correspond
#     list(col = "comorbidities_yn", type = "logical", lookup_list = NULL), # Comorbidities = simultaneous presence of two or more medical conditions or disorders in the same individual; TODO: Change to TRUE and FALSE
#     list(col = "glucose_units", type = "factor", lookup_list = NULL), # Measurement units. Use factor to make sure there are only a certain number of units allowed
#     list(col = "blood_sugar_yn", type = "logical", lookup_list = NULL), # TODO change to T and F
#     list(col = "apache_na___1", type = "logical", lookup_list = NULL), # TODO change to T and F
#     list(col = "sofa_vasopressors_yn", type = "logical", lookup_list = NULL), # TODO change to T and F
#     list(col = "sofa_gcs_yn", type = "logical", lookup_list = NULL), # TODO change to T and F
#     list(col = "sofa_score_complete", type = "logical", lookup_list = NULL), # TODO change to T and F
#     list(col = "weight_type_oth", type = "factor", lookup_list = NULL), 
#     list(col = "weight_type", type = "factor", lookup_list = NULL), 
#     list(col = "energy_req_yn", type = "logical", lookup_list = NULL) # Change to T and F
# )
# for (triplet in triplets){
#     entries = patient[, triplet$col] |> unlist()
#     browser()
#     if (triplet$type == "factor") patient[, triplet$col] = as.factor(entries, order = triplet$lookup_list)
#     else if (triplet$type == "logical") patient[, triplet$col]  = as.logical(entries)
# }


# com_myocardial___? = "logical", # TODO Change to TRUE and FALSE __or__ merge into one column; check attr(*, "label") to see what choice
# list(col = "com_myocardial", type = "multifactor", lookup_list = NULL)
# # Grouping multiple columns into new cols
# patient |> dplyr::select(contains("com_myocardial")) |> names()
# patient |> dplyr::select(contains("com_myocardial")) |> rowSums() |> table() # Multiple states possible

# # 
# com_neurologic___? = "logical", # TODO same as above
# patient |> dplyr::select(contains("com_neurologic")) |> names()
# patient |> dplyr::select(contains("com_neurologic")) |> rowSums() |> table()

# com_vascular___? = "logical", # TODO same as above
# com_pulmonary___? = "logical", # TODO same as above
# com_endocrine = "logical",# TODO same as above
# com_endocrine___? = "logical", # TODO same as above
# com_renal___? = "logical", 
# com_gastrointestinal___? = "logical", # TODO same as above
# com_cancer_immune___?  = "logical", # TODO same as above
# com_psychological___? = "logical", # TODO same as above
# com_muskoskeletal___? = "logical", # TODO same as above
# com_substance___? = "logical", # TODO same as above
# com_miscellaneous___? = "logical", # TODO same as above
# # assess_dt and assess_tm to datetime object; Note: Some have NA in either entry


patient = haven::read_sas(paste0(sas_data_path, "patientinfo.sas7bdat"), encoding = sas_encoding) 
    # Fix Formatting:

# daily = haven::read_sas(paste0(sas_data_path, "obsinfo.sas7bdat"), encoding = sas_encoding)

# PN seems giga useless
# pn = haven::read_sas(paste0(sas_data_path, "pn_lipids_info.sas7bdat"), encoding = sas_encoding)

# Difference patient and daily
# -> Daily is more focused on nutrition
patient |> 
    # Obersavtions per icuid
    dplyr::summarise(.by = c(year, icuid), n = dplyr::n())  |> 
    # Check how often icuid is contained (i.e. does icuid appear in two or more different years)
    dplyr::summarise(.by = icuid, n = dplyr::n())  |> 
    dplyr::summarise(.by = n, dplyr::n())

patient |> 
    # Only if mv_start_yn = 0, MV has been started in icu
    dplyr::filter(mv_start_yn == 0) |> 
    dplyr::select(mvstartdt) |> 
    dplyr::mutate(isna = is.na(mvstartdt)) |> 
    dplyr::summarise(.by = isna, n = dplyr::n())
    # That is:
    # All patients that have not received MV prior to ICU have been MV'ed after entering ICU

# For now, focus on ppl who entered icu without already being MV'ed
# mv_in_icu = patient |> dplyr::filter(mv_start_yn == 0)
# nrow(mv_in_icu)

# names(mv_in_icu)
# # Geographical data distribution
# mv_in_icu = mv_in_icu |> 
#     dplyr::left_join(
#         icu |> dplyr::select(icuid, sistersite_cntry),
#         by = "icuid"
#     ) |> 
#     dplyr::rename(country = sistersite_cntry)
# table(mv_in_icu$country)
# mv_in_icu |> 
#     dplyr::summarise(
#         n = dplyr::n(), 
#         .by = c(country, sex)
#     )

################################### PATIENT ####################################
# Following fully based on preproc-data Script. 
# NOTE: This is not a general pipeline.
    # That is, e.g. I removed parts of the sanity checks where the original code could not find any cases. 
#### rename variables - For now. The names are terrible, but used for now to re-create the filter in the preproc-data.R script
patient.vars.old <- c(
  "year"             , "combinedicuid" ,
  "CombinedID",
  "study_id"         , "sex"           ,
  "age"              ,
  "mv_start_yn"      , "admission_type",
  "DiagID"           ,
  "apache_score"     , "height"        ,
  "weight"           , "bmi"           ,
  "calsperkg"        , "protperkg",
  "patientdied"      ,
  "pt_die_icu_yn"    ,
  "mv_discontinued_icu_yn"
  )

patient.vars.new <- c(
  "Year"             , "CombinedicuID"     ,
  "CombinedID_old",
  "CombinedID"       , "Gender"            ,
  "Age"              ,
  "MechVentPrior"    , "AdmCatID"          ,
  "DiagID"           ,
  "ApacheIIScore"    , "Height"            ,
  "Weight"           , "BMI"               ,
  "Calories"         , "Protein"           ,
  "PatientDied"      ,
  "icuDischargeDeath",
  "VentDisICU"
  )

# Another vector of names for time dependence variables. lol. 
td.vars.old <- c(
  "mvstartdt"         , "endt"              , "pndt"             ,
  "hospadmindt"       , "icuadmindt"        , "deathdt"          ,
  "icudischargedt"    , "hospdischargedt"   , "mvdiscontinuedt")

td_vars_new <- c(
  "MechVent"          , "enTime"            , "pnTime"           ,
  "HospAdmissionDate" , "AdmissionDate"     , "PatientDeathTime" ,
  "icuDischargeDate"  , "HospDischargeDate" , "ventDisTime")

vars.to.rename <- c(patient.vars.old, td.vars.old)
patient.td_vars_new <- c(patient.vars.new, td_vars_new)

## first subset patient data
# TODO: This subsetting seems giga ranom. Are variables left out that could be interesting?
# patient <- patient[, vars.to.rename]

# rename to fit old script, via loop since ordering is crucial
for(i in seq_along(vars.to.rename) ) {
  ind <- which(colnames(patient) == vars.to.rename[i])
  colnames(patient)[ind] <- patient.td_vars_new[i]
}

lct <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")

# check integrity of variables containing date information
sapply(td_vars_new, function(x) {
  tmp  <- patient[[x]]
  tmp2 <- as.POSIXct(tmp, tz="CET")
  if(any(is.na(tmp2)!= is.na(tmp))) browser()
  patient[[x]] <<- tmp2
  return(NULL)
})

Sys.setlocale("LC_TIME", lct)

patient$CombinedID <- as.integer(gsub("_", "", patient$CombinedID))

# Sanity Checks ---
## Patient - Data Set
# Initially: 21_100 rows = patients
removed_patient = tibble::tibble(
    CombinedID= character(),
    cause = character(),
    comment = character()
)
# TODO: Check if there are duplicates in removed_patient i.e. multiple reasons for a patient to be removed. Curious.
# Remove Patients with td_vars_new incosistencies 
# 1) MV discontinuation before or equal to MV start
removed_patient = patient |> 
    # dplyr::filter(MechVent >= ventDisTime, !is.na(MechVent >= ventDisTime))
    dplyr::filter(MechVent >= ventDisTime) |> 
    add_to_be_removed(
        df_base = removed_patient, 
        cause = "inconsistent", 
        comment = "mv discontinuation before or equal to mv start"
    )

# 2) ICU admission before hospital admission
# -> 4 patient which we just remove
removed_patient = patient |> 
    dplyr::filter(AdmissionDate < HospAdmissionDate) |> 
    add_to_be_removed(
        df_base = removed_patient, 
        cause = "inconsistent", 
        comment = "icu admission before hospital admission"
    )

# 3) hospital discharge before icu discharge?
# 5 patients discharged from hospital before being discharged from icu.
# 2 seem to be just inaccurateley timed in the system. Keep them.
patient |> 
    dplyr::filter(HospDischargeDate < icuDischargeDate) |> 
    dplyr::select(CombinedID, HospDischargeDate, icuDischargeDate) |> 
    dplyr::mutate(time_diff = icuDischargeDate - HospDischargeDate) 

removed_patient = patient |> 
    dplyr::filter(HospDischargeDate < icuDischargeDate) |> 
    dplyr::select(CombinedID, HospDischargeDate, icuDischargeDate) |> 
    dplyr::mutate(time_diff = icuDischargeDate - HospDischargeDate) |> 
    dplyr::filter(time_diff > 3) |> 
    add_to_be_removed(
        df_base = removed_patient, 
        cause = "inconsistent", 
        comment = "hospital discharge before icu discharge"
    )

# 4) MV discontinuation before admission?
removed_patient = patient |> 
    dplyr::filter(ventDisTime < AdmissionDate) |> 
    add_to_be_removed(
        df_base = removed_patient, 
        cause = "inconsistent", 
        comment = "mv discontinued before admission"
    )

# 5) Remove patients with missing indication of death/survival
patient |> 
    dplyr::filter(is.na(PatientDied))

# 6) Missing BMI
# TODO: Apply if necessary 
patient |> 
    dplyr::filter(is.na(BMI))

# 7) Missing AII Score
# TODO: Apply if necessary
patient |> 
    dplyr::filter(is.na(ApacheIIScore))

# 8) Missing Sex (only 3 patient have missing sex. Just remove them.)
removed_patient = patient |> 
    dplyr::filter(is.na(Gender)) |> 
    add_to_be_removed(
        df_base = removed_patient, 
        cause = "incomplete", 
        comment = "missing gender"
    )

### End Sanity Checks
# Filter all patients to be removed:
patient = patient |> 
    dplyr::filter(!CombinedID %in% removed_patient$CombinedID) 


# Fix data encoding
patient = patient |> 
    dplyr::mutate(
        DiagID = as.factor(
            dplyr::case_when(
                # Mapping comes from old code; line 385ff
                DiagID %in% c(1:9, 50:54, 52.1, 52.2) ~ "cardio-vascular", 
                DiagID %in% c(10:19, 57:58) ~ "respiratory",
                DiagID %in% c(21:27, 61:68) ~ "gastrointestinal",
                DiagID %in% c(29:35, 70:74) ~ "neurologic",
                DiagID %in% c(37, 38, 82:85) ~ "sepsis",
                DiagID %in% c(39, 40, 76, 77, 81) ~ "orthopedic/trauma",
                DiagID %in% c(41:43) ~ "metabolic",
                DiagID %in% c(47, 78) ~ "renal",
                TRUE ~ "other"
            )
        )
    ) 

# Redefine diagnosis variable according to report docu
patient = patient |> 
    dplyr::mutate(
        # Calculate survival time:
            # ! Only available if patient died
        # We define discharge from hospital as the LATEST measured discharge event
        # in case there were discrepancies. However discharge from hospital must
        # have been tracked.
        Surv0To60 = PatientDeathTime - AdmissionDate, # NOTE: Admission date is date of admission IN ICU!! 
        # Calculate discharge time:
            # This is discharge FROM ICU! 
            # Note: Patient may have died but have been discharge from ICU prior to their death
        Disc0To60 = icuDischargeDate - AdmissionDate,
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
        diedInIcu = is.na(icuDischargeDate), # If NA, then Person died in icu --> no discharge date
        icudays = dplyr::if_else(diedInIcu == TRUE, PatientDeathTime - AdmissionDate, icuDischargeDate - AdmissionDate),
        icuCalenderdays = floor(icudays), 
        surv_icu0to60 = icudays, 
        # Alternative already contained in data:
        # surv_icu0to60 = ICUDAYS,  # already has 60+ transformed to 61 (i.e. loss of info. Note sure if I like)
        # Compare information: test |> dplyr::select(ICUDAYS, icudays, diedInIcu) |> dplyr::mutate(diff = ICUDAYS - icudays) |> ggplot() + geom_point(aes(x = ICUDAYS, y = icudays))

        surv_icu_status = dplyr::case_when(
          # Only observe event if patient died within icu AND within 60 days
          diedInIcu == TRUE & icudays < 61 ~ 1,
          TRUE ~ 0
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
        # )
        # Date of beginning of MV is NA if prior to icu MV already applied. 
        # Following previous code, set date of MV to admission date then

        MechVent = dplyr::if_else(MechVentPrior == 1, AdmissionDate, MechVent),

        # TODO: Some ppl do not have a ventDisTime, but a MechVent time. Is this censored bc at end of study still MV? 
          # But weird bc MV started 2007
          # Check: > patient |> dplyr::filter(MechVentPrior == 1) |> dplyr::select(AdmissionDate, MechVent, ventDisTime) |> dplyr::filter(is.na(ventDisTime))
          # Ppl who never were MV: table(is.na(patient$MechVent)) --> Those with no date at all
        mvDuration = difftime(ventDisTime, MechVent, units = "days"),
        mvDurationCalenderdays = floor(mvDuration)
    ) 

# Check if MV duration is correct:
# TODO: Discuss this approach with the others! 
patient |> 
  dplyr::mutate(issue = mvDurationCalenderdays > icuCalenderdays) |> 
  ggplot() + 
  geom_point(aes(x = mvDurationCalenderdays, y = icuCalenderdays, color = issue)) 
# Problem: ppl may be longer ventilated than they are on icu. 
# Solution? Set ventDisTime to Discharge date and set vent days to icu days? 
patient |> 
  dplyr::mutate(issue = mvDurationCalenderdays > icuCalenderdays) |> 
  dplyr::filter(issue == T) |>  
  dplyr::select(AdmissionDate, MechVent, icuDischargeDate, ventDisTime, icuCalenderdays, mvDurationCalenderdays) |> 
  View()

# Check how many missing events exist:
patient |> 
  dplyr::filter(is.na(Surv0To60), is.na(Disc0To60)) |> 
  nrow()
# --> We have 1163 observations where patient has neither been discharge nor died
# NOTE: Old data set was 1117. This should be due to different filter methods. 
# TODO: Check when time

# For all, we find a missing discharge date, however, we know that ALL survived. ---> TODO: What does that even mean??
patient |> 
  dplyr::filter(is.na(Surv0To60), is.na(Disc0To60)) |> 
  dplyr::select(icuDischargeDate) |> 
  summary()

# ### We use last_date_icu for these missing ones (all receive a 61L).
# patient$Disc0To60[is.na(s0to60) & is.na(d0to60)] <- 61L
# patient$AdCens <- ifelse(is.na(s0to60) & is.na(d0to60), 1, 0)
# ### if we measured longer survival /discharge, we censor it to 61L.
# ### We leave an indicator that we already censored these values.
# ### (assumed to have survived, Note that we do not make further use of the
# ### indicator.)
# patient$Disc0To60[patient$Surv0To60 > 61 & (!is.na(patient$Surv0To60))] <- 61L
# patient$Surv0To60[patient$Surv0To60 > 61 & (!is.na(patient$Surv0To60))] <- 61L
# patient$PatientDied[patient$Surv0To60 > 61 & (!is.na(patient$Surv0To60))] <- 0L
# patient$Disc0To60[patient$Disc0To60 > 61 & (!is.na(patient$Disc0To60))] <- 61L
# patient$PatientDischarged <- ifelse(patient$PatientDied == 0 & patient$Disc0To60 < 61L & !is.na(patient$Disc0To60), 1, 0)

#check
table(patient$PatientDied, patient$surv_icu_status)
# 1248 patients died but were released from icu prior to death -> status = 1
# 3771 patients died while at icu -> status 2
# 5 patients who died but after 60 days -> admistrative censoring

# TODO: No idea what previous logic does. lol. 
# There should not be any events (deah in icu) if patient does not die. This is not the case for our data, but old data seems to have it? 
with(patient, table(surv_icu_status, PatientDied))
# We now have 24 patient that were censored due to 60 day limit (insead of previous 5):
patient |> 
  dplyr::filter(surv_icu_status == 0, icudays >= 61) |> dplyr::select(icudays) |> 
  nrow()

### Compute days in ICU
# First create a variable that give last date time in ICU
# TODO: Discuss: I think what the following code replaces days in icu where it could not be calculated
  # That should be the case if date of death or day of icu discharge is missing
  # i.e. the person is still on the icu when study ended, but has not died yet.
  # The fuck is the following doing? Are they setting the duration to 61 days? That does not make sense...
# last_date_icu <- patient$icuDischargeDate
# last_date_icu[is.na(last_date_icu) & patient$PatientDied == 1] <-
#   patient$PatientDeathTime[is.na(last_date_icu) & patient$PatientDied == 1]

# last_date_icu[is.na(last_date_icu)] <-
#   patient$AdmissionDate[is.na(last_date_icu)] + days(61L)
# patient$last_date_icu <- last_date_icu
# patient$DaysInICU <-
#   as.numeric(with(patient,
#                   difftime(last_date_icu, AdmissionDate, units = "days")))
# # check variable
# checkmate::assert_numeric(patient$icudays, lower = 0, min.len = 1, any.missing = FALSE,
#   finite = TRUE)


# TODO: Discuss: What does old code to with MV? 


# Final cleaning or trafos
# TODO: Filter if necessary:
patient |> 
  dplyr::filter(ApacheIIScore > 71) |> 
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
  dplyr::filter(icuCalenderdays < 4) |> 
  add_to_be_removed(
      df_base = removed_patient, 
      cause = "excluded", 
      comment = "number of days in icu below 4 days"
  )

# Filter those who died within the first 5 days of icu admission
# i.e. Who died AND did not spend at least 5 days in icu
removed_patient = patient |> 
  dplyr::filter(PatientDied == 1) |> 
  dplyr::filter(icuCalenderdays < 5)
  add_to_be_removed(
      df_base = removed_patient, 
      cause = "excluded", 
      comment = "number of days in icu below 4 days"
  )

# Filter under age patients (none)
patient |> 
  dplyr::filter(Age < 18)

# Remove: 
patient = patient |> 
    dplyr::filter(!CombinedID %in% removed_patient$CombinedID) 

saveRDS(patient, file = "consulting05-2025-mv/data/patient.Rds")
saveRDS(removed_patient, file = "consulting05-2025-mv/data/removed_patient.Rds")


