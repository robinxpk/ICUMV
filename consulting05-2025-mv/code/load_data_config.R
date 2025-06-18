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
primary_diagnosis_codes = c(
  setNames(rep("cardio-vascular",  length(c(1:9, 50:54, 52.1, 52.2))), as.character(c(1:9, 50:54, 52.1, 52.2))),
  setNames(rep("respiratory",      length(c(10:19, 57:58))),            as.character(c(10:19, 57:58))),
  setNames(rep("gastrointestinal", length(c(21:27, 61:68))),            as.character(c(21:27, 61:68))),
  setNames(rep("neurologic",       length(c(29:35, 70:74))),            as.character(c(29:35, 70:74))),
  setNames(rep("sepsis",           length(c(37, 38, 82:85))),           as.character(c(37, 38, 82:85))),
  setNames(rep("orthopedic/trauma",length(c(39, 40, 76, 77, 81))),      as.character(c(39, 40, 76, 77, 81))),
  setNames(rep("metabolic",        length(c(41:43))),                   as.character(c(41:43))),
  setNames(rep("renal",            length(c(47, 78))),                  as.character(c(47, 78)))
)

# List to rename columns. 
# Format: oldname = newname
patient_rename_list = list(
    "icuid" = "icu_id",
    "combinedicuid" = "combined_icu_id", 
    "CombinedID" = "combined_id", 
    "hospadmindt" = "hospital_admission_date", 
    "icuadmindt" = "icu_admission_date",
    "mv_start_yn" = "has_mv_before_icu", 
    "mvdiscontinuedt" = "mv_discontinued_date",
    "mvstartdt" = "mv_start_date", 
    "DiagID" = "primary_diagnosis", 
    "comorbidities_yn" = "has_comorbidity",
    "CharlesonCombIndex" = "charleson_comb_index", 
    "FunctionCombIndex" = "function_comb_index",
    "blood_sugar_yn" = "has_blood_sugar",
    "sofa_vasopressors_yn" = "has_sofa_vasopressors",
    "energy_req_yn" = "has_complete_nutrition",
    "sofa_gcs_yn" = "has_sofa_gcs",
    "assess_dt" = "assessment_date",
    "assess_tm" = "assessment_time",
    "energyreqid07" = "energy_req_id___07",
    "energyreqid08" = "energy_req_id___08",
    "energyreqid09" = "energy_req_id___09", 
    "calsperkg" = "cals_per_kg",
    "protperkg" = "protein_per_kg",
    "lost_weight_yn" = "has_lost_weight", 
    "food_decline_yn" = "has_food_decline",
    "endt" = "en_date", 
    "daystoEN" = "days_to_en",
    "pndt" = "pn_date",
    "daystoPN" = "days_to_pn", 
    "icu_day_60_yn" = "has_60_icu_days", 
    "mv_discontinue_loc" = "has_mv_during_transfer",
    "mv_discontinued_hosp_death_yn" = "has_mv_discontinued_prior_hospital_death",
    "mv_discontinued_hosp_yn" = "has_mv_discontinued_in_hospital", 
    "mv_discontinued_icu_death_yn" = "has_mv_discontinued_prior_icu_death",
    "mv_discontinued_icu_yn" = "has_mv_discontinued_in_icu", 
    "deathdt" = "death_date",
    "pt_die_icu_yn" = "has_died_in_icu", 
    "VENTDAYS" = "mv_duration",
    "VENTEvent" = "mv_discontinued_60_days", 
    "ventfreedays" = "mv_free_days_in_first_28_days",
    "ventSinceAdmision" = "mv_since_admission", 
    "DischargedAlive60" = "has_survived_60_days",
    "ICUDAYS" = "icu_days", 
    "icudischargedt" = "icu_discharge_date",
    "ICUEvent" = "has_icu_discharge_in_60_days", 
    "HOSPDAYS" = "hospital_days",
    "hospdischargedt" = "hospital_discharge_date", 
    "HOSPEvent" = "has_hospital_discharge_in_60_days",
    "patientdied" = "has_died", 
    "DaysToHospDischargeAlive" = "time_to_hospital_discharge_alive"
)


# Correct types ---
# lookup_list = NULL --> Leave variable itself unchanged, but change type
patient_encoding_triplets = list(
    list(col = "icu_id", type = "factor", lookup_list = NULL),
    list(col = "combined_icu_id", type = "factor", lookup_list = NULL),
    list(col = "study_id", type = "factor", lookup_list = NULL),
    list(col = "combined_id", type = "factor", lookup_list = NULL),
    list(col = "sistersite", type = "factor", lookup_list = sistersite_codes),
    list(col = "sex", type = "factor", lookup_list = list("1" = "male", "2" = "female")),
    list(col = "has_mv_before_icu", type = "factor", lookup_list = list("1" = TRUE, "0" = FALSE)),
    list(col = "admission_type", type = "factor", lookup_list = list("1" = "medical", "2" = "surgical elective", "3" = "surgical emergency")), 
    list(col = "primary_diagnosis", type = "factor", lookup_list = primary_diagnosis_codes), 
    list(col = "has_comorbidity", type = "logical", lookup_list = list("1" = TRUE, "0" = FALSE)), # Comorbidities = simultaneous presence of two or more medical conditions or disorders in the same individual
    list(col = "has_blood_sugar", type = "logical", lookup_list = list("1" = TRUE, "0" = FALSE)), 
    list(col = "has_sofa_vasopressors", type = "logical", lookup_list = list("1" = TRUE, "0" = FALSE)), 
    list(col = "has_sofa_gcs", type = "logical", lookup_list = list("1" = TRUE, "0" = FALSE)), 
    list(col = "has_complete_nutrition", type = "logical", lookup_list = list("1" = TRUE, "0" = FALSE)) 
)


