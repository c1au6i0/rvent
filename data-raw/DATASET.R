## code to prepare `DATASET` dataset goes here


iox_head <- c(
  "id",
  "number",
  "cpu_date",
  "cpu_time",
  "site_time",
  "period_time",
  "string_type",
  "info",
  "x8",
  "x9",
  "sto_id",
  "first_beat_id",
  "last_beat_id",
  "Ti_msec",
  "Te_msec",
  "PIF_ml_sec",
  "PEF_ml_sec",
  "TV_ml",
  "EV_ml",
  "RT_msec",
  "MV_ml",
  "P_msec",
  "f_bpm",
  "EIP_msec",
  "EEP_msec",
  "Penh",
  "EF50_ml_s",
  "RH_percRH",
  "Tbox_infC",
  "Tbody_infC",
  "Patm_mmHg",
  "VCF",
  "AV_ml",
  "Sr_per",
  "n")


dat_vent <- readRDS(here::here("data-raw", "dat_vent.RDS"))
sess1 <- readRDS(here::here("data-raw", "sess1.RDS"))


usethis::use_data(dat_vent, sess1, iox_head, overwrite = TRUE, internal = TRUE)
