
names(df_Q2)

### module 1
names(df_age)

df_sex <- data.frame(df_gender, df_sex, df_relship)
#remove(df_sexual)
names(df_sex)

names(df_race)

names(df_citizen)

df_socioeco <- data.frame(df_fincur, df_finpast, df_educ)
names(df_socioeco)

#remove(df_religios)
names(df_relig)

df_academic <- data.frame(df_degree, df_transfer, df_yr, df_enroll, df_field, df_gpa, df_aca, df_timeclass, df_timestud, df_persist)
names(df_academic)

names(df_residenc)

names(df_activ)

names(df_military)

names(df_disab)

##########
names(df_Q3)

### modele 2

#remove(df_diener1, df_diener2, df_diener3, df_diener4, df_diener5, df_diener6, df_diener7, df_diener8)
names(df_diener)

df_depress <- data.frame(df_phq9, df_deprawsc, df_dep$dep_impa, df_phq2)
names(df_depress)[11] <- "dep_impa"
names(df_depress)

df_anxiety <- data.frame(df_gad7, df_anx$anx_score)
names(df_anxiety)[9] <- "anx_score"
names(df_anxiety)

df_eating <- data.frame(df_thin, df_body, df_height, df_weight, df_scoff, df_ed$ed_scoff)
names(df_eating)[11] <- "ed_scoff"
names(df_eating)

names(df_sib)

names(df_sui)

df_violence <- data.frame(df_violence, df_assault)
names(df_violence)

df_use <- data.frame(df_alc, df_binge, df_smok, df_drug)
names(df_use)

names(df_sleep)

##########
names(df_Q4)

### module 3

names(df_dx)

names(df_knowwher)

df_help <- data.frame(df_med$med_help, df_ther$ther_help)
names(df_help) <- c("med_help", "ther_help")
names(df_help)

df_stigma <- data.frame(df_stig$stig_pcv_1, df_stig$stig_per_1)
names(df_stigma) <- c("stig_pcv_1", "stig_per_1")
names(df_stigma)

names(df_percneed)

names(df_talk1)

df_use_ct <- data.frame(df_ther$ther_any, df_ther$ther_vis, df_ther$ther_cur, df_prov)
names(df_use_ct)[1:3] <- c("ther_any", "ther_vis", "ther_cur")
names(df_use_ct)

df_satisf <- data.frame(df_sat, df_ther$ther_helped_me)
names(df_satisf)[length(df_satisf)] <- "ther_helped_me"
names(df_satisf)

df_medic <- data.frame(df_meds, df_why)
names(df_medic)

names(df_bar)

names(df_anyprovi)

df_informal <- data.frame(df_inf, df_talk2, df_talkaca, df_talksup)
names(df_informal)

names(df_ins)

##########
names(df_Q10)

### module 9

df_know_mit <- data.frame(df_know$know_sp, df_dep[,3:10], df_anx[,2:5], df_ea, df_gk)
names(df_know_mit)[1] <- "know_sp"
names(df_know_mit)

df_know_cs <- data.frame(df_know$knowwher, df_outreach, df_heard, df_camp)
names(df_know_cs)[1] <- "knowwher"
names(df_know_cs)

df_belief <- data.frame(df_meds$meds_help_me, df_ther$ther_help_me)
names(df_belief) <- c("meds_help_me", "ther_help_me")
names(df_belief)

df_identity <- data.frame(df_self, df_dep$dep_secret, df_men)
names(df_identity)[2] <- "dep_secret"
names(df_identity)


df_stig_pcv <- data.frame(df_stig$stig_pcv_2, df_stig$stig_pcv_3, df_stig$stig_pcv_1)
names(df_stig_pcv) <- c("stig_pcv_2", "stig_pcv_3", "stig_pcv_1")
names(df_stig_pcv)

df_stig_per <- data.frame(df_stig$stig_per_2, df_stig$stig_per_3, df_stig$stig_per_1)
names(df_stig_per) <- c("stig_per_2", "stig_per_3", "stig_per_1")
names(df_stig_per)

names(df_txfrf)

##########
names(df_Q12)

### module 11

df_belong <- data.frame(df_belong1, df_belong2, df_belong3, df_belong4, df_belong5, df_belong6, df_belong7, df_belong8, df_belong9)
names(df_belong)

df_camp_cli <- data.frame(df_mh, df_cli, df_st$st_promote, df_admin, df_env)
names(df_camp_cli)[4] <- "st_promote"
names(df_camp_cli)

names(df_safe)

df_diverse <- data.frame(df_divers, df_discrim)
names(df_diverse)

##########
names(df_Boston)

save.image("split_data.RData")
