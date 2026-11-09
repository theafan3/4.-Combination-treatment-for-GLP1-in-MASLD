install.packages('tidyverse')
install.packages('tableone')
library(tidyverse)
library(tableone)

# Github同步
{
  # 第一次创建一个同步
  cd /Users/Hong/Desktop/Project2/1. My project/4. Combination treatment for GLP1 in MASLD # can not identify the space so should with ""
  git init # 告诉 Git："从今天开始帮我记录这个项目的历史。"
  touch .gitignore
  nano .gitignore
  将 data/ .DS_Store 写入 告诉 Git："这些文件不要管理。"
  
  git add . # 告诉 Git「我要保存这些修改」。
  git commit -m "what i did today" # 正式保存一个本地历史版本（快照）。
  # 去Github创建一个Repository（只做一次） 告诉Git上传到哪里
  git remote add origin git@github.com:theafan3/4.-Combination-treatment-for-GLP1-in-MASLD.git
  git push -u origin main # 把这些历史版本同步到 GitHub。
} # 第一次生成一个
git status
git add .
git commit -m "今天做了什么"
git push


# hong fan
# Oct 27 2025
# Revision Auguest 11, 2026
#combined data clean####
{
  setwd('/Users/Hong/Desktop/Project2/1. My project/4. Combination treatment for GLP1 in MASLD/data')
  library(tidyverse)
  # data <- read.csv('/home/hongfan1/Desktop/combine/combine.csv', header = TRUE, stringsAsFactors = FALSE)  
  data<-data.table::fread('combine.csv',fill=T,stringsAsFactors=TRUE) # 17378014
  # colnames(data)
  
  for (i in 1:17) {
    colname <- paste0("cci", i)
    data[[colname]][is.na(data[[colname]])] <- 0
  }
  
  data <- data %>% mutate(CCI = rowSums(across(cci1:cci10))+cci11*2 + cci12*2 + cci13*2 + cci14*2 +cci15*3 + cci16*6 + cci17*6)
  data<-data[,c(1:62,80:113)]
  
  # 疾病分组 并计算随访时间
  vars <- c(
    "SLD","diabetes", "HCC", "CVD", "CKD", "nonliver_cancer", "cirrhosis",
    "diabetic_neuropathy", "diabetic_retinopathy", "diabetic_nephropathy",
    "hypertension", "hyperlipidaemia", "COPD", "obesity", "acute_renal_failure",
    "stroke", "heart_failure", "HBV", "HCV", "autoimmune", "ALD", "cryptogenic", "other",
    "smoking", "lt", "dcc_HE", "dcc_gibeed", "dcc_ascites", "dcc_varices",
    "GLP1", "statin", "metformin", "asprin", "insulin", "DPP4", "sulfonylureas",
    "thiazides", "TZD", "SGLT2", "AGI", "ACEI", "ARB", "BB", "CCB"
  )
  
  # 将这些列中的 NA 替换为 0
  data <- data %>% mutate(across(all_of(vars), ~replace_na(.x, 0))) # 17378014
  data <- data[!(is.na(data$GLP1_index) & is.na(data$statin_index)), ] # 6943017
  data$year<-year(data$enroll_start)
  
  masld<-data[which(data$SLD==1),]
  diabetes<-data[which(data$diabetes==1),]
  
  write_excel_csv(masld,file='masld.csv')
  write_excel_csv(diabetes,file='diabetes.csv')
}

#MASLD poulation####
setwd('/Users/Hong/Desktop/Project2/1. My project/4. Combination treatment for GLP1 in MASLD/data')
library(tidyverse)
df<-data.table::fread('masld.csv',fill=T,stringsAsFactors=TRUE) # 1235046

##***GLP1 cohort1####
###纳排####
# 1. 纳入数据库中2010-2023年的所有GLP1首次使用者
df$SLD_index<-as.Date(df$SLD_index)
df$GLP1_index<-as.Date(df$GLP1_index)
df$statin_index<-as.Date(df$statin_index)

df <- df %>% 
  mutate(glp1_year = year(GLP1_index),
                    statin_year = year(statin_index),
                    sld_year = year(SLD_index)) %>% 
  filter (glp1_year>=2010) # 220479

# 2. 剔除过去一年没有医疗记录的人，即没有连续参保记录超过一年的人
# 纳排 连续12个月可见期 
df <- df %>%
  # Step 0. 确保日期类型正确
  mutate(
    enroll_start = as.Date(enroll_start),
    enroll_end   = as.Date(enroll_end),
    GLP1_index   = as.Date(GLP1_index)
  ) %>%
  
  # Step 1. 筛选 index 落在参保期间
  filter(GLP1_index >= enroll_start & GLP1_index <= enroll_end) %>%
  # Step 2. 计算 index 前可观测天数
  mutate(enroll_days_prior = as.numeric(GLP1_index - enroll_start)) %>%
  # Step 3. 保留连续参保 ≥12 个月（365 天）的人
  filter(enroll_days_prior >= 365) %>%
  # Step 4. 如果一个人有多段 enrollment，取 index 所在的那段（通常一人一条）
  group_by(ENROLID) %>%
  slice_max(order_by = enroll_start, n = 1, with_ties = FALSE) %>%
  ungroup() # 172980


# 3. 剔除GLP1 index date时年龄小于18岁的
df <- df %>% 
  mutate(age = glp1_year - DOBYR) %>%
  filter(age >= 18) # 172775

# 4. 剔除过去一年有GLP1和statin药物使用记录者 （这里实际操作中剔除2010年的人，不用在文中强调年份）
# 4. 至此，new users
df <- df %>% filter(glp1_year > 2010) # 168080
df <- df %>% filter(is.na(statin_year) | statin_year > 2010) # 148791

# 5. 剔除在GLP1 index前已经有慢性肝病相关病因，
# 计算所有疾病的基线患病情况，生成对应的baseline的变量
# DCC
df$cirrhosis<-ifelse(df$cirrhosis==1 | df$dcc_ascites==1 | df$dcc_gibeed==1 | df$dcc_HE==1 | df$dcc_varices==1,1,0)
df$dcc<-ifelse(df$dcc_ascites==1 | df$dcc_gibeed==1 | df$dcc_HE==1 | df$dcc_varices==1,1,0)
df$cirrhosis_index <- pmin(df$cirrhosis_index,df$dcc_ascites_index,df$dcc_gibeed_index,df$dcc_HE_index,df$dcc_varices_index,na.rm = TRUE)
df$dcc_index <- pmin(df$dcc_ascites_index,df$dcc_gibeed_index,df$dcc_HE_index,df$dcc_varices_index,na.rm = TRUE)

library(stringr)
df <- df %>%
  mutate(
    # 确保 index 和各疾病 index 都是日期类型
    across(c(GLP1_index, ends_with("_index")), as.Date),
    
    # 针对所有以 "_index" 结尾的列计算是否在 index 之前（含当天）
    across(
      ends_with("_index"),
      ~ if_else(!is.na(.) & (. - GLP1_index) <= 0, 1, 0),
      .names = "{str_replace(.col, '_index', '')}_baseline"
    )
  )

df$meta<-ifelse(df$diabetes_baseline==1 | df$hypertension_baseline==1 | df$hyperlipidaemia_baseline==1 | df$obesity_baseline==1,1,0)
df$meta_count <- rowSums(df[, c("diabetes_baseline","hypertension_baseline","hyperlipidaemia_baseline","obesity_baseline")], na.rm = TRUE)

# 排除SLD病因
nrow(df)
df <- df[df$HBV_baseline != 1, ] # 148434
df <- df[df$HCV_baseline != 1, ] # 147683
df <- df[df$autoimmune_baseline != 1, ] # 147427
df <- df[df$ALD_baseline != 1, ] # 146771
df <- df[df$cryptogenic_baseline != 1, ] # 146174
df <- df[df$other_baseline != 1, ] # 145446

# 6. 剔除SLD诊断在GLP1 index date之后的
df <- df %>% filter(GLP1_index >=SLD_index) # 89289   # 没有排除结局 105603

# 7. 剔除statin使用在GLP1之前的（避免病人同时进入 statin base cohort）
df <- df %>% filter(is.na(statin_index) | statin_index >= GLP1_index) # 42045   # 没有排除结局 47406
df$glp1_year_group<-ifelse(df$glp1_year<2015,1,ifelse(df$glp1_year>=2015 & df$glp1_year<2019,2,ifelse(df$glp1_year>=2019,3,NA)))

# # 8. 排除结局相关 生成用于分析的亚组
# df <- df[df$cirrhosis_baseline != 1, ] # 139383
# df <- df[df$dcc_baseline != 1, ] # 139383
# # df <- df[df$dcc_HE_baseline != 1, ]
# # df <- df[df$dcc_gibeed_baseline != 1, ]
# # df <- df[df$dcc_ascites_baseline != 1, ]
# # df <- df[df$dcc_varices_baseline != 1, ]
# df <- df[df$lt_baseline != 1, ] # 139341
# df <- df[df$HCC_baseline != 1, ] # 139316
# df <- df[df$CVD_baseline != 1, ] # 135762
# df <- df[df$CKD_baseline != 1, ] # 131581
# df <- df[df$nonliver_cancer_baseline != 1, ] # 125948

# # MASLD？
# df$meta<-ifelse(df$diabetes_baseline==1 | df$hypertension_baseline==1 | df$hyperlipidaemia_baseline==1 | df$obesity_baseline==1,1,0)
# df$MASLD<-ifelse(df$meta==1 & df$HBV_baseline==0 & df$HCV_baseline==0 & df$autoimmune_baseline==0 & df$other_baseline==0 & df$ALD_baseline==0, 1, 0) ## 30721  8610 (pdff)
# df<-df[which(df$MASLD==1),] # 194728

###暴露、匹配####
## GLP1 cohort (GLP1 -> add-on statin within 1 year)
## Time-updated covariates + 30-day risk-set PS matching
## (Assumes df is already eligibility-filtered and has dcc/dcc_index created)
library(dplyr)
library(lubridate)
library(MatchIt)
library(survival)
library(tidyr)
library(rlang)
library(readr)
library(broom)

flag_before <- function(index_date, assess_date) {
  if_else(!is.na(index_date) & index_date <= assess_date, 1L, 0L)
}

## 1) Pre-processing (GLP1 as background)
df <- df %>%
  mutate(
    enroll_end = as.Date(enroll_end),
    study_end  = as.Date("2023-12-31")
  ) %>%
  mutate(
    obs_end_date = pmin(enroll_end, study_end, na.rm = TRUE),
    obs_days     = as.numeric(obs_end_date - GLP1_index),
    glp1_to_statin_days = as.numeric(statin_index - GLP1_index)
  )

## 2) Exposure definition (add-on within 1 year)
df <- df %>%
  mutate(
    ever_combo = if_else(!is.na(statin_index) & glp1_to_statin_days >= 0, 1L, 0L),
    addon_1y   = if_else(ever_combo == 1L & glp1_to_statin_days <= 365, 1L, 0L)
  )

## Analysis pool:
## - treated candidates: addon_1y==1
## - controls: ever_combo==0 (never statin during follow-up) 修改后不要这样了
df_ana <- df

## 3) PS covariates (time-updated)
ps_covars_tv <- c(
  "SEX","age","REGION","CCI","glp1_year_group","meta_count",
  "smoking_tv",
  "SLD_tv","diabetes_tv","diabetic_neuropathy_tv","diabetic_retinopathy_tv","diabetic_nephropathy_tv",
  "hypertension_tv","hyperlipidaemia_tv","obesity_tv","acute_renal_failure_tv","stroke_tv","heart_failure_tv",
  "HBV_tv","HCV_tv","autoimmune_tv","ALD_tv","cryptogenic_tv","other_tv",
  "metformin_tv","asprin_tv","insulin_tv","DPP4_tv","sulfonylureas_tv","thiazides_tv","TZD_tv","SGLT2_tv","AGI_tv",
  "ACEI_tv","ARB_tv","BB_tv","CCB_tv",
  "cirrhosis_tv","dcc_tv","lt_tv","HCC_tv","CVD_tv","CKD_tv","nonliver_cancer_tv"
)

# 没有显著性的也画KM

ps_formula_tv <- as.formula(paste("treated_window ~", paste(ps_covars_tv, collapse = " + ")))

## 4) Time-conditional PS matching in 30-day windows
window_len   <- 30
max_window_k <- 12
window_grid  <- 0:(max_window_k - 1)

set.seed(123)

matched_list     <- list()
used_control_ids <- character(0)
pair_counter     <- 0

for (k in window_grid) {
  
  t_start <- k * window_len
  t_end   <- (k + 1) * window_len
  
  message("Processing window k = ", k, " (", t_start, "-", t_end, " days)")
  
  ## Risk set (outcome-agnostic), observable through t_end
  risk_set_k <- df_ana %>%
    filter(obs_days >= t_end) %>%
    filter(!(ENROLID %in% used_control_ids)) %>%
    mutate(
      treated_window = if_else(
        addon_1y == 1L &
          !is.na(glp1_to_statin_days) &
          glp1_to_statin_days >  t_start &
          glp1_to_statin_days <= t_end,
        1L, 0L
      ),
      eligible_control = if_else(is.na(glp1_to_statin_days) | glp1_to_statin_days > t_end, 1L, 0L)
    ) %>%
    filter(treated_window == 1L | eligible_control == 1L)
  
  n_treated <- sum(risk_set_k$treated_window == 1L)
  n_control <- sum(risk_set_k$treated_window == 0L & risk_set_k$eligible_control == 1L)
  
  if (n_treated == 0 || n_control == 0) {
    message("  Skip window k = ", k, " (no treated or no eligible controls).")
    next
  }
  
  message("  Window k = ", k, ": treated = ", n_treated, ", controls = ", n_control)
  
  ## Build exposure-set baseline covariates at assess_date = GLP1_index + t_start
  risk_set_k_ps <- risk_set_k %>%
    mutate(
      assess_date = GLP1_index + t_start,
      
      smoking_tv = flag_before(smoking_index, assess_date),
      
      SLD_tv      = flag_before(SLD_index, assess_date),
      diabetes_tv = flag_before(diabetes_index, assess_date),
      diabetic_neuropathy_tv  = flag_before(diabetic_neuropathy_index, assess_date),
      diabetic_retinopathy_tv = flag_before(diabetic_retinopathy_index, assess_date),
      diabetic_nephropathy_tv = flag_before(diabetic_nephropathy_index, assess_date),
      
      hypertension_tv        = flag_before(hypertension_index, assess_date),
      hyperlipidaemia_tv     = flag_before(hyperlipidaemia_index, assess_date),
      obesity_tv             = flag_before(obesity_index, assess_date),
      acute_renal_failure_tv = flag_before(acute_renal_failure_index, assess_date),
      stroke_tv              = flag_before(stroke_index, assess_date),
      heart_failure_tv       = flag_before(heart_failure_index, assess_date),
      
      HBV_tv         = flag_before(HBV_index, assess_date),
      HCV_tv         = flag_before(HCV_index, assess_date),
      autoimmune_tv  = flag_before(autoimmune_index, assess_date),
      ALD_tv         = flag_before(ALD_index, assess_date),
      cryptogenic_tv = flag_before(cryptogenic_index, assess_date),
      other_tv       = flag_before(other_index, assess_date),
      
      metformin_tv     = flag_before(metformin_index, assess_date),
      asprin_tv        = flag_before(asprin_index, assess_date),
      insulin_tv       = flag_before(insulin_index, assess_date),
      DPP4_tv          = flag_before(DPP4_index, assess_date),
      sulfonylureas_tv = flag_before(sulfonylureas_index, assess_date),
      thiazides_tv     = flag_before(thiazides_index, assess_date),
      TZD_tv           = flag_before(TZD_index, assess_date),
      SGLT2_tv         = flag_before(SGLT2_index, assess_date),
      AGI_tv           = flag_before(AGI_index, assess_date),
      ACEI_tv          = flag_before(ACEI_index, assess_date),
      ARB_tv           = flag_before(ARB_index, assess_date),
      BB_tv            = flag_before(BB_index, assess_date),
      CCB_tv           = flag_before(CCB_index, assess_date),
      
      cirrhosis_tv       = flag_before(cirrhosis_index, assess_date),
      dcc_tv             = flag_before(dcc_index, assess_date),
      lt_tv              = flag_before(lt_index, assess_date),
      HCC_tv             = flag_before(HCC_index, assess_date),
      CVD_tv             = flag_before(CVD_index, assess_date),
      CKD_tv             = flag_before(CKD_index, assess_date),
      nonliver_cancer_tv = flag_before(nonliver_cancer_index, assess_date)
    ) %>%
    select(
      ENROLID,
      GLP1_index, statin_index, glp1_to_statin_days,
      enroll_end, study_end, obs_end_date, obs_days,
      treated_window, ever_combo, addon_1y, eligible_control,
      cirrhosis_index, dcc_index, lt_index, HCC_index, CVD_index, CKD_index, nonliver_cancer_index,
      assess_date,
      all_of(ps_covars_tv)
    ) %>%
    filter(!is.na(age))
  
  ## PS matching within window
  m_out <- matchit(
    formula  = ps_formula_tv,
    data     = risk_set_k_ps,
    method   = "nearest",
    distance = "logit",
    ratio    = 1,     # change to 4 if you want 1:4
    caliper  = 0.1,
    replace  = FALSE
  )
  
  matched_k <- match.data(m_out)
  if (nrow(matched_k) == 0) next
  
  matched_k <- matched_k %>% mutate(window_k = k)
  
  ## create global pair_id
  subclass_map <- matched_k %>%
    distinct(subclass) %>%
    arrange(subclass) %>%
    mutate(pair_id = row_number() + pair_counter)
  
  matched_k <- matched_k %>% left_join(subclass_map, by = "subclass")
  pair_counter <- max(matched_k$pair_id)
  
  ## prevent control reuse
  new_controls <- matched_k %>% filter(treated_window == 0L) %>% pull(ENROLID)
  used_control_ids <- union(used_control_ids, new_controls)
  
  matched_list[[paste0("k", k)]] <- matched_k
}

matched_all <- bind_rows(matched_list) %>%
  mutate(exposure = treated_window)

table(matched_all$exposure)

## 5) Construct index_date_final (pseudo-index for controls)
matched_all <- matched_all %>%
  group_by(pair_id) %>%
  mutate(
    t_star = max(if_else(exposure == 1, glp1_to_statin_days, NA_real_), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    pseudo_index = if_else(exposure == 0, GLP1_index + t_star, statin_index),
    index_date_final = if_else(exposure == 1, statin_index, pseudo_index)
  )

library(arrow)
arrow::write_parquet(matched_all, "matched_MASLD_GLP1cohort.csv")
# Sensitivity analysis PSM 1:1
arrow::write_parquet(matched_all, "subPS1.1_MASLD_GLP1cohort.csv")

# arrow::write_parquet(matched_all, "/home/hongfan1/Desktop/combine/matched_MASLD_GLP1cohort.csv")
# # Sensitivity analysis PSM 1:1
# arrow::write_parquet(matched_all, "/home/hongfan1/Desktop/combine/subPS1.1_MASLD_GLP1cohort.csv")



###Analysis outcomes####
library(arrow)
matched_all <- arrow::read_parquet("matched_MASLD_GLP1cohort.csv")
matched_all <- arrow::read_parquet("subPS1.1_MASLD_GLP1cohort.csv")
# subgroup
matched_all<-matched_all[which(matched_all$age<55),] # young
matched_all<-matched_all[which(matched_all$age>=55),] # old
matched_all<-matched_all[which(matched_all$SEX==1),] # male
matched_all<-matched_all[which(matched_all$SEX==2),] # female
matched_all<-matched_all[which(matched_all$CCI<4),] # low CCI
matched_all<-matched_all[which(matched_all$CCI>=4),] # high CCI
matched_all<-matched_all[which(matched_all$obesity_tv==0),] # non-obesity
matched_all<-matched_all[which(matched_all$obesity_tv==1),] # obesity
matched_all<-matched_all[which(matched_all$diabetes_tv==0),] # non-diabetes
matched_all<-matched_all[which(matched_all$diabetes_tv==1),] # diabetes
matched_all<-matched_all[which(matched_all$hyperlipidaemia_tv==0),] # non-hyperlipidaemia
matched_all<-matched_all[which(matched_all$hyperlipidaemia_tv==1),] # hyperlipidaemia
matched_all<-matched_all[which(matched_all$glp1_year_group==1),] # 2013-2015
matched_all<-matched_all[which(matched_all$glp1_year_group==2),] # 2016-2018
matched_all<-matched_all[which(matched_all$glp1_year_group==3),] # 2019-2021

# # 生成60 90 120天连续用药的变量,匹配上来
# glp1<-data.table::fread('/home/hongfan1/Desktop/combine/glp1.csv',fill=T,stringsAsFactors=TRUE) %>% 
#   setNames(c('ENROLID','end60_glp1','end90_glp1','end120_glp1')) # 1,274,075
# statin<-data.table::fread('/home/hongfan1/Desktop/combine/statin.csv',fill=T,stringsAsFactors=TRUE) %>%
#   setNames(c('ENROLID','end60_statin','end90_statin','end120_statin'))  # 16,633,836
# 
# matched_all<-left_join(matched_all,glp1,by='ENROLID')
# matched_all<-left_join(matched_all,statin,by='ENROLID')

## 6) Outcome analysis (ITT; same structure as your statin cohort)
analyze_outcome <- function(data, outcome_name) {
  
  index_var_name <- paste0(outcome_name, "_index")
  index_var_sym  <- rlang::sym(index_var_name)
  
  if (!(index_var_name %in% names(data))) stop("变量不存在: ", index_var_name)
  
  dat <- data %>%
    filter(!is.na(index_date_final)) %>%
    
    ## Exclude history BEFORE GLP1_index (background start for GLP1 cohort)
    mutate(
      outcome_baseline = if_else(!is.na(!!index_var_sym) & (!!index_var_sym) <= GLP1_index, 1L, 0L)
    ) %>%
    filter(outcome_baseline == 0L) %>%
    
    ## Exclude events before index_date_final
    mutate(
      outcome_by_index = if_else(!is.na(!!index_var_sym) & (!!index_var_sym) < index_date_final, 1L, 0L)
    ) %>%
    filter(outcome_by_index == 0L) %>%
    
    ## ITT follow-up end
    mutate(
      # one_year_end = index_date_final + lubridate::days(365), # one_year_end的加入保证随访时间只留在一年这个时间节点
      # follow_end_outcome = pmin(enroll_end, study_end, one_year_end, !!index_var_sym, na.rm = TRUE),
      # follow_end_outcome = pmin(enroll_end, study_end, end120_glp1, end120_statin, !!index_var_sym, na.rm = TRUE), # 连续用药敏感性分析 end60_glp1 end90_glp1 end120_glp1
      # landmark_date = index_date_final + lubridate::days(183), # landmark analysis
      # fu_days_outcome = as.numeric(follow_end_outcome - landmark_date), # landmark analysis
      follow_end_outcome = pmin(enroll_end, study_end, !!index_var_sym, na.rm = TRUE),
      event_outcome = if_else(!is.na(!!index_var_sym) & (!!index_var_sym) == follow_end_outcome, 1L, 0L),
      fu_days_outcome  = as.numeric(follow_end_outcome - index_date_final),
      fu_years_outcome = fu_days_outcome / 365.25
    ) %>%
    filter(fu_days_outcome > 0)
  
  if (nrow(dat) == 0) {
    warning("No valid follow-up for outcome: ", outcome_name)
    return(NULL)
  }
  
  incidence_table <- dat %>%
    group_by(exposure) %>%
    summarise(
      n       = n(),
      events  = sum(event_outcome),
      py      = sum(fu_years_outcome),
      ir_1000 = events / py * 1000,
      ir_lower = (0.5 * qchisq(0.025, 2 * events)) / py * 1000,
      ir_upper = (0.5 * qchisq(0.975, 2 * (events + 1))) / py * 1000,
      .groups = "drop"
    ) %>%
    mutate(
      ir_ci = sprintf("%.2f (%.2f–%.2f)",
                      round(ir_1000, 2), round(ir_lower, 2), round(ir_upper, 2))
    )
  
  cox_crude <- coxph(
    Surv(fu_years_outcome, event_outcome) ~ exposure + cluster(pair_id),
    data = dat
  )
  
  list(outcome_name = outcome_name,
       data = dat,
       incidence_table = incidence_table,
       cox_crude = cox_crude)
}

## Run outcomes + collect
outcomes <- c("cirrhosis","dcc","lt","HCC","CVD","CKD","nonliver_cancer")

ir_all <- tibble()
hr_all <- tibble()

for (out in outcomes) {
  res <- analyze_outcome(matched_all, out)
  if (is.null(res)) next
  
  ir_all <- bind_rows(ir_all, res$incidence_table %>% mutate(outcome = out))
  
  hr_tmp <- broom::tidy(res$cox_crude, conf.int = TRUE, exp = TRUE) %>%
    mutate(
      outcome = out,
      HR_95CI = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high),
      p = format.pval(p.value, format = "e", scientific = TRUE)
    ) %>%
    select(outcome, term, HR_95CI, p)
  
  hr_all <- bind_rows(hr_all, hr_tmp)
}

ir_all
hr_all

# exposure = 1: GLP1 + statin (add-on within 1 year)
# exposure = 0: GLP1 alone (never statin)


###Tableone####
library(tableone)
# colnames(df)
id<-matched_all[,c('ENROLID','exposure')]
df<-left_join(df,id,by='ENROLID')

myVars <- c('SEX','age','REGION','smoking_baseline','CCI','glp1_year_group','meta_count',
            'SLD_baseline','diabetes_baseline','diabetic_neuropathy_baseline','diabetic_retinopathy_baseline','diabetic_nephropathy_baseline', 
            'hypertension_baseline','hyperlipidaemia_baseline','obesity_baseline',"acute_renal_failure_baseline","stroke_baseline","heart_failure_baseline",
            'HBV_baseline','HCV_baseline','autoimmune_baseline','ALD_baseline','cryptogenic_baseline','other_baseline',
            'metformin_baseline','asprin_baseline','insulin_baseline','DPP4_baseline','sulfonylureas_baseline','thiazides_baseline','TZD_baseline','SGLT2_baseline','AGI_baseline','ACEI_baseline','ARB_baseline','BB_baseline','CCB_baseline',
            'cirrhosis_baseline','dcc_baseline','lt_baseline','HCC_baseline','CVD_baseline','CKD_baseline','nonliver_cancer_baseline')  
catVars <- c('SEX','REGION','smoking_baseline','glp1_year_group','meta_count',
             'SLD_baseline','diabetes_baseline','diabetic_neuropathy_baseline','diabetic_retinopathy_baseline','diabetic_nephropathy_baseline', 
             'hypertension_baseline','hyperlipidaemia_baseline','obesity_baseline',"acute_renal_failure_baseline","stroke_baseline","heart_failure_baseline",
             'HBV_baseline','HCV_baseline','autoimmune_baseline','ALD_baseline','cryptogenic_baseline','other_baseline',
             'metformin_baseline','asprin_baseline','insulin_baseline','DPP4_baseline','sulfonylureas_baseline','thiazides_baseline','TZD_baseline','SGLT2_baseline','AGI_baseline','ACEI_baseline','ARB_baseline','BB_baseline','CCB_baseline',
             'cirrhosis_baseline','dcc_baseline','lt_baseline','HCC_baseline','CVD_baseline','CKD_baseline','nonliver_cancer_baseline')  


# tab <- CreateTableOne(vars = myVars, data = data, factorVars = catVars) # 不分层
tab <- CreateTableOne(vars = myVars, strata = "exposure" , data = df, factorVars = catVars) %>% print(smd = TRUE)  # addon_1y
# df, cirrhosis_matched, dcc_matched, lt_matched, hcc_matched, cvd_matched, ckd_matched, nonliver_cancer_matched
print(tab,  formatOptions = list(big.mark = ","))
tabMat <- print(tab, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(tabMat, file = "nonmatched_baseline.csv")

table(hos.pat$age_group,hos.pat$year_group) # OSHPD population

####Plot####
# y.max <- max(1 - summary(fit)$surv)   # 最大累积发病率
# y.limit <- ceiling((y.max + 0.03) * 10) / 10  # 上调 5%，并四舍五入到 0.1 的倍数
library(survival)
library(survminer)

# 用于计算的统一函数
make_km_dat <- function(dat, outcome_name){
  idx <- sym(paste0(outcome_name, "_index"))
  
  dat %>%
    filter(!is.na(index_date_final)) %>%
    # 排除 time zero 之前发生的该结局（否则会出现负随访）
    filter(is.na(!!idx) | (!!idx) > index_date_final) %>%
    mutate(
      censor_date = pmin(enroll_end, study_end, na.rm = TRUE),
      follow_end  = pmin(censor_date, !!idx, na.rm = TRUE),
      event       = if_else(!is.na(!!idx) & (!!idx) <= censor_date, 1L, 0L),
      time_years  = as.numeric(follow_end - index_date_final) / 365.25,
      exposure_f  = factor(exposure, levels = c(0,1),
                           labels = c("Monotherapy", "Combination"))
    ) %>%
    filter(time_years > 0)
}

# 替换结局
km <- make_km_dat(matched_all, "nonliver_cancer")
fit <- survfit(Surv(time_years, event) ~ exposure_f, data = km)
# Size 7.1*6.7
ggsurvplot(
  fit,
  data =km,
  fun = "event",
  pval = TRUE,              # 显示 log-rank 检验 P 值
  conf.int = FALSE,          # 显示置信区间
  risk.table = TRUE,        # 添加风险表
  legend.labs = c("0", "1"),  # "With GLP1-RAs", "Without GLP1-RAs"
  legend.title = "Group",
  xlab = "Follow-up (years)",
  xlim = c(0, 10),
  break.time.by = 2,
  # ylim = c(0, y.limit),
  ylim = c(0, 0.15),
  pval.coord = c(0, 0.15), # P=...文本的位置 
  ylab = "Cumulative incidence of nonliver cancers",
  palette = c("#E7B800", "#2E9FDF"), # 自定义颜色
  size = 0.8,
  censor = FALSE
)

##Regression####
library(survival)
res.cox<-coxph(Surv(fu_years_cirrhosis,cirrhosis) ~ exposure+window_k,data=matched_all) 
# df, cirrhosis_matched, dcc_matched, lt_matched, hcc_matched, cvd_matched, ckd_matched, nonliver_cancer_matched
d <- broom::tidy(res.cox, conf.int = TRUE, exp = TRUE) %>% #broom::tidy(..., exp = TRUE) 会自动计算 exp(coef)（即 OR/HR）和 95% CI
  mutate(
    OR_95CI = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high),
    p = format.pval(p.value, digits = 3, scientific = TRUE)
  ) %>%
  select(term, OR_95CI, p)

d




##***Statin cohort2####
library(tidyverse)
df<-data.table::fread('/home/hongfan1/Desktop/combine/masld.csv',fill=T,stringsAsFactors=TRUE) # 1235046
# df<-df[which(df$diabetes==1),]

###纳排####
# 1. 纳入数据库中2010-2023年的所有Statin首次使用者
df$SLD_index<-as.Date(df$SLD_index)
df$GLP1_index<-as.Date(df$GLP1_index)
df$statin_index<-as.Date(df$statin_index)

df <- df %>% 
  mutate(glp1_year = year(GLP1_index),
         statin_year = year(statin_index),
         sld_year = year(SLD_index)) %>% 
  filter (statin_year>=2010) # 893776

# 2. 剔除过去一年没有医疗记录的人，即没有连续参保记录超过一年的人
# 纳排 连续12个月可见期 
df <- df %>%
  # Step 0. 确保日期类型正确
  mutate(
    enroll_start = as.Date(enroll_start),
    enroll_end   = as.Date(enroll_end),
    statin_index   = as.Date(statin_index)
  ) %>%
  
  # Step 1. 筛选 index 落在参保期间
  filter(statin_index >= enroll_start & statin_index <= enroll_end) %>%
  # Step 2. 计算 index 前可观测天数
  mutate(enroll_days_prior = as.numeric(statin_index - enroll_start)) %>%
  # Step 3. 保留连续参保 ≥12 个月（365 天）的人
  filter(enroll_days_prior >= 365) %>%
  # Step 4. 如果一个人有多段 enrollment，取 index 所在的那段（通常一人一条）
  group_by(ENROLID) %>%
  slice_max(order_by = enroll_start, n = 1, with_ties = FALSE) %>%
  ungroup() # 515639


# 3. 剔除statin index date时年龄小于18岁的
df <- df %>% 
  mutate(age = statin_year - DOBYR) %>%
  filter(age >= 18) # 515351

# 4. 剔除过去一年有GLP1和statin药物使用记录者 （这里实际操作中剔除2010年的人，不用在文中强调年份）
# 4. 至此，new users
df <- df %>% filter(statin_year > 2010) # 475745
df <- df %>% filter(is.na(glp1_year) | glp1_year > 2010) # 474624

# 5. 剔除在statin index前已经有慢性肝病相关病因，
# 计算所有疾病的基线患病情况，生成对应的baseline的变量
# DCC
df$cirrhosis<-ifelse(df$cirrhosis==1 | df$dcc_ascites==1 | df$dcc_gibeed==1 | df$dcc_HE==1 | df$dcc_varices==1,1,0)
df$dcc<-ifelse(df$dcc_ascites==1 | df$dcc_gibeed==1 | df$dcc_HE==1 | df$dcc_varices==1,1,0)
df$cirrhosis_index <- pmin(df$cirrhosis_index,df$dcc_ascites_index,df$dcc_gibeed_index,df$dcc_HE_index,df$dcc_varices_index,na.rm = TRUE)
df$dcc_index <- pmin(df$dcc_ascites_index,df$dcc_gibeed_index,df$dcc_HE_index,df$dcc_varices_index,na.rm = TRUE)

library(stringr)
df <- df %>%
  mutate(
    # 确保 index 和各疾病 index 都是日期类型
    across(c(statin_index, ends_with("_index")), as.Date),
    
    # 针对所有以 "_index" 结尾的列计算是否在 index 之前（含当天）
    across(
      ends_with("_index"),
      ~ if_else(!is.na(.) & (. - statin_index) <= 0, 1, 0),
      .names = "{str_replace(.col, '_index', '')}_baseline"
    )
  )

df$meta<-ifelse(df$diabetes_baseline==1 | df$hypertension_baseline==1 | df$hyperlipidaemia_baseline==1 | df$obesity_baseline==1,1,0)
df$meta_count <- rowSums(df[, c("diabetes_baseline","hypertension_baseline","hyperlipidaemia_baseline","obesity_baseline")], na.rm = TRUE)

# 排除SLD病因
nrow(df)
df <- df[df$HBV_baseline != 1, ] # 473665
df <- df[df$HCV_baseline != 1, ] # 472117
df <- df[df$autoimmune_baseline != 1, ] # 471653
df <- df[df$ALD_baseline != 1, ] # 469604
df <- df[df$cryptogenic_baseline != 1, ] # 469159
df <- df[df$other_baseline != 1, ] # 468008

# 6. 剔除SLD诊断在GLP1 index date之后的
df <- df %>% filter(statin_index >=SLD_index) # 252846

# 7. 剔除GLP1使用在statin之前的（避免病人同时进入 statin base cohort）
df <- df %>% filter(is.na(GLP1_index) | GLP1_index >= statin_index) # 238925
df$statin_year_group<-ifelse(df$statin_year<2015,1,ifelse(df$statin_year>=2015 & df$statin_year<2019,2,ifelse(df$statin_year>=2019,3,NA)))

# # 8. 排除结局相关 生成用于分析的亚组
# df <- df[df$cirrhosis_baseline != 1, ] # 139383
# df <- df[df$dcc_baseline != 1, ] # 139383
# # df <- df[df$dcc_HE_baseline != 1, ]
# # df <- df[df$dcc_gibeed_baseline != 1, ]
# # df <- df[df$dcc_ascites_baseline != 1, ]
# # df <- df[df$dcc_varices_baseline != 1, ]
# df <- df[df$lt_baseline != 1, ] # 139341
# df <- df[df$HCC_baseline != 1, ] # 139316
# df <- df[df$CVD_baseline != 1, ] # 135762
# df <- df[df$CKD_baseline != 1, ] # 131581
# df <- df[df$nonliver_cancer_baseline != 1, ] # 125948

# # MASLD？
# df$meta<-ifelse(df$diabetes_baseline==1 | df$hypertension_baseline==1 | df$hyperlipidaemia_baseline==1 | df$obesity_baseline==1,1,0)
# df$MASLD<-ifelse(df$meta==1 & df$HBV_baseline==0 & df$HCV_baseline==0 & df$autoimmune_baseline==0 & df$other_baseline==0 & df$ALD_baseline==0, 1, 0) ## 30721  8610 (pdff)
# df<-df[which(df$MASLD==1),] # 194728

##Statistical analysis####
## Statin cohort (statin -> add-on GLP1 within 1 year)
## Time-updated covariates + 30-day risk-set PS matching
## (Assumes df is already eligibility-filtered and has dcc/dcc_index created)
library(dplyr)
library(lubridate)
library(MatchIt)
library(survival)
library(tidyr)
library(rlang)
library(readr)
library(broom)

## Helper: time-updated 0/1 based on *_index <= assess_date
flag_before <- function(index_date, assess_date) {
  if_else(!is.na(index_date) & index_date <= assess_date, 1L, 0L)
}

## 1) Pre-processing (statin as background)
df <- df %>%
  mutate(
    GLP1_index   = as.Date(GLP1_index),
    statin_index = as.Date(statin_index),
    enroll_end   = as.Date(enroll_end),
    study_end    = as.Date("2023-12-31")
  ) %>%
  mutate(
    obs_end_date = pmin(enroll_end, study_end, na.rm = TRUE),
    obs_days     = as.numeric(obs_end_date - statin_index),
    statin_to_GLP1_days = as.numeric(GLP1_index - statin_index)
  )

## 2) Exposure definition (add-on within 1 year)
df <- df %>%
  mutate(
    ever_combo = if_else(!is.na(GLP1_index) & statin_to_GLP1_days >= 0, 1L, 0L),
    addon_1y   = if_else(ever_combo == 1L & statin_to_GLP1_days <= 365, 1L, 0L)
  )

## Analysis pool:
## - treated candidates: addon_1y==1
## - control candidates: ever_combo==0 (never GLP1 during follow-up)
df_ana <- df

## 3) PS covariates (time-updated at each exposure set)
##    NOTE: Use *_index columns you already have.
ps_covars_tv <- c(
  # fixed/demo
  "SEX","age","REGION","CCI","statin_year_group","meta_count",
  
  # time-updated clinical/lifestyle
  "smoking_tv",
  
  "SLD_tv","diabetes_tv","diabetic_neuropathy_tv","diabetic_retinopathy_tv","diabetic_nephropathy_tv",
  "hypertension_tv","hyperlipidaemia_tv","obesity_tv","acute_renal_failure_tv","stroke_tv","heart_failure_tv",
  "HBV_tv","HCV_tv","autoimmune_tv","ALD_tv","cryptogenic_tv","other_tv",
  
  # meds (time-updated)
  "metformin_tv","asprin_tv","insulin_tv","DPP4_tv","sulfonylureas_tv","thiazides_tv","TZD_tv","SGLT2_tv","AGI_tv",
  "ACEI_tv","ARB_tv","BB_tv","CCB_tv",
  
  # prior outcomes / disease burden (time-updated)
  "cirrhosis_tv","dcc_tv","lt_tv","HCC_tv","CVD_tv","CKD_tv","nonliver_cancer_tv"
)

ps_formula_tv <- as.formula(paste("treated_window ~", paste(ps_covars_tv, collapse = " + ")))

## 4) Time-conditional PS matching in 30-day windows
window_len   <- 30
max_window_k <- 12
window_grid  <- 0:(max_window_k - 1)

set.seed(123)

matched_list     <- list()
used_control_ids <- character(0)
pair_counter     <- 0

for (k in window_grid) {
  
  t_start <- k * window_len
  t_end   <- (k + 1) * window_len
  
  message("Processing window k = ", k, " (", t_start, "-", t_end, " days)")
  
  ## 4.1 Risk set (outcome-agnostic), require being observable through t_end
  risk_set_k <- df_ana %>%
    filter(obs_days >= t_end) %>%
    filter(!(ENROLID %in% used_control_ids)) %>%
    mutate(
      ## treated if add-on occurs within this window (t_start, t_end]
      treated_window = if_else(
        addon_1y == 1L &
          !is.na(statin_to_GLP1_days) &
          statin_to_GLP1_days >  t_start &
          statin_to_GLP1_days <= t_end,
        1L, 0L
      ),
      eligible_control = if_else(is.na(statin_to_GLP1_days) | statin_to_GLP1_days > t_end, 1L, 0L)
    ) %>%
    filter(treated_window == 1L | eligible_control == 1L)
  
  n_treated <- sum(risk_set_k$treated_window == 1L)
  n_control <- sum(risk_set_k$treated_window == 0L & risk_set_k$eligible_control == 1L)
  
  if (n_treated == 0 || n_control == 0) {
    message("  Skip window k = ", k, " (no treated or no eligible controls).")
    next
  }
  
  message("  Window k = ", k, ": treated = ", n_treated, ", controls = ", n_control)
  
  ## 4.2 Build exposure-set baseline covariates at assess_date = statin_index + t_start
  risk_set_k_ps <- risk_set_k %>%
    mutate(
      assess_date = statin_index + t_start,
      
      smoking_tv = flag_before(smoking_index, assess_date),
      
      SLD_tv      = flag_before(SLD_index, assess_date),
      diabetes_tv = flag_before(diabetes_index, assess_date),
      diabetic_neuropathy_tv  = flag_before(diabetic_neuropathy_index, assess_date),
      diabetic_retinopathy_tv = flag_before(diabetic_retinopathy_index, assess_date),
      diabetic_nephropathy_tv = flag_before(diabetic_nephropathy_index, assess_date),
      
      hypertension_tv        = flag_before(hypertension_index, assess_date),
      hyperlipidaemia_tv     = flag_before(hyperlipidaemia_index, assess_date),
      obesity_tv             = flag_before(obesity_index, assess_date),
      acute_renal_failure_tv = flag_before(acute_renal_failure_index, assess_date),
      stroke_tv              = flag_before(stroke_index, assess_date),
      heart_failure_tv       = flag_before(heart_failure_index, assess_date),
      
      HBV_tv         = flag_before(HBV_index, assess_date),
      HCV_tv         = flag_before(HCV_index, assess_date),
      autoimmune_tv  = flag_before(autoimmune_index, assess_date),
      ALD_tv         = flag_before(ALD_index, assess_date),
      cryptogenic_tv = flag_before(cryptogenic_index, assess_date),
      other_tv       = flag_before(other_index, assess_date),
      
      metformin_tv     = flag_before(metformin_index, assess_date),
      asprin_tv        = flag_before(asprin_index, assess_date),
      insulin_tv       = flag_before(insulin_index, assess_date),
      DPP4_tv          = flag_before(DPP4_index, assess_date),
      sulfonylureas_tv = flag_before(sulfonylureas_index, assess_date),
      thiazides_tv     = flag_before(thiazides_index, assess_date),
      TZD_tv           = flag_before(TZD_index, assess_date),
      SGLT2_tv         = flag_before(SGLT2_index, assess_date),
      AGI_tv           = flag_before(AGI_index, assess_date),
      ACEI_tv          = flag_before(ACEI_index, assess_date),
      ARB_tv           = flag_before(ARB_index, assess_date),
      BB_tv            = flag_before(BB_index, assess_date),
      CCB_tv           = flag_before(CCB_index, assess_date),
      
      cirrhosis_tv       = flag_before(cirrhosis_index, assess_date),
      dcc_tv             = flag_before(dcc_index, assess_date),   # <- use your existing dcc_index
      lt_tv              = flag_before(lt_index, assess_date),
      HCC_tv             = flag_before(HCC_index, assess_date),
      CVD_tv             = flag_before(CVD_index, assess_date),
      CKD_tv             = flag_before(CKD_index, assess_date),
      nonliver_cancer_tv = flag_before(nonliver_cancer_index, assess_date)
    ) %>%
    select(
      ENROLID,
      statin_index, GLP1_index, statin_to_GLP1_days,
      enroll_end, study_end, obs_end_date, obs_days,
      treated_window, ever_combo, addon_1y, eligible_control,
      cirrhosis_index, dcc_index, lt_index, HCC_index, CVD_index, CKD_index, nonliver_cancer_index,
      assess_date,
      all_of(ps_covars_tv)
    ) %>%
    filter(!is.na(age))
  
  ## safety
  if (sum(risk_set_k_ps$treated_window == 1L) == 0 ||
      sum(risk_set_k_ps$treated_window == 0L & risk_set_k_ps$eligible_control == 1L) == 0) {
    message("  After PS prep: no treated or no controls; skip.")
    next
  }
  
  ## 4.3 PS matching within window (1:1 here; change ratio if desired)
  m_out <- matchit(
    formula  = ps_formula_tv,
    data     = risk_set_k_ps,
    method   = "nearest",
    distance = "logit",
    ratio    = 4,
    caliper  = 0.1,
    replace  = FALSE
  )
  
  matched_k <- match.data(m_out)
  if (nrow(matched_k) == 0) {
    message("  No matched pairs in this window.")
    next
  }
  
  matched_k <- matched_k %>% mutate(window_k = k)
  
  ## 4.4 create global pair_id
  subclass_map <- matched_k %>%
    distinct(subclass) %>%
    arrange(subclass) %>%
    mutate(pair_id = row_number() + pair_counter)
  
  matched_k <- matched_k %>% left_join(subclass_map, by = "subclass")
  pair_counter <- max(matched_k$pair_id)
  
  ## 4.5 prevent control reuse
  new_controls <- matched_k %>% filter(treated_window == 0L) %>% pull(ENROLID)
  used_control_ids <- union(used_control_ids, new_controls)
  
  matched_list[[paste0("k", k)]] <- matched_k
}

matched_all <- bind_rows(matched_list) %>%
  mutate(exposure = treated_window)

table(matched_all$exposure)
length(unique(matched_all$ENROLID))

## 5) Construct index_date_final (pseudo-index for controls)
matched_all <- matched_all %>%
  group_by(pair_id) %>%
  mutate(
    t_star = max(if_else(exposure == 1, statin_to_GLP1_days, NA_real_), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    pseudo_index = if_else(exposure == 0, statin_index + t_star, GLP1_index),
    index_date_final = if_else(exposure == 1, GLP1_index, pseudo_index)
  )

library(arrow)
arrow::write_parquet(matched_all, "/home/hongfan1/Desktop/combine/matched_MASLD_Statincohort.csv")
# Sensitivity analysis PSM 1:1
# arrow::write_parquet(matched_all, "/home/hongfan1/Desktop/combine/subPS1.1_MASLD_Statincohort.csv")

###Analysis outcomes####
## 6) Outcome analysis (ITT; your logic preserved with small robustness fixes)
library(arrow)
matched_all <- arrow::read_parquet("/home/hongfan1/Desktop/combine/matched_MASLD_Statincohort.csv")

# subgroup
matched_all<-matched_all[which(matched_all$age<55),] # young
matched_all<-matched_all[which(matched_all$age>=55),] # old
matched_all<-matched_all[which(matched_all$SEX==1),] # male
matched_all<-matched_all[which(matched_all$SEX==2),] # female
matched_all<-matched_all[which(matched_all$CCI<4),] # low CCI
matched_all<-matched_all[which(matched_all$CCI>=4),] # high CCI
matched_all<-matched_all[which(matched_all$obesity_tv==0),] # non-obesity
matched_all<-matched_all[which(matched_all$obesity_tv==1),] # obesity
matched_all<-matched_all[which(matched_all$diabetes_tv==0),] # non-diabetes
matched_all<-matched_all[which(matched_all$diabetes_tv==1),] # diabetes
matched_all<-matched_all[which(matched_all$hyperlipidaemia_tv==0),] # non-hyperlipidaemia
matched_all<-matched_all[which(matched_all$hyperlipidaemia_tv==1),] # hyperlipidaemia
matched_all<-matched_all[which(matched_all$statin_year_group==1),] # 2013-2015
matched_all<-matched_all[which(matched_all$statin_year_group==2),] # 2016-2018
matched_all<-matched_all[which(matched_all$statin_year_group==3),] # 2019-2021

# # 生成60 90 120天连续用药的变量,匹配上来
# glp1<-data.table::fread('/home/hongfan1/Desktop/combine/glp1.csv',fill=T,stringsAsFactors=TRUE) %>% 
#   setNames(c('ENROLID','end60_glp1','end90_glp1','end120_glp1')) # 1,274,075
# statin<-data.table::fread('/home/hongfan1/Desktop/combine/statin.csv',fill=T,stringsAsFactors=TRUE) %>%
#   setNames(c('ENROLID','end60_statin','end90_statin','end120_statin'))  # 16,633,836
# 
# matched_all<-left_join(matched_all,glp1,by='ENROLID')
# matched_all<-left_join(matched_all,statin,by='ENROLID')

analyze_outcome <- function(data, outcome_name) {
  
  index_var_name <- paste0(outcome_name, "_index")
  index_var_sym  <- rlang::sym(index_var_name)
  
  if (!(index_var_name %in% names(data))) {
    stop("变量不存在: ", index_var_name)
  }
  
  dat <- data %>%
    filter(!is.na(index_date_final)) %>%
    
    ## Exclude history BEFORE statin_index (background start for statin cohort)
    mutate(
      outcome_baseline = if_else(!is.na(!!index_var_sym) & (!!index_var_sym) <= statin_index, 1L, 0L)
    ) %>%
    filter(outcome_baseline == 0L) %>%
    
    ## Exclude events before index_date_final
    mutate(
      outcome_by_index = if_else(!is.na(!!index_var_sym) & (!!index_var_sym) < index_date_final, 1L, 0L)
    ) %>%
    filter(outcome_by_index == 0L) %>%
    
    ## ITT follow-up end
    mutate(
      # one_year_end = index_date_final + lubridate::days(365), # one_year_end的加入保证随访时间只留在一年这个时间节点
      # follow_end_outcome = pmin(enroll_end, study_end, one_year_end, !!index_var_sym, na.rm = TRUE),
      # follow_end_outcome = pmin(enroll_end, study_end, end120_glp1, end120_statin, !!index_var_sym, na.rm = TRUE), # 连续用药敏感性分析 end60_glp1 end90_glp1 end120_glp1
      # landmark_date = index_date_final + lubridate::days(183), # landmark analysis
      # fu_days_outcome = as.numeric(follow_end_outcome - landmark_date), # landmark analysis
      follow_end_outcome = pmin(enroll_end, study_end, !!index_var_sym, na.rm = TRUE),
      event_outcome = if_else(!is.na(!!index_var_sym) & (!!index_var_sym) == follow_end_outcome, 1L, 0L),
      fu_days_outcome  = as.numeric(follow_end_outcome - index_date_final),
      fu_years_outcome = fu_days_outcome / 365.25
    ) %>%
    filter(fu_days_outcome > 0)
  
  if (nrow(dat) == 0) {
    warning("No valid follow-up for outcome: ", outcome_name)
    return(NULL)
  }
  
  incidence_table <- dat %>%
    group_by(exposure) %>%
    summarise(
      n       = n(),
      events  = sum(event_outcome),
      py      = sum(fu_years_outcome),
      ir_1000 = events / py * 1000,
      ir_lower = (0.5 * qchisq(0.025, 2 * events)) / py * 1000,
      ir_upper = (0.5 * qchisq(0.975, 2 * (events + 1))) / py * 1000,
      .groups = "drop"
    ) %>%
    mutate(
      ir_ci = sprintf("%.2f (%.2f–%.2f)",
                      round(ir_1000, 2), round(ir_lower, 2), round(ir_upper, 2))
    )
  
  cox_crude <- coxph(
    Surv(fu_years_outcome, event_outcome) ~ exposure + cluster(pair_id),
    data = dat
  )
  
  list(outcome_name = outcome_name,
       data = dat,
       incidence_table = incidence_table,
       cox_crude = cox_crude)
}

## Run outcomes + collect
outcomes <- c("cirrhosis","dcc","lt","HCC","CVD","CKD","nonliver_cancer")

ir_all <- tibble()
hr_all <- tibble()

for (out in outcomes) {
  res <- analyze_outcome(matched_all, out)
  if (is.null(res)) next
  
  ir_all <- bind_rows(ir_all, res$incidence_table %>% mutate(outcome = out))
  
  hr_tmp <- broom::tidy(res$cox_crude, conf.int = TRUE, exp = TRUE) %>%
    mutate(
      outcome = out,
      HR_95CI = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high),
      p = format.pval(p.value, format = "e", scientific = TRUE)
    ) %>%
    select(outcome, term, HR_95CI, p)
  
  hr_all <- bind_rows(hr_all, hr_tmp)
}

ir_all
hr_all

# exposure = 1: statin + GLP1 (add-on within 1 year)
# exposure = 0: statin alone (never GLP1)

###Tableone####
library(tableone)
# colnames(df)
id<-matched_all[,c('ENROLID','exposure')]
df<-left_join(df,id,by='ENROLID')

myVars <- c('SEX','age','REGION','smoking_baseline','CCI','statin_year_group','meta_count',
            'SLD_baseline','diabetes_baseline','diabetic_neuropathy_baseline','diabetic_retinopathy_baseline','diabetic_nephropathy_baseline', 
            'hypertension_baseline','hyperlipidaemia_baseline','obesity_baseline',"acute_renal_failure_baseline","stroke_baseline","heart_failure_baseline",
            'HBV_baseline','HCV_baseline','autoimmune_baseline','ALD_baseline','cryptogenic_baseline','other_baseline',
            'metformin_baseline','asprin_baseline','insulin_baseline','DPP4_baseline','sulfonylureas_baseline','thiazides_baseline','TZD_baseline','SGLT2_baseline','AGI_baseline','ACEI_baseline','ARB_baseline','BB_baseline','CCB_baseline',
            'cirrhosis_baseline','dcc_baseline','lt_baseline','HCC_baseline','CVD_baseline','CKD_baseline','nonliver_cancer_baseline')  
catVars <- c('SEX','REGION','smoking_baseline','statin_year_group','meta_count',
             'SLD_baseline','diabetes_baseline','diabetic_neuropathy_baseline','diabetic_retinopathy_baseline','diabetic_nephropathy_baseline', 
             'hypertension_baseline','hyperlipidaemia_baseline','obesity_baseline',"acute_renal_failure_baseline","stroke_baseline","heart_failure_baseline",
             'HBV_baseline','HCV_baseline','autoimmune_baseline','ALD_baseline','cryptogenic_baseline','other_baseline',
             'metformin_baseline','asprin_baseline','insulin_baseline','DPP4_baseline','sulfonylureas_baseline','thiazides_baseline','TZD_baseline','SGLT2_baseline','AGI_baseline','ACEI_baseline','ARB_baseline','BB_baseline','CCB_baseline',
             'cirrhosis_baseline','dcc_baseline','lt_baseline','HCC_baseline','CVD_baseline','CKD_baseline','nonliver_cancer_baseline')  


# tab <- CreateTableOne(vars = myVars, data = data, factorVars = catVars) # 不分层
tab <- CreateTableOne(vars = myVars, strata = "addon_1y" , data = df, factorVars = catVars) %>% print(smd = TRUE) # addon_1y exposure
# df, cirrhosis_matched, dcc_matched, lt_matched, hcc_matched, cvd_matched, ckd_matched, nonliver_cancer_matched
print(tab,  formatOptions = list(big.mark = ","))
tabMat <- print(tab, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(tabMat, file = "nonmatched_baseline.csv")



####Plot####
# y.max <- max(1 - summary(fit)$surv)   # 最大累积发病率
# y.limit <- ceiling((y.max + 0.03) * 10) / 10  # 上调 5%，并四舍五入到 0.1 的倍数
library(survival)
library(survminer)

# 用于计算的统一函数
make_km_dat <- function(dat, outcome_name){
  idx <- sym(paste0(outcome_name, "_index"))
  
  dat %>%
    filter(!is.na(index_date_final)) %>%
    # 排除 time zero 之前发生的该结局（否则会出现负随访）
    filter(is.na(!!idx) | (!!idx) > index_date_final) %>%
    mutate(
      censor_date = pmin(enroll_end, study_end, na.rm = TRUE),
      follow_end  = pmin(censor_date, !!idx, na.rm = TRUE),
      event       = if_else(!is.na(!!idx) & (!!idx) <= censor_date, 1L, 0L),
      time_years  = as.numeric(follow_end - index_date_final) / 365.25,
      exposure_f  = factor(exposure, levels = c(0,1),
                           labels = c("Monotherapy", "Combination"))
    ) %>%
    filter(time_years > 0)
}

# 替换结局
km <- make_km_dat(matched_all, "CVD")
fit <- survfit(Surv(time_years, event) ~ exposure_f, data = km)
# Size 7.1*6.7
ggsurvplot(
  fit,
  data =km,
  fun = "event",
  pval = TRUE,              # 显示 log-rank 检验 P 值
  conf.int = FALSE,          # 显示置信区间
  risk.table = TRUE,        # 添加风险表
  legend.labs = c("0", "1"),  # "With GLP1-RAs", "Without GLP1-RAs"
  legend.title = "Group",
  xlab = "Follow-up (years)",
  xlim = c(0, 10),
  break.time.by = 2,
  # ylim = c(0, y.limit),
  ylim = c(0, 0.2),
  pval.coord = c(0, 0.2), # P=...文本的位置 
  ylab = "Cumulative incidence of CVD",
  palette = c("#E7B800", "#2E9FDF"), # 自定义颜色
  size = 0.8,
  censor = FALSE
)
#RCS####
library(rms)
library(ggplot2)

matched_all$cirrhosis<-ifelse(is.na(matched_all$cirrhosis_index),0,1)
matched_all$fu_cirrhosis<-matched_all$cirrhosis_index-matched_all$index_date_final

be<-matched_all[,c('ENROLID','glp1_to_statin_days','statin_index','pseudo_index','index_date_final','cirrhosis','fu_cirrhosis')] # 
head(be)
be<-be[!is.na(be$glp1_to_statin_days),]
be<-be[be$glp1_to_statin_days<=365,]

dd <- datadist(be) #为后续程序设定数据环境tab
options(datadist='dd')
# dd$limits$f.30790.0.0[2]<-10.75 # 设置最低点的，一般是临床意义的点
fit<-cph(Surv(fu_cirrhosis,cirrhosis) ~ rcs(glp1_to_statin_days,nk=3),data=be)  ## 全人群/不吸烟人群 Model1
HR<-Predict(fit,glp1_to_statin_days, fun=exp, ref.zero = TRUE) ## 拟合曲线
# HR
summary(fit)
anova(fit)

# 绘制左右双坐标轴baseplot
par(mar = c(5, 4, 4, 4) + 0.3)
par(xpd=NA)
ylim.bot<-min(HR[,"lower"])
ylim.top<-max(HR[,"upper"])

# 不用几分位数，只是均匀绘制直方图
hist(be$glp1_to_statin_days, yaxt = "n", xaxt = "n", xlab = "", ylab = "", main = "",col = ggplot2::alpha('grey',0.25),border = ggplot2::alpha('grey'))


quantiles <- quantile(be$glp1_to_statin_days, probs = seq(0, 1, 0.2)) # 十分位
hist(be$glp1_to_statin_days, yaxt = "n", xaxt = "n", xlab = "", ylab = "", main = "",col = ggplot2::alpha('grey',0.25),border = ggplot2::alpha('grey'))
# hist(be$glp1_to_statin_days, breaks = quantiles,yaxt = "n", xaxt = "n", xlab = "", ylab = "", main = "",col = ggplot2::alpha('grey',0.25),border = ggplot2::alpha('grey'))
# quantiles <- quantile(data$GRS2, probs = seq(0, 1, 0.2)) # 五分位
# hist(be$GRS2, breaks = quantiles,yaxt = "n", xaxt = "n", xlab = "", ylab = "", main = "",col = ggplot2::alpha('grey',0.25),border = ggplot2::alpha('grey'))
# 
# hist(be$f.30790.0.0,breaks = quantiles,yaxt = "n", xaxt = "n", xlab = "", ylab = "", main = "",col = ggplot2::alpha('grey',0.25),border = ggplot2::alpha('grey'))

# color <- rgb(158,214,194, maxColorValue = 255)
# library(plotRCS)
options(scipen = 999)
axis(4)
par(new=TRUE) # 新增画布
plot(HR[,1],HR[,"yhat"],
     xlab = "Lp(a) concentration (nmol/L)",ylab = paste0("HR (95% CI)"), # Lp(a) (nmol/L)
     type = "l",bty='l',ylim = c(ylim.bot,ylim.top),
     col='steelblue',lwd=2) # 'steelblue'
# 
polygon(c(HR$f.30790.0.0,rev(HR$f.30790.0.0)),c(HR$lower,rev(HR$upper)),col = ggplot2::alpha('steelblue',0.4),border = ggplot2::alpha('steelblue',0.4))
lines(x=c(min(HR$f.30790.0.0),max(HR$f.30790.0.0)),y=c(1,1),lty=3,col="grey40",lwd=1.5)
# polygon(c(HR$GRS,rev(HR$GRS)),c(HR$lower,rev(HR$upper)),col = ggplot2::alpha('steelblue',0.4),border = ggplot2::alpha('steelblue',0.4))
# lines(x=c(min(HR$GRS),max(HR$GRS)),y=c(1,1),lty=3,col="grey40",lwd=1.5)

points(10.75,1,pch=20,cex=1.0) ## 手动在OR结果里找到这个点
# 图片大小：6.34*5.60
mtext("Measured Lp(a) and cirrhosis", side = 3, line = 1, cex = 1.2, font = 1)
mtext("Ref=10.75", side = 3, line = 2, cex = 1.0, font = 1) 





#&&&&&&&&&&&&&&&&&###########################
#Diabetes poulation####
library(tidyverse)
df<-data.table::fread('/home/hongfan1/Desktop/combine/diabetes.csv',fill=T,stringsAsFactors=TRUE) # 6419781




