# ==============================================================================
# Zung SDS & depression awareness - Syrian university students
# Clean pipeline: raw Excel -> clean workbook -> every published table & figure.
# Step-by-step rationale for the whole pipeline lives in docs/WORKFLOW.md;
# variable definitions in docs/CODEBOOK.md.
# Run: Rscript R/analysis_SDS.R  (from the repository root; R >= 4.3, deterministic)
# ==============================================================================

# 0. Setup ---------------------------------------------------------------------
pkgs <- c("readxl", "writexl", "dplyr", "tidyr", "stringr", "tibble", "forcats",
          "ggplot2", "scales", "psych", "GPArotation", "broom", "pROC", "binom",
          "rcompanion", "gridExtra", "RColorBrewer")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE))
  install.packages(p, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))

# Paths resolve relative to the repository root, so the pipeline runs anywhere
# after `git clone`. Run it from the repo root:
#     Rscript R/analysis_SDS.R
# To run from elsewhere, point SDS_BASE at the repository root:
#     SDS_BASE=/path/to/depression-awareness-syria Rscript R/analysis_SDS.R
BASE <- Sys.getenv("SDS_BASE", unset = getwd())
if (!dir.exists(file.path(BASE, "R")))
  stop("Run this script from the repository root, or set SDS_BASE. Current BASE: ", BASE)
OUT  <- file.path(BASE, "outputs")
FIG  <- file.path(OUT, "figures"); TAB <- file.path(OUT, "tables")
dir.create(FIG, FALSE, TRUE); dir.create(TAB, FALSE, TRUE)
DATA <- file.path(BASE, "data"); dir.create(DATA, FALSE, TRUE)
RAW_XLSX   <- file.path(DATA, "Depression Awareness.xlsx")
CLEAN_XLSX <- file.path(DATA, "Depression_SDS_clean.xlsx")
if (!file.exists(RAW_XLSX))
  stop("Raw survey workbook not found at ", RAW_XLSX,
       "\n  Individual-level data are not distributed with this repository; see data/README.md.")

pv   <- function(p) ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3))
pv4  <- function(p) ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 4))
tab  <- function(x, name) write.csv(x, file.path(TAB, paste0(name, ".csv")), row.names = FALSE)
gsav <- function(name, w, h, plot = ggplot2::last_plot())
  ggsave(file.path(FIG, paste0(name, ".png")), plot, width = w, height = h,
         dpi = 300, bg = "white", limitsize = FALSE)
med_iqr <- function(x) sprintf("%.1f (%.1f-%.1f)", median(x), quantile(x, .25), quantile(x, .75))

theme_set(theme_minimal(base_size = 11) +
          theme(plot.title = element_text(face = "bold", hjust = .5),
                plot.subtitle = element_text(hjust = .5),
                plot.background  = element_rect(fill = "white", color = NA),
                panel.background = element_rect(fill = "white", color = NA)))

# 1. Clean data & workbook -----------------------------------------------------
raw <- readxl::read_excel(RAW_XLSX)
names(raw) <- str_trim(names(raw))
raw <- raw |> mutate(across(where(is.character), str_trim))

rename_map <- c(
  "Gender" = "Sex", "Height (cm)" = "Height", "Weight (kg)" = "Weight",
  "Marital Status" = "MaritalStatus", "Governorate of Origin" = "Governorate",
  "Place of Residence" = "Residence", "Socioeconomic Status" = "SES",
  "Academic Year" = "AcademicYear", "University Cumulative Grade (%)" = "GPA",
  "Does one of the parents work in healthcare?" = "ParentHealthcare",
  "Are you a smoker? (Multiple Response Question)" = "SmokerRaw",
  "Alcohol Consumption" = "Alcohol",
  "Working Hours / Employment Status" = "Employment",
  "Do you suffer from any chronic diseases? (Multiple Response Question)" = "ChronicRaw",
  "Do you have any siblings?" = "Siblings",
  "Do you believe you have a good relationship with your family?" = "FamilyRelationship",
  "Have you ever been previously diagnosed with depression by a doctor?" = "PriorDepressionDx",
  "Do you know anyone who has been diagnosed with depression by a doctor?" = "KnowsDepressed",
  "Have you ever been diagnosed with a mental illness by a doctor?" = "PriorMentalDx")
for (old in names(rename_map))
  if (old %in% names(raw)) names(raw)[names(raw) == old] <- rename_map[[old]]

# Zung SDS: items detected by "1."-"20." headers; text Likert -> 1..4;
# positively-worded items (survey positions 11-20 in this dataset) reversed.
sds_old <- grep("^\\s*\\d+\\.", names(raw), value = TRUE)
sds_old <- sds_old[order(as.integer(sub("^\\s*(\\d+)\\..*$", "\\1", sds_old)))]
stopifnot(length(sds_old) == 20)
sds <- sprintf("SDS%02d", 1:20)
names(raw)[match(sds_old, names(raw))] <- sds

likert <- c("A little of the time" = 1, "Some of the time" = 2,
            "Good part of the time" = 3, "Most of the time" = 4)
df <- raw |> mutate(across(all_of(sds), ~ unname(likert[str_trim(as.character(.x))])))
POSITIVE <- 11:20
for (i in POSITIVE) df[[sprintf("SDS%02d_rc", i)]] <- 5 - df[[sprintf("SDS%02d", i)]]
scoring_cols <- ifelse(seq_len(20) %in% POSITIVE, paste0(sds, "_rc"), sds)

df$SDS_complete  <- as.integer(rowSums(is.na(df[sds])) == 0)
df$SDS_raw       <- rowSums(df[scoring_cols])
df$SDS_index     <- df$SDS_raw / 80 * 100
df$SDS_category  <- cut(df$SDS_index, c(-Inf, 50, 60, 70, Inf),
                        labels = c("Normal", "Mild", "Moderate", "Severe"), right = FALSE)
df$SDS_sev_ord   <- as.integer(df$SDS_category)
df$SDS_depressed <- as.integer(df$SDS_index >= 50)

df$BMI     <- df$Weight / (df$Height / 100)^2
df$BMI_WHO <- cut(df$BMI, c(-Inf, 18.5, 25, 30, Inf),
                  labels = c("Underweight", "Normal", "Overweight", "Obese"), right = FALSE)
df$Height_implausible <- as.integer(df$Height < 130 | df$Height > 220)
df$BMI_implausible    <- as.integer(df$BMI < 12 | df$BMI > 60)

df$Smoker <- ifelse(grepl("smoke", df$SmokerRaw, ignore.case = TRUE) &
                      !grepl("^Non-smoker$", df$SmokerRaw), "Smoker", "Non-smoker")
df$ChronicDisease <- ifelse(grepl("None", df$ChronicRaw) & !grepl(",", df$ChronicRaw), "No", "Yes")
df$SES_g <- dplyr::recode(df$SES,
  "Low (Insufficient to meet the basic needs of the family)" = "Low",
  "Medium (Sufficient to meet basic needs only)"             = "Medium",
  "Good (Sufficient to meet basic needs with some luxuries)" = "Good",
  "Excellent (Provides comfort and luxury)"                  = "Excellent")
df$AcademicYearN <- as.integer(factor(df$AcademicYear,
                      levels = c("First","Second","Third","Fourth","Fifth","Sixth")))
df$AgeGroup <- cut(df$Age, c(-Inf, 20, 23, 26, Inf),
                   labels = c("<20","20-22","23-25","26+"), right = FALSE)
df$University <- dplyr::recode(df$University,
  "Damascus University" = "Damascus University", "Other Universities" = "Other")
df$Faculty  <- str_replace(df$Faculty, "^Faculty of ", "")
df$Siblings <- dplyr::recode(df$Siblings, "Yes, I do" = "Yes", "No, I am an only child" = "No")
df$ParentHealthcare <- dplyr::recode(df$ParentHealthcare,
  "No (None of them)" = "No", "Yes (One of them)" = "One parent",
  "Yes (Both of them)" = "Both parents")
df$Employment <- dplyr::recode(df$Employment,
  "Unemployed / Do not work" = "Unemployed", "Part-time" = "Part-time",
  "Full-time" = "Full-time")

SRC_COL <- "What is your source of information regarding awareness about depression ? (Multiple Response)"
src_lc <- tolower(replace_na(df[[SRC_COL]], ""))
df$Source_Mass_Media         <- as.integer(grepl("mass\\s*media",        src_lc))
df$Source_Doctors            <- as.integer(grepl("doctor",               src_lc))
df$Source_Family             <- as.integer(grepl("family",               src_lc))
df$Source_Social_Media       <- as.integer(grepl("social\\s*media",      src_lc))
df$Source_University_Courses <- as.integer(grepl("university\\s*course", src_lc))
matched <- df$Source_Mass_Media + df$Source_Doctors + df$Source_Family +
           df$Source_Social_Media + df$Source_University_Courses
df$Source_Other <- as.integer(nchar(src_lc) > 0 & matched == 0)
df$Source_Count <- matched + df$Source_Other

AW_KEY <- tibble::tribble(
  ~new,                     ~col, ~correct,
  "AW01_Disease",           "Do you believe that depression is considered a disease/illness?",            "Yes",
  "AW02_NotSpontaneous",    "? Do you believe that depression can recede or heal spontaneously",          "No, it cannot",
  "AW03_Genetic",           "Do you believe that depression is associated with a genetic factor?",        "Yes",
  "AW04_FemaleRisk",        "Do you believe that females are more susceptible to developing depression?", "Yes",
  "AW05_Hormones",          "Do you believe that depression is associated with hormone secretion?",       "Yes",
  "AW06_ChronicDisease",    "?Do you believe that depression is associated with chronic diseases",        "Yes",
  "AW07_Personality",       "?Do you believe that depression is associated with an individual's personality", "Yes",
  "AW08_Symptoms",          "Do you know what the symptoms of depression are?",                           "Yes",
  "AW09_SubstanceAbuse",    "Do you believe that depression is associated with substance abuse?",         "Yes",
  "AW10_Treatment",         "Do you know how depression is treated?",                                     "Yes",
  "AW11_AntidepressantUse", "Do you know how to use antidepressant medications?",                         "Yes",
  "AW12_Relapse",           "Do you believe that depression can relapse?",                                "Yes",
  "AW13_SideEffects",       "Do you know what the side effects of depression treatment are?",             "Yes",
  "AW14_HealthEffect",      "Do you know the effect of depression on health?",                            "Yes",
  "AW15_Prevalence",        "Do you know the prevalence rate of depression among university students?",   "I know",
  "AW16_NotSelfTreat",      "Do you believe that depression can be self-treated (without consulting a doctor)?", "No",
  "AW17_Prevention",        "Do you know the methods for preventing depression?",                         "I know")
for (i in seq_len(nrow(AW_KEY))) {
  col <- AW_KEY$col[i]; stopifnot(col %in% names(df))
  df[[AW_KEY$new[i]]] <- as.integer(str_trim(as.character(df[[col]])) == AW_KEY$correct[i])
}
df$AwarenessScore <- rowSums(df[AW_KEY$new])
qs <- quantile(df$AwarenessScore, c(1/3, 2/3), na.rm = TRUE)
df$AwarenessLevel <- cut(df$AwarenessScore, c(-Inf, qs[1], qs[2], Inf),
                         labels = c("Low", "Moderate", "High"))

codebook <- tibble::tribble(
  ~Variable, ~Type, ~Coding_or_formula,
  "Sex","categorical","Female / Male (renamed from 'Gender')",
  "Age","numeric","Years, as reported",
  "AgeGroup","ordinal","<20 / 20-22 / 23-25 / 26+ (from Age)",
  "Height","numeric","cm; Height_implausible flags <130 or >220",
  "Weight","numeric","kg",
  "BMI","numeric","= Weight / (Height/100)^2",
  "BMI_WHO","ordinal","Underweight<18.5 / Normal 18.5-24.9 / Overweight 25-29.9 / Obese >=30",
  "BMI_implausible","flag 0/1","1 if BMI<12 or >60 (kept, not deleted)",
  "Height_implausible","flag 0/1","1 if Height<130 or >220 (kept, not deleted)",
  "GPA","numeric","University cumulative grade (%)",
  "MaritalStatus","categorical","Single/Married/Widowed/Divorced",
  "Residence","categorical","Urban / Rural",
  "Governorate","categorical","Governorate of origin (whitespace-trimmed)",
  "SES_g","ordinal","Low/Medium/Good/Excellent (collapsed from SES long labels)",
  "University","categorical","Damascus University / Other",
  "Faculty","categorical","Medicine / Dentistry / Pharmacy",
  "AcademicYear","ordinal","First..Sixth; AcademicYearN = 1..6",
  "ParentHealthcare","ordinal","No / One parent / Both parents",
  "Smoker","binary","Smoker / Non-smoker (condensed from multi-response)",
  "Alcohol","binary","Yes / No",
  "Employment","categorical","Unemployed / Part-time / Full-time",
  "ChronicDisease","binary","Yes if any non-'None' chronic condition reported",
  "Siblings","binary","Yes / No",
  "FamilyRelationship","binary","Yes / No (good family relationship)",
  "PriorDepressionDx","binary","Yes / No (doctor-diagnosed depression)",
  "KnowsDepressed","binary","Yes / No (knows someone diagnosed)",
  "PriorMentalDx","binary","Yes / No (doctor-diagnosed mental illness)",
  "SDS01..SDS20","numeric 1-4","Zung items, text Likert recoded 1..4",
  "SDS11_rc..SDS20_rc","numeric 1-4","Reverse score = 5 - item, for positively-worded items 11..20",
  "SDS_complete","flag 0/1","1 if all 20 SDS items present (scoring gate)",
  "SDS_raw","numeric","Sum of 10 negative items + 10 reverse-scored positive items (20..80)",
  "SDS_index","numeric","= SDS_raw / 80 * 100 (25..100)",
  "SDS_category","ordinal","Normal<50 / Mild 50-59 / Moderate 60-69 / Severe >=70",
  "SDS_sev_ord","ordinal 1-4","Numeric severity (Normal=1..Severe=4)",
  "SDS_depressed","binary 0/1","1 if SDS_index >= 50 (screening-positive)",
  "AW01..AW17","binary 0/1","1 if the answer matches the informed/correct option",
  "AwarenessScore","numeric 0-17","Sum of the 17 awareness items",
  "AwarenessLevel","ordinal","Low/Moderate/High (tertile bands of AwarenessScore)",
  "Source_*","binary 0/1","Information-source indicators parsed from the multi-response item",
  "Source_Count","numeric","Number of information sources reported")
writexl::write_xlsx(list(data_clean = df, codebook = codebook), path = CLEAN_XLSX)

n_total <- sum(!is.na(df$SDS_index))

# Publication labels (used by the supplementary tables and heatmaps)
LBL_MAP <- c(
  Age = "Age (years)", Height = "Height (cm)", Weight = "Weight (kg)",
  BMI = "BMI (kg/m2)", GPA = "GPA (%)", AcademicYearN = "Academic year (1-6)",
  Sex = "Sex", MaritalStatus = "Marital status", Residence = "Residence",
  SES_g = "Socioeconomic status", University = "University", Faculty = "Faculty",
  AcademicYear = "Academic year", ParentHealthcare = "Parent works in healthcare",
  Smoker = "Smoking status", Alcohol = "Alcohol consumption",
  Employment = "Employment status", ChronicDisease = "Chronic disease",
  Siblings = "Has siblings", FamilyRelationship = "Good family relationship",
  PriorDepressionDx = "Prior depression diagnosis",
  KnowsDepressed = "Knows someone with depression",
  PriorMentalDx = "Prior mental-illness diagnosis", AgeGroup = "Age group",
  SDS_raw = "SDS raw score", SDS_index = "SDS Index", SDS_category = "SDS severity")
LBL <- function(v) ifelse(v %in% names(LBL_MAP), LBL_MAP[v], v)

LIN_LAB <- c(
  "(Intercept)"="(Intercept)", "Age"="Age (years)", "GPA"="GPA (%)",
  "SexMale"="Sex: Male (vs Female)",
  "SmokerSmoker"="Smoking: Smoker (vs Non-smoker)",
  "SiblingsYes"="Has siblings: Yes (vs No)",
  "FamilyRelationshipYes"="Good family relationship: Yes (vs No)",
  "PriorDepressionDxYes"="Prior depression dx: Yes (vs No)",
  "PriorMentalDxYes"="Prior mental-illness dx: Yes (vs No)",
  "ChronicDiseaseYes"="Chronic disease: Yes (vs No)",
  "SES_gGood"="SES: Good (vs Excellent)", "SES_gLow"="SES: Low (vs Excellent)",
  "SES_gMedium"="SES: Medium (vs Excellent)",
  "FacultyMedicine"="Faculty: Medicine (vs Dentistry)",
  "FacultyPharmacy"="Faculty: Pharmacy (vs Dentistry)",
  "AcademicYearFirst"="Academic year: 1st (vs 5th)",
  "AcademicYearSecond"="Academic year: 2nd (vs 5th)",
  "AcademicYearThird"="Academic year: 3rd (vs 5th)",
  "AcademicYearFourth"="Academic year: 4th (vs 5th)",
  "AcademicYearSixth"="Academic year: 6th (vs 5th)")

# 2. Descriptives: T1, T6/T6b, T7 -----------------------------------------------
fmt_med <- function(x) sprintf("%.1f (%.1f-%.1f)", median(x, na.rm = TRUE),
                               quantile(x, .25, na.rm = TRUE), quantile(x, .75, na.rm = TRUE))
fmt_npct <- function(x) {
  tb <- table(x, useNA = "no"); pc <- round(prop.table(tb) * 100, 1)
  data.frame(Category = names(tb), Value = sprintf("%d (%.1f%%)", as.integer(tb), pc))
}
cont_t1 <- c("Age", "Height", "Weight", "BMI", "GPA")
cat_t1  <- c("Sex","MaritalStatus","Residence","SES_g","University","Faculty",
             "AcademicYear","BMI_WHO","ParentHealthcare","Smoker","Alcohol",
             "Employment","ChronicDisease","Siblings","FamilyRelationship",
             "PriorDepressionDx","KnowsDepressed","PriorMentalDx")
t1 <- list()
for (v in cont_t1)
  t1[[length(t1)+1]] <- data.frame(Variable = v, Category = "Median (Q1-Q3)",
                                   Value = fmt_med(df[[v]]))
for (v in cat_t1) {
  tb <- fmt_npct(df[[v]]); tb$Variable <- c(v, rep("", nrow(tb)-1))
  t1[[length(t1)+1]] <- tb[, c("Variable","Category","Value")]
}
tab(dplyr::bind_rows(t1), "T1_descriptives")

wilson <- function(k, n) {
  ci <- binom::binom.wilson(k, n)
  c(n = k, pct = round(k/n*100, 1),
    CI_low_pct = round(ci$lower*100, 1), CI_high_pct = round(ci$upper*100, 1))
}
bands <- c("Normal","Mild","Moderate","Severe")
t6b <- dplyr::bind_rows(lapply(bands, function(b)
  data.frame(Band = b, t(wilson(sum(df$SDS_category == b, na.rm = TRUE), n_total)))))
w_dep <- wilson(sum(df$SDS_depressed, na.rm = TRUE), n_total)
t6b <- rbind(t6b, data.frame(Band = "Any depression (SDS>=50)", t(w_dep)))
tab(t6b, "T6b_severity_CI")
tab(setNames(t6b[1:4, c("Band","n","pct")], c("SDS_category","n","%")), "T6_severity")

sw <- shapiro.test(df$SDS_index)
ks <- suppressWarnings(ks.test(scale(df$SDS_index), "pnorm"))
tab(tibble(Test = c("Shapiro-Wilk", "Kolmogorov-Smirnov"),
           Statistic = round(c(sw$statistic, ks$statistic), 4),
           p_value = pv(c(sw$p.value, ks$p.value))), "T7_normality")

# 3. Reliability & item analysis: T2, T3 ----------------------------------------
sds_scored <- df[scoring_cols]; colnames(sds_scored) <- sds
alpha_obj <- psych::alpha(sds_scored, check.keys = FALSE)
sb_obj    <- psych::splitHalf(sds_scored)
tab(tibble(
  Statistic = c("Cronbach's alpha","Average inter-item r","Mean item-total r",
                "Spearman-Brown reliability","N items"),
  Value = c(round(alpha_obj$total$raw_alpha, 3), round(alpha_obj$total$average_r, 3),
            round(mean(alpha_obj$item.stats$r.drop), 3), round(sb_obj$maxrb, 3), 20)),
  "T2_reliability")

total <- rowSums(sds_scored)
tab(dplyr::bind_rows(lapply(seq_along(scoring_cols), function(j) {
  item <- sds_scored[[j]]
  tibble(Item = sds[j], Mean = round(mean(item), 2), SD = round(sd(item), 2),
         Corrected_ItemTotal_r = round(cor(item, total - item), 3),
         Alpha_if_Deleted = round(
           psych::alpha(sds_scored[, -j], check.keys = FALSE)$total$raw_alpha, 3))
})), "T3_item_analysis")

# 4. Group comparisons: T8, T9, T9b, chi-square ---------------------------------
bin_vars <- c("Sex","Residence","University","Smoker","Alcohol","Siblings",
              "FamilyRelationship","PriorDepressionDx","KnowsDepressed",
              "PriorMentalDx","ChronicDisease")
multi_vars <- c("MaritalStatus","SES_g","Faculty","AcademicYear",
                "ParentHealthcare","Employment","BMI_WHO")
med_iqr_rank <- function(x, r) sprintf("%s [%.1f]", med_iqr(x), round(mean(r), 1))

tab(dplyr::bind_rows(lapply(bin_vars, function(v) {
  d <- df[!is.na(df[[v]]) & !is.na(df$SDS_index), c(v, "SDS_index")]
  cats <- sort(unique(d[[v]])); if (length(cats) != 2) return(NULL)
  d$r <- rank(d$SDS_index)
  a <- d$SDS_index[d[[v]] == cats[1]]; b <- d$SDS_index[d[[v]] == cats[2]]
  ra <- d$r[d[[v]] == cats[1]];        rb <- d$r[d[[v]] == cats[2]]
  mw <- suppressWarnings(wilcox.test(a, b)); U <- as.numeric(mw$statistic)
  tibble(Variable = v,
         Group1 = sprintf("%s (n=%d)", cats[1], length(a)),
         G1_median = med_iqr(a), G1_mean_rank = round(mean(ra), 1),
         G1_median_rank = med_iqr_rank(a, ra),
         Group2 = sprintf("%s (n=%d)", cats[2], length(b)),
         G2_median = med_iqr(b), G2_mean_rank = round(mean(rb), 1),
         G2_median_rank = med_iqr_rank(b, rb),
         U = round(U, 1), p = pv(mw$p.value),
         r_rankbiserial = round(1 - 2 * U / (length(a) * length(b)), 3))
})), "T8_bivariate_binary")

t9 <- dplyr::bind_rows(lapply(multi_vars, function(v) {
  d <- df[!is.na(df[[v]]) & !is.na(df$SDS_index), c(v, "SDS_index")]
  kw  <- kruskal.test(d$SDS_index, factor(d[[v]]))
  eps <- rcompanion::epsilonSquared(d$SDS_index, factor(d[[v]]))
  tibble(Variable = v, H = round(kw$statistic, 2), df = kw$parameter,
         p = pv(kw$p.value), epsilon_sq = round(as.numeric(eps), 3))
}))
tab(t9, "T9_bivariate_multi")

tab(dplyr::bind_rows(lapply(multi_vars, function(v) {
  d <- df[!is.na(df[[v]]) & !is.na(df$SDS_index), c(v, "SDS_index")]
  d$r <- rank(d$SDS_index)
  d |> group_by(.data[[v]]) |>
    summarise(n = n(), Median_Q1_Q3 = med_iqr(SDS_index),
              Mean_rank = round(mean(r), 1), .groups = "drop") |>
    transmute(Variable = v, Group = .data[[v]], n, Median_Q1_Q3, Mean_rank)
})), "T9_mean_ranks")

sig_multi <- multi_vars[sapply(multi_vars, function(v) {
  d <- df[!is.na(df[[v]]) & !is.na(df$SDS_index), ]
  suppressWarnings(kruskal.test(d$SDS_index, factor(d[[v]]))$p.value) < 0.05
})]
if (length(sig_multi))
  tab(dplyr::bind_rows(lapply(sig_multi, function(v) {
    d <- df[!is.na(df[[v]]) & !is.na(df$SDS_index), ]
    pt <- pairwise.wilcox.test(d$SDS_index, factor(d[[v]]), p.adjust.method = "holm")
    m <- as.data.frame(as.table(pt$p.value)); names(m) <- c("Group1","Group2","p_holm")
    m <- m[!is.na(m$p_holm), ]; if (!nrow(m)) return(NULL)
    cbind(Variable = v, m, p = pv(m$p_holm))[, c("Variable","Group1","Group2","p")]
  })), "T9b_posthoc")

tab(dplyr::bind_rows(lapply(c(bin_vars, multi_vars), function(v) {
  d <- df[!is.na(df[[v]]) & !is.na(df$SDS_depressed), ]
  tb <- table(d[[v]], d$SDS_depressed); chi <- suppressWarnings(chisq.test(tb))
  tibble(Variable = v, chi2 = round(chi$statistic, 2), df = chi$parameter,
         p = pv(chi$p.value),
         CramersV = round(as.numeric(rcompanion::cramerV(tb)), 3))
})), "T_chi2_depressed")

# 5. Regression models: T12, T11, S3, S4, T11b, T_outliers ----------------------
mv_vars <- c("Age","Sex","Smoker","Siblings","FamilyRelationship",
             "PriorDepressionDx","PriorMentalDx","ChronicDisease",
             "SES_g","Faculty","AcademicYear","GPA")
mdat <- df |> dplyr::select(SDS_depressed, all_of(mv_vars)) |>
  tidyr::drop_na() |> dplyr::mutate(across(where(is.character), factor))
logit <- glm(SDS_depressed ~ ., data = mdat, family = binomial)
# Wald CIs (exp(b +/- 1.96 SE)) to match the published OR tables and the
# SDS >= 60 sensitivity table below; broom's default profile-likelihood CIs
# differ in the third decimal for the sparser contrasts.
lg_b  <- coef(logit); lg_se <- sqrt(diag(vcov(logit)))
tab(tibble(Predictor = names(lg_b),
           OR     = round(unname(exp(lg_b)), 3),
           CI_low = round(unname(exp(lg_b - 1.96 * lg_se)), 3),
           CI_high= round(unname(exp(lg_b + 1.96 * lg_se)), 3),
           p      = pv(2 * pnorm(-abs(lg_b / lg_se)))), "T12_logistic")
phat <- predict(logit, type = "response")
auc  <- as.numeric(pROC::auc(pROC::roc(mdat$SDS_depressed, phat, quiet = TRUE)))

lin_dat <- df |> dplyr::select(SDS_index, all_of(mv_vars)) |>
  tidyr::drop_na() |> dplyr::mutate(across(where(is.character), factor))
lin_fit <- lm(SDS_index ~ ., data = lin_dat)
lin_sm  <- summary(lin_fit); lin_ci <- confint(lin_fit)
t11 <- tibble(
  Predictor = unname(LIN_LAB[names(coef(lin_fit))]),
  Beta = round(unname(coef(lin_fit)), 3),
  SE = round(unname(lin_sm$coefficients[, 2]), 3),
  CI_low = round(lin_ci[, 1], 3), CI_high = round(lin_ci[, 2], 3),
  p = pv(lin_sm$coefficients[, 4]))
tab(t11, "T11_linear_regression")

# Per-term VIF on the design matrix; Breusch-Pagan for heteroscedasticity
Xdes <- model.matrix(lin_fit)
vif_of <- function(j) {
  xi <- Xdes[, j]; Z <- Xdes[, -j, drop = FALSE]
  if ("(Intercept)" %in% colnames(Z)) {
    Zd <- Z[, colnames(Z) != "(Intercept)", drop = FALSE]
    r2 <- summary(lm(xi ~ Zd))$r.squared
  } else r2 <- summary(lm(xi ~ Z - 1))$r.squared
  1 / (1 - r2)
}
t_vif <- tibble(Variable = unname(LIN_LAB[colnames(Xdes)]),
                VIF = round(sapply(seq_len(ncol(Xdes)), vif_of), 2))
tab(t_vif, "S3_VIF")
bp_r2 <- summary(lm(residuals(lin_fit)^2 ~ Xdes[, colnames(Xdes) != "(Intercept)"]))$r.squared
bp_p  <- pchisq(nrow(Xdes) * bp_r2, ncol(Xdes) - 1, lower.tail = FALSE)

# Heteroscedasticity-consistent (HC3) standard errors for the linear model.
# The Breusch-Pagan test above is significant, so the OLS estimates stay the
# published model and this robust-SE refit is reported as a sensitivity check
# (S11). Coefficients are unchanged; only the standard errors, CIs and p-values
# are recomputed. S11b collects the linear model's diagnostics in one table.
h_lin  <- hatvalues(lin_fit)
e_lin  <- residuals(lin_fit)
XtXinv <- solve(crossprod(Xdes))
Xw     <- Xdes * (e_lin / (1 - h_lin))          # HC3 weights: e_i / (1 - h_i)
V_hc3  <- XtXinv %*% crossprod(Xw) %*% XtXinv
se_hc3 <- sqrt(diag(V_hc3))
b_lin  <- coef(lin_fit)
df_lin <- df.residual(lin_fit)
tcrit  <- qt(0.975, df_lin)
p_ols  <- lin_sm$coefficients[, 4]
p_hc3  <- 2 * pt(-abs(b_lin / se_hc3), df_lin)
tab(tibble(Predictor   = unname(LIN_LAB[names(b_lin)]),
           Beta        = round(unname(b_lin), 3),
           SE_OLS      = round(unname(lin_sm$coefficients[, 2]), 3),
           SE_HC3      = round(unname(se_hc3), 3),
           CI_low_HC3  = round(unname(b_lin - tcrit * se_hc3), 3),
           CI_high_HC3 = round(unname(b_lin + tcrit * se_hc3), 3),
           p_OLS       = pv(p_ols),
           p_HC3       = pv(p_hc3),
           Sig_flip    = (p_ols < 0.05) != (p_hc3 < 0.05)),
    "S11_linear_robust_se")

sw_res <- shapiro.test(e_lin)
ck_lin <- cooks.distance(lin_fit)
tab(tibble(
  Check = c("Breusch-Pagan LM statistic", "Breusch-Pagan df", "Breusch-Pagan p-value",
            "Shapiro-Wilk on residuals (W)", "Shapiro-Wilk on residuals (p)",
            "Maximum Cook's distance", "Maximum leverage",
            "Leverage threshold 3(k+1)/n", "Coefficients changing significance under HC3"),
  Value = c(round(nrow(Xdes) * bp_r2, 3), ncol(Xdes) - 1, pv(bp_p),
            round(unname(sw_res$statistic), 4), pv(sw_res$p.value),
            round(max(ck_lin), 4), round(max(h_lin), 4),
            round(3 * ncol(Xdes) / nrow(Xdes), 4),
            sum((p_ols < 0.05) != (p_hc3 < 0.05)))),
  "S11b_linear_diagnostics")

# Sensitivity: logistic at the stricter SDS >= 60 cut-off (Wald CIs)
sens_dat <- df |> dplyr::select(SDS_index, all_of(mv_vars)) |>
  tidyr::drop_na() |> dplyr::mutate(across(where(is.character), factor),
                                    dep60 = as.integer(SDS_index >= 60))
g60 <- glm(dep60 ~ . - SDS_index, data = sens_dat, family = binomial)
g60_b <- coef(g60); g60_se <- sqrt(diag(vcov(g60)))
tab(tibble(Predictor = unname(LIN_LAB[names(g60_b)]),
           OR = round(exp(g60_b), 3),
           CI_low = round(exp(g60_b - 1.96 * g60_se), 3),
           CI_high = round(exp(g60_b + 1.96 * g60_se), 3),
           p = pv(2 * pnorm(-abs(g60_b / g60_se)))), "S4_sensitivity_logit_cut60")

# Sensitivity: refit the linear model excluding implausible Height/BMI (T11b)
excl <- (df$Height_implausible == 1) | (df$BMI_implausible == 1)
excl[is.na(excl)] <- FALSE
sens_lin_dat <- df[!excl, ] |> dplyr::select(SDS_index, all_of(mv_vars)) |>
  tidyr::drop_na() |> dplyr::mutate(across(where(is.character), factor))
lin_sens <- lm(SDS_index ~ ., data = sens_lin_dat)
tt <- names(coef(lin_fit))
ci_s <- confint(lin_sens)[tt, , drop = FALSE]
p_f  <- lin_sm$coefficients[tt, 4]
p_s  <- summary(lin_sens)$coefficients[tt, 4]
t11b <- tibble(
  Predictor    = unname(LIN_LAB[tt]),
  Beta_full    = round(unname(coef(lin_fit)[tt]), 3),
  CI_low_full  = round(lin_ci[tt, 1], 3), CI_high_full = round(lin_ci[tt, 2], 3),
  p_full       = pv4(p_f),
  Beta_sens    = round(unname(coef(lin_sens)[tt]), 3),
  CI_low_sens  = round(ci_s[, 1], 3), CI_high_sens = round(ci_s[, 2], 3),
  p_sens       = pv4(p_s),
  `dBeta (sens - full)` = round(unname(coef(lin_sens)[tt] - coef(lin_fit)[tt]), 3),
  Sig_flip     = (p_f < 0.05) != (p_s < 0.05))
t11b <- t11b[order(t11b$Predictor != "(Intercept)", t11b$Predictor), ]
tab(t11b, "T11b_sensitivity_outliers")

# Outlier / distribution check (|z| > 3 flagged, never dropped)
out_vars <- list("Age (years)"=df$Age, "Height (cm)"=df$Height, "Weight (kg)"=df$Weight,
                 "BMI (kg/m2)"=df$BMI, "GPA (%)"=df$GPA,
                 "SDS raw score"=df$SDS_raw, "SDS Index"=df$SDS_index)
tab(dplyr::bind_rows(lapply(names(out_vars), function(nm) {
  x <- out_vars[[nm]]; x <- x[!is.na(x)]; zz <- (x - mean(x)) / sd(x)
  tibble(Variable = nm, n = length(x), Mean = round(mean(x), 2), SD = round(sd(x), 2),
         Min = round(min(x), 2), Max = round(max(x), 2),
         `Outliers_|z|>3` = sum(abs(zz) > 3))
})), "T_outliers")

# 5b. Logistic-model assumptions: S5, S6, S7, S8, S8b, S9 -----------------------
# Diagnostics for `logit`, the multivariable model of probable depression
# (SDS index >= 50) fitted above on `mdat`. Each block writes one supplementary
# table; the companion figure is drawn in section 10 (fig_logit_diagnostics).

# (a) Multicollinearity. VIF and tolerance come from auxiliary linear
#     regressions on the model's own design matrix: each column is regressed on
#     all the others, so VIF = 1 / (1 - R2) and tolerance = 1 / VIF.
Xlog     <- model.matrix(logit)
log_cols <- which(colnames(Xlog) != "(Intercept)")   # the intercept VIF is meaningless
vif_log  <- rep(NA_real_, length(log_cols))
for (i in seq_along(log_cols)) {
  j  <- log_cols[i]
  xi <- Xlog[, j]
  Z  <- Xlog[, setdiff(log_cols, j), drop = FALSE]
  vif_log[i] <- 1 / (1 - summary(lm(xi ~ Z))$r.squared)
}
tab(tibble(Predictor      = unname(LIN_LAB[colnames(Xlog)[log_cols]]),
           Tolerance      = round(1 / vif_log, 3),
           VIF            = round(vif_log, 3),
           Interpretation = ifelse(vif_log < 5, "Acceptable", "Collinearity concern")),
    "S5_logit_multicollinearity")

# (b) Linearity of the logit (Box-Tidwell). Every continuous predictor is
#     entered together with its x * ln(x) term; a non-significant interaction
#     term supports a linear relationship with the log-odds.
cont_pred <- c("Age", "GPA")
bt_dat <- mdat
for (v in cont_pred) bt_dat[[paste0(v, "_lnx")]] <- bt_dat[[v]] * log(bt_dat[[v]])
bt_fit <- glm(SDS_depressed ~ ., data = bt_dat, family = binomial)
bt_p   <- summary(bt_fit)$coefficients[paste0(cont_pred, "_lnx"), 4]
tab(tibble(Predictor        = unname(LIN_LAB[cont_pred]),
           Interaction_term = sprintf("%s x ln(%s)", cont_pred, cont_pred),
           p_value          = pv(bt_p),
           Linearity        = ifelse(bt_p > 0.05, "Linearity holds", "Linearity violated")),
    "S6_logit_box_tidwell")

# (c) Influential observations: Cook's distance and leverage (hat values).
#     Cases are flagged, never dropped, in line with the outlier policy above.
cook_log <- cooks.distance(logit)
lev_log  <- hatvalues(logit)
k_log    <- length(coef(logit)) - 1                  # predictor parameters
n_log    <- nrow(mdat)
lev_thr  <- 3 * (k_log + 1) / n_log
cook_at_maxlev <- unname(cook_log[which.max(lev_log)])
tab(tibble(
  Diagnostic = c("Cook's distance", "Leverage (hat value)",
                 "Cook's distance of the highest-leverage case"),
  Observed_value = round(c(max(cook_log), max(lev_log), cook_at_maxlev), 4),
  Threshold = c("< 1.00", sprintf("< 3(k+1)/n = %.4f", lev_thr), "< 1.00"),
  N_exceeding = c(sum(cook_log >= 1), sum(lev_log > lev_thr), NA),
  Interpretation = c(
    ifelse(max(cook_log) < 1, "No influential cases", "Influential cases present"),
    ifelse(max(lev_log) <= lev_thr, "No high-leverage cases", "High-leverage cases present"),
    ifelse(cook_at_maxlev < 1, "Atypical predictor pattern, no undue influence",
           "High-leverage case is also influential"))),
  "S7_logit_influence")

# (d) Calibration: Hosmer-Lemeshow test over deciles of predicted risk.
hl_g   <- 10
hl_grp <- cut(phat, unique(quantile(phat, seq(0, 1, length.out = hl_g + 1))),
              include.lowest = TRUE)
hl_obs <- tapply(mdat$SDS_depressed, hl_grp, sum)
hl_exp <- tapply(phat, hl_grp, sum)
hl_n   <- tapply(phat, hl_grp, length)
hl_chi <- sum((hl_obs - hl_exp)^2 / (hl_exp * (1 - hl_exp / hl_n)))
hl_df  <- length(hl_n) - 2
hl_p   <- pchisq(hl_chi, hl_df, lower.tail = FALSE)
tab(tibble(Statistic = c("Hosmer-Lemeshow chi-square", "Degrees of freedom",
                         "p-value", "Groups (deciles of predicted risk)"),
           Value = c(round(hl_chi, 3), hl_df, pv(hl_p), length(hl_n))),
    "S8_logit_calibration")
tab(tibble(Group           = seq_along(hl_n),
           N               = as.integer(hl_n),
           Observed_events = as.integer(hl_obs),
           Expected_events = round(as.numeric(hl_exp), 2),
           Observed_pct    = round(100 * as.numeric(hl_obs) / as.integer(hl_n), 1),
           Expected_pct    = round(100 * as.numeric(hl_exp) / as.integer(hl_n), 1)),
    "S8b_logit_hl_groups")

# (e) Sample-size adequacy, separation and discrimination.
ev_log  <- sum(mdat$SDS_depressed == 1)
nev_log <- n_log - ev_log
se_log  <- sqrt(diag(vcov(logit)))
tab(tibble(
  Check = c("Observations in the model (n)", "Events (SDS index >= 50)", "Non-events",
            "Predictor parameters (k)", "Events per parameter",
            "Largest coefficient standard error",
            "Fitted probabilities < 0.001 or > 0.999", "Discrimination (AUC)"),
  Value = c(n_log, ev_log, nev_log, k_log,
            round(min(ev_log, nev_log) / k_log, 1), round(max(se_log), 3),
            sum(phat < 0.001 | phat > 0.999), round(auc, 3))),
  "S9_logit_model_checks")

# (f) Robustness of the two points flagged above. Box-Tidwell is repeated after
#     excluding implausible GPA values (<20 on a 0-100 scale, i.e. data-entry
#     errors), and the model is refitted without the high-leverage cases.
#     Nothing is dropped from the published model; both are reported as checks.
GPA_MIN  <- 20
bt_dat2  <- bt_dat[bt_dat$GPA >= GPA_MIN, ]
bt_fit2  <- glm(SDS_depressed ~ ., data = bt_dat2, family = binomial)
bt_p2    <- summary(bt_fit2)$coefficients[paste0(cont_pred, "_lnx"), 4]
tab(tibble(Predictor = unname(LIN_LAB[cont_pred]),
           p_all_cases = pv(bt_p),
           p_excluding_implausible_GPA = pv(bt_p2),
           N_excluded = nrow(bt_dat) - nrow(bt_dat2),
           Linearity = ifelse(bt_p2 > 0.05, "Linearity holds", "Linearity violated")),
    "S6b_logit_box_tidwell_sensitivity")

hi_lev   <- which(lev_log > lev_thr)
nohi_dat <- if (length(hi_lev) == 0) mdat else mdat[-hi_lev, ]   # guard: empty index drops all rows
fit_nohi <- glm(SDS_depressed ~ ., data = nohi_dat, family = binomial)
tt_log   <- names(coef(logit))
p_full_l <- summary(logit)$coefficients[tt_log, 4]
p_nohi_l <- summary(fit_nohi)$coefficients[tt_log, 4]
tab(tibble(Predictor = unname(LIN_LAB[tt_log]),
           OR_full   = round(unname(exp(coef(logit)[tt_log])), 3),
           p_full    = pv(p_full_l),
           OR_excluding_high_leverage = round(unname(exp(coef(fit_nohi)[tt_log])), 3),
           p_excluding_high_leverage  = pv(p_nohi_l),
           N_excluded = length(hi_lev),
           Sig_flip   = (p_full_l < 0.05) != (p_nohi_l < 0.05)),
    "S10_logit_influence_sensitivity")

# 6. Factor analyses: T4/T5 (SDS, full + reduced), T_belief_efa ------------------
run_efa <- function(items) {
  M <- sds_scored[, items, drop = FALSE]
  kmo  <- psych::KMO(M)
  bart <- psych::cortest.bartlett(cor(M), n = nrow(M))
  fa   <- psych::fa(M, nfactors = 2, fm = "pa", rotate = "promax")
  load <- as.data.frame(unclass(fa$loadings)); names(load) <- c("Factor1", "Factor2")
  load <- tibble::rownames_to_column(round(load, 3), "Item")
  load$Communality <- round(fa$communality, 3)
  sa <- tibble(Statistic = c("KMO measure of sampling adequacy",
                             "Bartlett's chi-square", "Bartlett's df",
                             "Bartlett's p-value", "N items"),
               Value = c(round(kmo$MSA, 3), round(bart$chisq, 2),
                         bart$df, pv(bart$p.value), length(items)))
  ve <- as.data.frame(fa$Vaccounted)
  ve <- tibble(Factor = c("Factor1", "Factor2"),
               SS_Loadings    = round(as.numeric(ve["SS loadings", ]), 3),
               Proportion_Var = round(as.numeric(ve["Proportion Var", ]), 3),
               Cumulative_Var = round(as.numeric(ve["Cumulative Var", ]), 3))
  list(load = load, sa = sa, ve = ve, kmo = kmo$MSA, comm = fa$communality)
}
efa_full <- run_efa(sds)
tab(efa_full$load, "T5_factor_loadings")
tab(efa_full$ve,   "T5b_variance_explained")
tab(efa_full$sa,   "T4_sampling_adequacy")

COMM_CUT <- 0.30
dropped <- sds[efa_full$comm < COMM_CUT]; kept <- sds[efa_full$comm >= COMM_CUT]
efa_red <- run_efa(kept)
tab(efa_red$load, "T5c_factor_loadings_reduced")
tab(efa_red$ve,   "T5d_variance_explained_reduced")
tab(efa_red$sa,   "T4b_sampling_adequacy_reduced")
tab(tibble(Dropped_item = dropped,
           Communality_full = round(efa_full$comm[dropped], 3)), "T5c_dropped_items")

belief <- c("AW01_Disease","AW02_NotSpontaneous","AW03_Genetic","AW04_FemaleRisk",
            "AW05_Hormones","AW06_ChronicDisease","AW07_Personality",
            "AW09_SubstanceAbuse","AW12_Relapse","AW16_NotSelfTreat")
bdat <- df[belief] |> tidyr::drop_na()
kmo_b  <- psych::KMO(bdat)
bart_b <- psych::cortest.bartlett(cor(bdat), n = nrow(bdat))
fa_b   <- psych::fa(bdat, nfactors = 3, fm = "pa", rotate = "promax")
tab(as.data.frame(unclass(fa_b$loadings)) |> round(3) |>
      tibble::rownames_to_column("Item") |>
      mutate(Communality = round(fa_b$communality, 3),
             KMO_overall = round(kmo_b$MSA, 3),
             Bartlett_p  = pv(bart_b$p.value)), "T_belief_efa")

# 7. Awareness & sources: T_awareness_items, T_sources, T_awareness_vs_sds ------

# Composite awareness score: KR-20 reliability and the tertile band cut-points.
# KR-20 is Cronbach's alpha for dichotomous items; it is the coefficient quoted
# for the awareness composite in the Results.
aw_mat <- as.matrix(df[AW_KEY$new])
k_aw   <- ncol(aw_mat)
p_aw   <- colMeans(aw_mat)
kr20   <- k_aw / (k_aw - 1) * (1 - sum(p_aw * (1 - p_aw)) / var(rowSums(aw_mat)))
tab(tibble(
  Statistic = c("KR-20 (Kuder-Richardson formula 20)", "N items", "Median score (Q1-Q3)",
                "Tertile cut-points (low/moderate, moderate/high)"),
  Value = c(round(kr20, 3), k_aw,
            sprintf("%.0f (%.0f-%.0f)", median(df$AwarenessScore),
                    quantile(df$AwarenessScore, .25), quantile(df$AwarenessScore, .75)),
            sprintf("%.0f, %.0f", qs[1], qs[2]))),
  "T_awareness_reliability")
tab(dplyr::bind_rows(lapply(seq_len(nrow(AW_KEY)), function(i) {
  k <- sum(df[[AW_KEY$new[i]]], na.rm = TRUE)
  ci <- binom::binom.wilson(k, n_total)
  data.frame(Item = AW_KEY$col[i], correct = AW_KEY$correct[i], k = k,
             pct = round(k / n_total * 100, 1),
             CI_low_pct = round(ci$lower * 100, 1),
             CI_high_pct = round(ci$upper * 100, 1))
})) |> arrange(desc(pct)), "T_awareness_items")

src_tbl <- tibble::tribble(
  ~Source,              ~n,
  "Mass media",         sum(df$Source_Mass_Media),
  "Doctors",            sum(df$Source_Doctors),
  "Family / relatives", sum(df$Source_Family),
  "Social media",       sum(df$Source_Social_Media),
  "University courses", sum(df$Source_University_Courses),
  "Other",              sum(df$Source_Other)) |>
  mutate(`%` = round(n / n_total * 100, 1),
         CI_low_pct  = round(binom::binom.wilson(n, n_total)$lower * 100, 1),
         CI_high_pct = round(binom::binom.wilson(n, n_total)$upper * 100, 1)) |>
  arrange(desc(n))
tab(src_tbl, "T_sources")

sp <- suppressWarnings(cor.test(df$AwarenessScore, df$SDS_index, method = "spearman"))
chi_aw <- suppressWarnings(chisq.test(table(df$AwarenessLevel, df$SDS_depressed)))
tab(tibble(Test = c("Spearman: Awareness vs SDS Index",
                    "Chi-square: AwarenessLevel x Depressed"),
           Statistic = c(round(unname(sp$estimate), 3), round(unname(chi_aw$statistic), 2)),
           p = pv(c(sp$p.value, chi_aw$p.value))), "T_awareness_vs_sds")

# 8. Subgroups, interactions, mediation: T13, T13b, T_moderation, T_mediation ---
strata <- c(Sex = "Sex", AgeGroup = "Age group",
            Employment = "Employment status", AcademicYear = "Academic year")
tab(dplyr::bind_rows(lapply(names(strata), function(v) {
  lv <- if (is.factor(df[[v]])) levels(df[[v]]) else sort(unique(na.omit(df[[v]])))
  dplyr::bind_rows(lapply(lv, function(g) {
    s <- df[!is.na(df[[v]]) & df[[v]] == g, ]
    ss <- s$SDS_index[!is.na(s$SDS_index)]
    if (length(ss) < 20) return(NULL)
    tibble(Stratifier = strata[[v]], Group = as.character(g), n = length(ss),
           Mean = round(mean(ss), 2), SD = round(sd(ss), 2),
           Median = round(median(ss), 1),
           Depressed_n = sum(s$SDS_depressed == 1, na.rm = TRUE),
           Depressed_pct = round(mean(s$SDS_depressed == 1, na.rm = TRUE) * 100, 1))
  }))
})), "T13_subgroup")

# Type-II interaction F = additive vs full model comparison
int_pairs <- list(c("Sex","AgeGroup"), c("Sex","Employment"),
                  c("Sex","ChronicDisease"), c("Sex","PriorDepressionDx"))
tab(dplyr::bind_rows(lapply(int_pairs, function(pr) {
  d <- df[complete.cases(df[, c("SDS_index", pr)]), c("SDS_index", pr)]
  names(d) <- c("y","A","B")
  cmp <- anova(lm(y ~ factor(A) + factor(B), d), lm(y ~ factor(A) * factor(B), d))
  tibble(Interaction = sprintf("%s x %s", LBL(pr[1]), LBL(pr[2])),
         F = round(cmp$F[2], 2), p = pv4(cmp$`Pr(>F)`[2]))
})), "T13b_interactions")

d <- df[complete.cases(df[, c("SDS_index","Sex","ChronicDisease")]),
        c("SDS_index","Sex","ChronicDisease")]
names(d) <- c("y","A","B")
f_full <- lm(y ~ factor(A) * factor(B), d); f_add <- lm(y ~ factor(A) + factor(B), d)
ss  <- c(deviance(lm(y ~ factor(B), d)) - deviance(f_add),
         deviance(lm(y ~ factor(A), d)) - deviance(f_add),
         deviance(f_add) - deviance(f_full))
mse <- deviance(f_full) / df.residual(f_full); Fv <- ss / mse
tab(tibble(Term = c("Sex","Chronic disease","Sex x Chronic disease","Residual"),
           sum_sq = round(c(ss, deviance(f_full)), 3),
           df = c(1, 1, 1, df.residual(f_full)),
           F = c(round(Fv, 3), NA),
           `PR(>F)` = c(round(pf(Fv, 1, df.residual(f_full), lower.tail = FALSE), 3), NA)),
    "T_moderation")

# Mediation (pingouin convention): logistic a-path, unadjusted Y~M row,
# indirect = bootstrapped a*b (seed 0, B = 1000; a is on the log-odds scale)
md_d <- df[complete.cases(df[, c("Sex","PriorDepressionDx","SDS_index")]), ]
mx <- as.integer(md_d$Sex == "Female")
mm <- as.integer(md_d$PriorDepressionDx == "Yes")
my <- md_d$SDS_index
a_fit <- glm(mm ~ mx, family = binomial)
b_fit <- lm(my ~ mm); full_fit <- lm(my ~ mx + mm); tot_fit <- lm(my ~ mx)
coef_row <- function(fit, term, wald = FALSE) {
  cf <- summary(fit)$coefficients
  ci <- if (wald) confint.default(fit) else confint(fit)
  c(coef = cf[term, 1], se = cf[term, 2], pval = cf[term, 4],
    lo = ci[term, 1], hi = ci[term, 2])
}
set.seed(0)
ind_boot <- replicate(1000, {
  i <- sample.int(length(my), replace = TRUE)
  coef(suppressWarnings(glm(mm[i] ~ mx[i], family = binomial)))[2] *
    coef(lm(my[i] ~ mx[i] + mm[i]))[3]
})
rows <- rbind(`M ~ X` = coef_row(a_fit, "mx", wald = TRUE),
              `Y ~ M` = coef_row(b_fit, "mm"),
              Total   = coef_row(tot_fit, "mx"),
              Direct  = coef_row(full_fit, "mx"),
              Indirect = c(mean(ind_boot), sd(ind_boot),
                           2 * min(mean(ind_boot <= 0), mean(ind_boot >= 0)),
                           quantile(ind_boot, .025), quantile(ind_boot, .975)))
tab(tibble(path = rownames(rows),
           coef = round(rows[, 1], 3), se = round(rows[, 2], 3),
           pval = round(rows[, 3], 4),
           CI2.5 = round(rows[, 4], 3), CI97.5 = round(rows[, 5], 3),
           sig = ifelse(rows[, 4] > 0 | rows[, 5] < 0, "Yes", "No")), "T_mediation")

# 9. Supplementary data quality: S1, S2, T_implausible, T10 ---------------------
s1_vars <- c("Age","Sex","MaritalStatus","Residence","SES_g","University",
             "Faculty","AcademicYear","GPA","Height","Weight","BMI",
             "ParentHealthcare","Smoker","Alcohol","Employment","ChronicDisease",
             "Siblings","FamilyRelationship","PriorDepressionDx","KnowsDepressed",
             "PriorMentalDx", sds, "SDS_raw","SDS_index","SDS_category")
tab(tibble(Variable = unname(LBL(s1_vars)),
           Missing_n = sapply(s1_vars, function(v) sum(is.na(df[[v]]))),
           Missing_pct = round(sapply(s1_vars, function(v) mean(is.na(df[[v]])) * 100), 2)),
    "S1_missing")

tab(dplyr::bind_rows(lapply(sds, function(it) {
  x <- df[[it]]; row <- list(Item = it)
  for (k in 1:4)
    row[[sprintf("Score %d", k)]] <- sprintf("%d (%.1f%%)",
      sum(x == k, na.rm = TRUE), sum(x == k, na.rm = TRUE) / nrow(df) * 100)
  row$Missing <- sprintf("%d (%.1f%%)", sum(is.na(x)), mean(is.na(x)) * 100)
  as.data.frame(row, check.names = FALSE)
})), "S2_item_distribution")

tab(tibble(Check = c("Age_implausible","Height_implausible","Weight_implausible",
                     "BMI_implausible","GPA_implausible"),
           n = c(sum(df$Age < 16 | df$Age > 60, na.rm = TRUE),
                 sum(df$Height_implausible == 1, na.rm = TRUE),
                 sum(df$Weight < 30 | df$Weight > 200, na.rm = TRUE),
                 sum(df$BMI_implausible == 1, na.rm = TRUE),
                 sum(df$GPA < 0 | df$GPA > 100, na.rm = TRUE))), "T_implausible")

cont_pred <- c("Age","Height","Weight","BMI","GPA","AcademicYearN")
tab(dplyr::bind_rows(lapply(cont_pred, function(v) {
  s <- complete.cases(df[[v]], df$SDS_index)
  ct <- suppressWarnings(cor.test(df[[v]][s], df$SDS_index[s], method = "spearman"))
  tibble(Variable = unname(LBL(v)), Method = "Spearman",
         Coefficient = round(unname(ct$estimate), 3), p = pv4(ct$p.value), n = sum(s))
})), "T10_correlation")

# 10. Figures --------------------------------------------------------------------
## fig1: severity bar
sev_df <- t6b[1:4, ]; sev_df$Band <- factor(sev_df$Band, levels = bands)
ggplot(sev_df, aes(Band, n, fill = Band)) +
  geom_col(color = "black", width = .7) +
  geom_text(aes(label = sprintf("%d\n(%.1f%%)", n, pct)), vjust = -.2, size = 3.4) +
  scale_fill_manual(values = c("#2ca02c","#ffbf00","#ff7f0e","#d62728")) +
  scale_y_continuous(expand = expansion(mult = c(0, .18))) +
  labs(#title = sprintf("Depression severity (Zung SDS) — N = %d", n_total),
       x = "Severity band", y = "Number of respondents") +
  theme(legend.position = "none")
gsav("fig1_severity_bar", 6.5, 4.3)

## fig2: SDS Index histogram
m <- mean(df$SDS_index); md <- median(df$SDS_index); sd_ <- sd(df$SDS_index)
ggplot(df, aes(SDS_index)) +
  geom_histogram(bins = 25, fill = "#3b6ea7", color = "black") +
  geom_vline(xintercept = m,  color = "black", linetype = "dashed") +
  geom_vline(xintercept = md, color = "firebrick", linetype = "dotdash") +
  geom_vline(xintercept = c(50,60,70), color = "grey40", linetype = "dotted") +
  labs(#title = sprintf("Distribution of the Zung SDS Index (N = %d)", n_total),
       #subtitle = sprintf("Mean %.2f, Median %.1f, SD %.2f  (cut-offs 50/60/70)", m, md, sd_),
       x = "SDS Index", y = "Number of respondents")
gsav("fig2_sds_histogram", 7.5, 4.6)

## fig_scree_parallel
pa <- psych::fa.parallel(sds_scored, fm = "pa", fa = "fa", plot = FALSE)
png(file.path(FIG, "fig_scree_parallel.png"), 2100, 1200, res = 300, bg = "white")
plot(1:20, pa$fa.values, type = "b", pch = 19, xlab = "Factor",
     ylab = "Eigenvalue" #main = "Scree plot with parallel analysis"
     )
lines(1:20, pa$fa.sim, type = "b", pch = 1, lty = 2, col = "firebrick")
abline(h = 0, col = "grey70")
legend("topright", bty = "n", legend = c("Observed (PAF)", "Random (parallel)"),
       pch = c(19,1), lty = c(1,2), col = c("black","firebrick"))
dev.off()

## fig_assoc_severity (+ T_assoc_severity): signed Spearman rho / Cramer's V
sev <- df$SDS_sev_ord
sp_rho <- function(x) { s <- complete.cases(x, sev)
  as.numeric(suppressWarnings(cor(x[s], sev[s], method = "spearman"))) }
cramv <- function(f) { s <- complete.cases(f, df$SDS_category)
  as.numeric(rcompanion::cramerV(table(f[s], df$SDS_category[s]))) }
enc <- list(
  "Age (years)"  = sp_rho(df$Age),  "Height (cm)" = sp_rho(df$Height),
  "Weight (kg)"  = sp_rho(df$Weight), "BMI (kg/m^2)" = sp_rho(df$BMI),
  "GPA (%)"      = sp_rho(df$GPA),
  "Socioeconomic status" = sp_rho(as.integer(factor(df$SES_g,
                              levels = c("Low","Medium","Good","Excellent")))),
  "Academic year" = sp_rho(df$AcademicYearN),
  "Age group"     = sp_rho(as.integer(df$AgeGroup)),
  "Parent works in healthcare" = sp_rho(as.integer(factor(df$ParentHealthcare,
                              levels = c("No","One parent","Both parents"))) - 1),
  "Sex"          = sp_rho(as.integer(df$Sex == "Female")),
  "Residence"    = sp_rho(as.integer(df$Residence == "Rural")),
  "University"   = sp_rho(as.integer(df$University == "Other")),
  "Smoking status" = sp_rho(as.integer(df$Smoker == "Smoker")),
  "Alcohol consumption" = sp_rho(as.integer(df$Alcohol == "Yes")),
  "Has siblings" = sp_rho(as.integer(df$Siblings == "Yes")),
  "Good family relationship" = sp_rho(as.integer(df$FamilyRelationship == "Yes")),
  "Prior depression diagnosis" = sp_rho(as.integer(df$PriorDepressionDx == "Yes")),
  "Knows someone with depression" = sp_rho(as.integer(df$KnowsDepressed == "Yes")),
  "Prior mental-illness diagnosis" = sp_rho(as.integer(df$PriorMentalDx == "Yes")),
  "Chronic disease" = sp_rho(as.integer(df$ChronicDisease == "Yes")))
nominal <- list(
  "Marital status"    = cramv(factor(df$MaritalStatus)),
  "Faculty"           = cramv(factor(df$Faculty)),
  "Employment status" = cramv(factor(df$Employment)))
assoc <- dplyr::bind_rows(
  tibble(Variable = names(enc), value = unlist(enc), kind = "rho"),
  tibble(Variable = names(nominal), value = unlist(nominal), kind = "v")) |>
  mutate(absval = abs(value)) |> arrange(absval) |>
  mutate(Variable = forcats::fct_inorder(Variable),
         col = dplyr::case_when(kind == "v" ~ "Cramer's V",
                                value >= 0 ~ "Positive rho", TRUE ~ "Negative rho"))
tab(dplyr::select(assoc, Variable, value, kind), "T_assoc_severity")

pad <- max(assoc$absval) * 0.18
ggplot(assoc, aes(value, Variable, fill = col)) +
  geom_col(color = "black", width = .72) +
  geom_vline(xintercept = 0, color = "black", linewidth = .4) +
  geom_text(aes(label = sprintf("%.2f", value),
                hjust = ifelse(value >= 0, -0.15, 1.15)), size = 2.9) +
  scale_fill_manual(values = c("Positive rho" = "#1f77b4",
                               "Negative rho" = "#d62728",
                               "Cramer's V"   = "#7f7f7f"),
                    breaks = c("Positive rho","Negative rho","Cramer's V"),
                    labels = c("Positive rho (-> higher severity)",
                               "Negative rho (-> lower severity)",
                               "Cramer's V (nominal, unsigned)"), name = NULL) +
  scale_x_continuous(limits = c(min(assoc$value) - pad, max(assoc$value) + pad),
                     expand = expansion(mult = 0)) +
  labs(#title = "Association of each variable with SDS severity",
       #subtitle = sprintf("Syrian university students (N = %d)", n_total),
       x = "Spearman rho (blue +, red -)  /  Cramer's V (grey, unsigned)", y = NULL) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = NA),
        legend.text = element_text(size = 8))
gsav("fig_assoc_severity", 8.5, 9)

## fig10: adjusted OR forest plot
or_df <- broom::tidy(logit, conf.int = TRUE, exponentiate = TRUE) |>
  filter(term != "(Intercept)") |> arrange(estimate) |>
  mutate(term = forcats::fct_inorder(term))
ggplot(or_df, aes(estimate, term)) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = .18, color = "grey40") +
  geom_point(size = 2) + scale_x_log10() +
  labs(#title = "Adjusted odds ratios for screening-positive depression (SDS >= 50)",
       #subtitle = "Multivariable logistic regression",
       x = "Adjusted OR (95% CI, log scale)", y = NULL)
gsav("fig10_or_forest_logistic", 8.5, 6)

## fig3 + fig_corr_all: Spearman heatmaps
gg_heat <- function(M, txt = 2.9, title = NULL) {
  h <- as.data.frame(as.table(M)); names(h) <- c("V1","V2","r")
  h$V1 <- factor(h$V1, levels = rownames(M))
  h$V2 <- factor(h$V2, levels = rev(colnames(M)))
  ggplot(h, aes(V1, V2, fill = r)) +
    geom_tile(color = "white", linewidth = .3) +
    geom_text(aes(label = sprintf("%.2f", r)), size = txt) +
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                         limits = c(-1, 1), name = "Spearman rho") +
    coord_fixed() + labs(title = title, x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8),
          axis.text.y = element_text(size = 8))
}
hm_vars <- c(cont_pred, "SDS_index")
cm6 <- cor(df[hm_vars], method = "spearman", use = "pairwise.complete.obs")
rownames(cm6) <- colnames(cm6) <- unname(LBL(hm_vars))
gsav("fig3_corr_heatmap", 7, 5.5,
     gg_heat(cm6, 3.1))
             #"Spearman correlation: continuous variables and SDS Index"))

enc_all <- data.frame(check.names = FALSE,
  "SDS Index"    = df$SDS_index,
  "SDS severity" = df$SDS_sev_ord,
  "Age (years)"  = df$Age, "Height (cm)" = df$Height, "Weight (kg)" = df$Weight,
  "BMI (kg/m2)"  = df$BMI, "GPA (%)" = df$GPA,
  "Socioeconomic status" = as.integer(factor(df$SES_g,
                             levels = c("Low","Medium","Good","Excellent"))),
  "Academic year" = df$AcademicYearN,
  "Age group"     = as.integer(df$AgeGroup),
  "Parent works in healthcare" = as.integer(factor(df$ParentHealthcare,
                             levels = c("No","One parent","Both parents"))) - 1,
  "Sex"        = as.integer(df$Sex == "Female"),
  "Residence"  = as.integer(df$Residence == "Rural"),
  "University" = as.integer(df$University == "Other"),
  "Smoking status" = as.integer(df$Smoker == "Smoker"),
  "Alcohol consumption" = as.integer(df$Alcohol == "Yes"),
  "Has siblings" = as.integer(df$Siblings == "Yes"),
  "Good family relationship" = as.integer(df$FamilyRelationship == "Yes"),
  "Prior depression diagnosis" = as.integer(df$PriorDepressionDx == "Yes"),
  "Knows someone with depression" = as.integer(df$KnowsDepressed == "Yes"),
  "Prior mental-illness diagnosis" = as.integer(df$PriorMentalDx == "Yes"),
  "Chronic disease" = as.integer(df$ChronicDisease == "Yes"))
cm_all <- cor(enc_all, method = "spearman", use = "pairwise.complete.obs")
gsav("fig_corr_all_heatmap", 12, 10,
     gg_heat(cm_all, 1.9))
             #"Spearman correlation matrix - all encodable variables, SDS Index & severity"))

## fig_normality: Q-Q + density vs normal
p_qq <- ggplot(data.frame(y = df$SDS_index), aes(sample = y)) +
  stat_qq(size = .8, alpha = .5, color = "#3b6ea7") + stat_qq_line(color = "red") +
  labs(title = "Q-Q plot: SDS Index", x = "Theoretical quantiles", y = "Ordered values")
p_dens <- ggplot(data.frame(x = df$SDS_index), aes(x)) +
  geom_density(fill = "#3b6ea7", alpha = .5) +
  stat_function(fun = dnorm, args = list(mean = m, sd = sd_),
                color = "red", linetype = "dashed") +
  labs(title = "Density vs normal reference", x = "SDS Index", y = "Density")
gsav("fig_normality", 10, 4, gridExtra::arrangeGrob(p_qq, p_dens, ncol = 2))

## fig_outliers: box + histogram per continuous variable
out_plots <- list()
for (nm in names(out_vars)) {
  x <- out_vars[[nm]]; x <- x[!is.na(x)]
  out_plots[[paste0("b_", nm)]] <-
    ggplot(data.frame(y = x), aes(x = "", y = y)) +
    geom_boxplot(fill = "#4C72B0") + labs(title = nm, x = NULL, y = NULL) +
    theme(plot.title = element_text(size = 9))
  out_plots[[paste0("h_", nm)]] <-
    ggplot(data.frame(x = x), aes(x)) +
    geom_histogram(bins = 20, fill = "#55A868", color = "white") +
    labs(x = NULL, y = NULL)
}
ord <- c(paste0("b_", names(out_vars)), paste0("h_", names(out_vars)))
gsav("fig_outliers", 21, 6, gridExtra::arrangeGrob(grobs = out_plots[ord], nrow = 2))

## fig4: ROC curve
roc_obj <- pROC::roc(mdat$SDS_depressed, phat, quiet = TRUE)
rd <- data.frame(fpr = 1 - roc_obj$specificities, tpr = roc_obj$sensitivities)
rd <- rd[order(rd$fpr, rd$tpr), ]
ggplot(rd, aes(fpr, tpr)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_line(color = "#3b6ea7", linewidth = 1) +
  annotate("text", x = .65, y = .1, label = sprintf("AUC = %.3f", auc), size = 3.6) +
  labs(#title = "ROC curve: logistic prediction of depression (SDS >= 50)",
       x = "1 - Specificity", y = "Sensitivity")
gsav("fig4_roc", 5.5, 5)

## fig_residual_diagnostics + fig_forest_linear
diag_df <- data.frame(fitted = fitted(lin_fit), resid = residuals(lin_fit),
                      sl = sqrt(abs(rstandard(lin_fit))))
p_rf <- ggplot(diag_df, aes(fitted, resid)) +
  geom_point(alpha = .35, size = .9) + geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals vs fitted", x = "Fitted", y = "Residual")
p_rq <- ggplot(diag_df, aes(sample = resid)) +
  stat_qq(size = .8, alpha = .5) + stat_qq_line(color = "red") +
  labs(title = "Q-Q plot of residuals", x = "Theoretical quantiles", y = "Residuals")
p_rh <- ggplot(diag_df, aes(resid)) +
  geom_histogram(bins = 30, fill = "#3b6ea7", color = "white") +
  labs(title = "Histogram of residuals", x = "Residual", y = "Count")
p_sl <- ggplot(diag_df, aes(fitted, sl)) +
  geom_point(alpha = .35, size = .9) +
  labs(title = "Scale-Location", x = "Fitted", y = "sqrt(|standardized residual|)")
gsav("fig_residual_diagnostics", 10, 8,
     gridExtra::arrangeGrob(p_rf, p_rq, p_rh, p_sl, ncol = 2))

## fig_logit_diagnostics: logistic-model assumption checks
diag_log <- data.frame(phat = phat, sres = rstudent(logit), cook = cook_log,
                       lev = lev_log, id = seq_len(n_log),
                       obs = factor(mdat$SDS_depressed, 0:1,
                                    c("Not screen-positive", "Screen-positive")))
p_ls <- ggplot(diag_log, aes(phat, sres, color = obs)) +
  geom_point(alpha = .45, size = .9) +
  geom_hline(yintercept = 0, color = "red") +
  geom_hline(yintercept = c(-3, 3), color = "grey50", linetype = "dashed") +
  scale_color_manual(values = c("#3b6ea7", "#c0504d"), name = NULL) +
  labs(title = "Studentized residuals",
       x = "Predicted probability", y = "Studentized residual") +
  theme(legend.position = "bottom")
p_lc <- ggplot(diag_log, aes(id, cook)) +
  geom_segment(aes(xend = id, yend = 0), color = "grey55", linewidth = .3) +
  labs(title = "Cook's distance", x = "Observation", y = "Cook's D")
p_ll <- ggplot(diag_log, aes(id, lev)) +
  geom_point(alpha = .35, size = .8) +
  geom_hline(yintercept = lev_thr, color = "red", linetype = "dashed") +
  labs(title = sprintf("Leverage (threshold = %.4f)", lev_thr),
       x = "Observation", y = "Hat value")
gsav("fig_logit_diagnostics", 13, 4.5,
     gridExtra::arrangeGrob(p_ls, p_lc, p_ll, ncol = 3))

pt <- t11[t11$Predictor != "(Intercept)", ]
pt <- pt[order(pt$Beta), ]; pt$Predictor <- forcats::fct_inorder(pt$Predictor)
ggplot(pt, aes(Beta, Predictor)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = .18, color = "grey40") +
  geom_point(size = 2) +
  labs(title = "Linear regression: predictors of SDS Index",
       x = "Beta (95% CI) - SDS Index", y = NULL)
gsav("fig_forest_linear", 8, max(4, 0.34 * nrow(pt)))

## fig5: awareness score histogram
aw <- df$AwarenessScore[!is.na(df$AwarenessScore)]
aw_m <- mean(aw); aw_md <- median(aw); aw_sd <- sd(aw)
aw_ymax <- max(table(factor(aw, levels = 0:17)))
ggplot(data.frame(x = aw), aes(x)) +
  geom_histogram(binwidth = 1, boundary = -0.5, fill = "#4C72B0", color = "black") +
  geom_vline(xintercept = aw_m,  color = "black", linetype = "dashed") +
  geom_vline(xintercept = aw_md, color = "firebrick", linetype = "dotdash") +
  annotate("label", x = 0, y = aw_ymax * 1.15, hjust = 0, vjust = 1, size = 3,
           label = sprintf("N = %d\nMean = %.2f\nMedian = %.1f\nSD = %.2f\nRange = %d-%d",
                           length(aw), aw_m, aw_md, aw_sd, min(aw), max(aw))) +
  scale_x_continuous(breaks = 0:17) +
  scale_y_continuous(limits = c(0, aw_ymax * 1.2)) +
  labs(#title = sprintf("Distribution of the depression-awareness score (N = %d)", length(aw)),
      # subtitle = "Dashed = mean, dot-dash = median",
       x = "Awareness score (0-17 informed items)", y = "Number of respondents")
gsav("fig5_awareness_histogram", 7.5, 4.8)

## fig6: awareness vs SDS scatter (deterministic jitter)
sub6 <- df[complete.cases(df[, c("AwarenessScore","SDS_index")]),
           c("AwarenessScore","SDS_index")]
set.seed(0)
sub6$x_jit <- sub6$AwarenessScore + runif(nrow(sub6), -0.18, 0.18)
fit6 <- lm(SDS_index ~ AwarenessScore, data = sub6)
ggplot(sub6, aes(x_jit, SDS_index)) +
  geom_point(alpha = .35, size = 1.2, color = "#3b6ea7") +
  geom_abline(intercept = coef(fit6)[1], slope = coef(fit6)[2],
              color = "firebrick", linewidth = 1) +
  geom_hline(yintercept = 50, color = "grey40", linetype = "dotted") +
  scale_x_continuous(breaks = 0:17) +
  labs(#title = "Depression-awareness score vs Zung SDS Index",
       subtitle = sprintf("Spearman rho = %.2f (p = %s; N = %d); red line = OLS fit (slope = %.2f); dotted = SDS cut-off 50",
                          sp$estimate, pv(sp$p.value), nrow(sub6), coef(fit6)[2]),
       x = "Awareness score (0-17 informed items)", y = "SDS Index")
gsav("fig6_awareness_vs_sds_scatter", 7.5, 5.2)

## fig7: information sources bar
n_src <- sum(!is.na(df[[SRC_COL]]))
src_fig <- src_tbl |> mutate(pct = round(n / n_src * 100, 1)) |>
  arrange(n) |> mutate(Source = forcats::fct_inorder(Source))
ggplot(src_fig, aes(n, Source, fill = Source)) +
  geom_col(color = "black", width = .72) +
  geom_text(aes(label = sprintf("%d (%.1f%%)", n, pct)), hjust = -.08, size = 3.1) +
  scale_fill_viridis_d(option = "mako", begin = .25, end = .85) +
  scale_x_continuous(limits = c(0, max(src_fig$n) * 1.2), expand = expansion(mult = 0)) +
  labs(#title = "Information sources on depression",
       #subtitle = sprintf("Multi-response - denominator N = %d respondents", n_src),
       x = "Number of respondents who reported the source", y = NULL) +
  theme(legend.position = "none")
gsav("fig7_information_sources_bar", 8, 4.8)

## fig9: diverging informed/uninformed bar
AW_SHORT <- c(
  AW01_Disease           = "Depression is a disease/illness (Yes)",
  AW02_NotSpontaneous    = "Depression does NOT heal spontaneously (No, it cannot)",
  AW03_Genetic           = "Genetic factor (Yes)",
  AW04_FemaleRisk        = "Females more susceptible (Yes)",
  AW05_Hormones          = "Hormone-related (Yes)",
  AW06_ChronicDisease    = "Linked to chronic disease (Yes)",
  AW07_Personality       = "Linked to personality (Yes)",
  AW08_Symptoms          = "Know symptoms (Yes)",
  AW09_SubstanceAbuse    = "Linked to substance abuse (Yes)",
  AW10_Treatment         = "Know how it's treated (Yes)",
  AW11_AntidepressantUse = "Know how to use antidepressants (Yes)",
  AW12_Relapse           = "Can relapse (Yes)",
  AW13_SideEffects       = "Know treatment side effects (Yes)",
  AW14_HealthEffect      = "Know effect on health (Yes)",
  AW15_Prevalence        = "Know prevalence in students (I know)",
  AW16_NotSelfTreat      = "NOT self-treatable (No)",
  AW17_Prevention        = "Know prevention methods (I know)")
divg <- dplyr::bind_rows(lapply(names(AW_SHORT), function(v) {
  k <- sum(df[[v]], na.rm = TRUE)
  tibble(Item = AW_SHORT[[v]], Informed = k / n_total * 100,
         Uninformed = -(n_total - k) / n_total * 100)
})) |> arrange(Informed) |> mutate(Item = forcats::fct_inorder(Item))
ggplot(divg) +
  geom_col(aes(Informed, Item, fill = "Informed answer"), color = "black") +
  geom_col(aes(Uninformed, Item, fill = "Uninformed answer"), color = "black") +
  geom_vline(xintercept = 0, color = "black", linewidth = .4) +
  geom_text(aes(Informed + 1.5, Item, label = sprintf("%.0f%%", Informed)),
            hjust = 0, size = 2.8) +
  geom_text(aes(Uninformed - 1.5, Item, label = sprintf("%.0f%%", -Uninformed)),
            hjust = 1, size = 2.8) +
  scale_fill_manual(values = c("Informed answer" = "#2ca02c",
                               "Uninformed answer" = "#d62728"), name = NULL) +
  scale_x_continuous(limits = c(-105, 105), breaks = seq(-100, 100, 25),
                     labels = function(b) sprintf("%d%%", abs(b))) +
  labs(title = "Knowledge / belief items - informed vs uninformed responses",
       subtitle = sprintf("Syrian university students (N = %d)", n_total),
       x = "Share of respondents", y = NULL) +
  theme(legend.position = "bottom")
gsav("fig9_awareness_items_diverging", 10, 7.5)

## fig8 + fig11: 3x3 group panels
PANEL_GROUPS <- list(
  list("Sex",               c("Female","Male"),                                   "Sex"),
  list("AcademicYear",      c("First","Second","Third","Fourth","Fifth","Sixth"), "Academic year"),
  list("SES_g",             c("Low","Medium","Good","Excellent"),                 "Socio-economic status"),
  list("Faculty",           c("Medicine","Dentistry","Pharmacy"),                 "Faculty"),
  list("BMI_WHO",           c("Underweight","Normal","Overweight","Obese"),       "WHO BMI category"),
  list("ChronicDisease",    c("No","Yes"),                                        "Chronic disease"),
  list("PriorDepressionDx", c("No","Yes"),                                        "Prior depression diagnosis"),
  list("Smoker",            c("Non-smoker","Smoker"),                             "Smoking status"),
  list("Residence",         c("Urban","Rural"),                                   "Residence"))

mk_panel <- function(var, lv, ttl, outcome, ylim, cutoff = NA) {
  d <- df[, c(var, outcome)]; names(d) <- c("g", "y")
  d$g <- as.character(d$g)
  d <- d[!is.na(d$g) & !is.na(d$y) & d$g %in% lv, ]
  keep <- lv[sapply(lv, function(l) sum(d$g == l)) >= 3]
  d <- d[d$g %in% keep, ]
  n_by <- sapply(keep, function(l) sum(d$g == l))
  grps <- lapply(keep, function(l) d$y[d$g == l])
  if (length(grps) == 2) {
    tst <- "Mann-Whitney"
    p <- suppressWarnings(wilcox.test(grps[[1]], grps[[2]]))$p.value
  } else if (length(grps) > 2) {
    tst <- "Kruskal-Wallis"; p <- kruskal.test(grps)$p.value
  } else { tst <- "n/a"; p <- NA }
  p_str <- if (is.na(p)) "n/a" else if (p < 0.001) "< 0.001" else sprintf("= %.3f", p)
  d$g <- factor(d$g, levels = keep, labels = sprintf("%s\n(n=%d)", keep, n_by))
  gg <- ggplot(d, aes(g, y, fill = g))
  if (!is.na(cutoff))
    gg <- gg + geom_hline(yintercept = cutoff, color = "grey55", linetype = "dotted")
  gg +
    geom_boxplot(outlier.shape = NA, width = .55, linewidth = .35) +
    geom_point(position = position_jitter(width = .25, seed = 1),
               size = .4, alpha = .18, color = "black") +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 2.2,
                 fill = "white", color = "black", stroke = .8) +
    scale_fill_brewer(palette = "Set2") +
    coord_cartesian(ylim = ylim) +
    labs(title = sprintf("%s\n%s  p %s", ttl, tst, p_str), x = NULL, y = NULL) +
    theme(legend.position = "none",
          plot.title = element_text(size = 9, face = "bold"),
          axis.text.x = element_text(size = 7))
}
save_panels <- function(outcome, ylim, cutoff, ylab, suptitle, fname) {
  panels <- lapply(PANEL_GROUPS, function(gr)
    mk_panel(gr[[1]], gr[[2]], gr[[3]], outcome, ylim, cutoff))
  gsav(fname, 13.5, 11, gridExtra::arrangeGrob(
    grobs = panels, ncol = 3,
    top  = grid::textGrob(suptitle, gp = grid::gpar(fontface = "bold", fontsize = 13)),
    left = grid::textGrob(ylab, rot = 90, gp = grid::gpar(fontsize = 11))))
}
save_panels("SDS_index", c(20, 100), 50, "SDS Index",
            "Zung SDS Index across demographic, clinical, and academic groups",
            "fig8_sds_by_group_box")
save_panels("AwarenessScore", c(-0.5, 18), NA, "Awareness score (0-17)",
            "Depression-awareness score (0-17) across demographic, clinical, and academic groups",
            "fig11_awareness_by_group_box")

for (f in c(file.path(getwd(), "Rplots.pdf"), file.path(OUT, "Rplots.pdf"),
            file.path(TAB, "Rplots.pdf"))) if (file.exists(f)) unlink(f)

# 11. Summary --------------------------------------------------------------------
cat("\n================= Summary=================\n")
cat(sprintf("N (complete SDS) = %d | mean (SD) = %.2f (%.2f) | depressed >=50 = %.1f%% (%.1f-%.1f)\n",
            n_total, m, sd_, w_dep["pct"], w_dep["CI_low_pct"], w_dep["CI_high_pct"]))
cat(sprintf("alpha = %.3f | AUC = %.3f | linear R2 = %.3f | BP p = %s | awareness rho = %.3f (p = %s)\n",
            alpha_obj$total$raw_alpha, auc, lin_sm$r.squared, pv(bp_p),
            sp$estimate, pv(sp$p.value)))
cat(sprintf("Logistic assumptions: max VIF = %.2f | Box-Tidwell min p = %s | max Cook's D = %.4f | max leverage = %.4f (thr %.4f) | H-L p = %s\n",
            max(vif_log), pv(min(bt_p)), max(cook_log), max(lev_log), lev_thr, pv(hl_p)))
cat(sprintf("EFA: full KMO %.3f -> reduced KMO %.3f (dropped %s)\n",
            efa_full$kmo, efa_red$kmo, paste(dropped, collapse = ", ")))
cat(sprintf("Wrote %d tables and %d figures to %s\n",
            length(list.files(TAB, pattern = "\\.csv$")),
            length(list.files(FIG, pattern = "\\.png$")), OUT))
