# package to use
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)

# read value added, capital compensation, capital stock, producer price index,
VA <- read_excel("CIP_4.0_(2023)_1.2.xlsx")
Cap_VA <- read_excel("CIP_4.0_(2023)_1.4.xlsx",sheet="Capital compensation")
PPI <- read_excel("CIP_4.0_(2023)_1.6.xlsx")
Cap_S <- read_excel("CIP_4.0_(2023)_2.2.xlsx")
LAB <- read_excel("CIP_4.0_(2023)_3.2.xlsx")


# extract the needed data
VA1 <- VA[-1:-2,-1:-3] 
CAP_VA1 <- Cap_VA[-1:-2,-1:-2]
CAP_S1 <- Cap_S[-1:-2,-1:-2]
PPI1 <- PPI[-1:-2,-1:-2]
LAB1 <- LAB[-1:-2,-1:-2]
PPI_fac <- PPI1/100

# calculate the cumulative product,
fixed_base_index_factors <- t(apply(PPI_fac, 1, cumprod))
fixed_base_ppi <- as.data.frame(fixed_base_index_factors * 100)
colnames(fixed_base_ppi) <- colnames(PPI1)

# calculate the real value added, growth of labor productivity
VA_real <- (VA1/fixed_base_ppi)*100
LP1 <- VA_real/LAB1
gamma <- (LP1[, -1] - LP1[, -ncol(LP1)]) / LP1[, -ncol(LP1)]
gamma <- cbind(gamma, NA)

# calculate the sum
total_real <- VA_real %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))
VA_real <- bind_rows(VA_real,total_real)
total_CAP_S <- CAP_S1 %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))
CAP_S1 <- bind_rows(CAP_S1,total_CAP_S)
total_CAP_VA <- CAP_VA1 %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))
CAP_VA1 <- bind_rows(CAP_VA1,total_CAP_VA)
total_LAB <- LAB1 %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))
LAB1 <- bind_rows(LAB1,total_LAB)
total_VA <- VA1 %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))
VA1 <- bind_rows(VA1,total_VA)
total_LAB <- total_real/total_LAB
total_gamma <- (total_LAB[-1]-total_LAB[-length(total_LAB)])/total_LAB[-length(total_LAB)]
total_gamma <- cbind(total_gamma,NA)

# share of capital compensation
pi <- CAP_VA1/VA1
#growth of factor productivity
rho <- VA_real/CAP_S1
gamma <- bind_rows(gamma,total_gamma)
rho1 <- rho[,2:ncol(rho)]
rho2 <- rho[,1:(ncol(rho)-1)]
chi <- cbind((rho1-rho2)/rho2,NA)

# viability parameter
omega <- gamma*(1+chi)/(gamma-chi)
gamma <- as.data.frame(lapply(gamma, as.numeric))
rho <- as.data.frame(lapply(rho, as.numeric))
omega <- as.data.frame(lapply(omega, as.numeric))
pi <- as.data.frame(lapply(pi, as.numeric))

col <- Cap_S[2, -1:-2]
row_industries <- Cap_VA[-1:-2,2]
row <- Cap_VA[-1:-2,2]
col1 <- as.character(unlist(Cap_S[2, -1:-2]))
row1 <- c(as.character(unlist(row_industries)), "Total")

# calcualate the labor weight
LAB_sectors <- LAB1[-nrow(LAB1), ]          
LAB_total   <- as.numeric(LAB1[nrow(LAB1), ])

s_share <- sweep(LAB_sectors, MARGIN = 2, STATS = LAB_total, FUN = "/")
colnames(s_share) <- col1
rownames(s_share) <- row1[-length(row1)]

# calculate the capital weight
CAP_S_sectors <- CAP_S1[-nrow(CAP_S1), ]   
CAP_S_total   <- as.numeric(CAP_S1[nrow(CAP_S1), ])

t_share <- sweep(CAP_S_sectors, MARGIN = 2, STATS = CAP_S_total, FUN = "/")
colnames(t_share) <- col1
rownames(t_share) <- row1[-length(row1)]

# remove the total row of productivty growth lists
rho_sec <- as.data.frame(lapply(rho[-nrow(rho), ], as.numeric))
LP_sec  <- as.data.frame(lapply(LP1, as.numeric))  

t_mat <- as.data.frame(lapply(t_share, as.numeric))
s_mat <- as.data.frame(lapply(s_share, as.numeric))

# ── 定义计算函数（逐年，t 期为当年，t+1 期为下一年）─────────────
n_years <- ncol(rho_sec) - 1  # 最后一年没有"下一期"

rho_between <- numeric(n_years)
rho_within  <- numeric(n_years)
x_between   <- numeric(n_years)
x_within    <- numeric(n_years)
year_labels <- col1[1:n_years]

for (k in 1:n_years) {
  rho_t  <- rho_sec[, k]
  rho_t1 <- rho_sec[, k + 1]
  t_t    <- t_mat[, k]
  t_t1   <- t_mat[, k + 1]
  
  x_t    <- LP_sec[, k]
  x_t1   <- LP_sec[, k + 1]
  s_t    <- s_mat[, k]
  s_t1   <- s_mat[, k + 1]
  
  denom_rho <- sum(rho_t * t_t, na.rm = TRUE)
  denom_x   <- sum(x_t * s_t,   na.rm = TRUE)
  
  # 资本生产率 Between：份额变化，用 t+1 期的 rho 加权
  rho_between[k] <- sum(rho_t1 * t_t1 - rho_t1 * t_t, na.rm = TRUE) / denom_rho
  # 资本生产率 Within：rho 变化，份额固定在 t 期
  rho_within[k]  <- sum(rho_t1 * t_t  - rho_t  * t_t, na.rm = TRUE) / denom_rho
  
  # 劳动生产率 Between：份额变化，用 t+1 期的 x 加权
  x_between[k]   <- sum(x_t1 * s_t1 - x_t1 * s_t, na.rm = TRUE) / denom_x
  # 劳动生产率 Within：x 变化，份额固定在 t 期
  x_within[k]    <- sum(x_t1 * s_t  - x_t  * s_t, na.rm = TRUE) / denom_x
}

# ── 汇总结果 ──────────────────────────────────────────────────
shift_share_result <- data.frame(
  Year            = year_labels,
  rho_between     = rho_between,
  rho_within      = rho_within,
  x_between       = x_between,
  x_within        = x_within
)



# viability condition and pattern of technical change
viability <- ifelse(is.na(gamma) | is.na(chi), 
                    NA, 
                    ifelse((gamma > chi & omega > pi) | (gamma <= chi & omega <= pi), 
                           "yes", 
                           "no"))
viability_check<- as.data.frame(viability)

TCpattern <- ifelse(gamma > 0 & chi < 0, "MBTC",
                    ifelse(gamma < 0 & chi > 0, "RMBTC",
                           ifelse(gamma > 0 & chi > 0, "both-strengthen", "neither")))
pattern <- as.data.frame(TCpattern)

# combination
combined <- paste(as.matrix(pattern), as.matrix(viability_check), sep = " and ")
combined_matrix_reshaped <- matrix(combined, 
                                   nrow = nrow(pattern), 
                                   ncol = ncol(pattern))
combined_df <- as.data.frame(combined_matrix_reshaped)

colnames(viability_check) <- col1
rownames(viability_check) <- row1
colnames(gamma) <- col1
rownames(gamma) <- row1
colnames(chi) <- col1
rownames(chi) <- row1
colnames(pi) <- col1
rownames(pi) <- row1
colnames(omega) <- col1
rownames(omega) <- row1
colnames(pattern) <- col1
rownames(pattern) <- row1
colnames(combined_df) <- col1
rownames(combined_df) <- row1


# count
row_yes_counts <- apply(viability_check, 1, function(row) sum(row == "yes", na.rm = TRUE))
row_no_counts <- apply(viability_check, 1, function(row) sum(row == "no", na.rm = TRUE))
row_na_counts <- apply(viability_check, 1, function(row) sum(is.na(row)))

row_summary <- cbind(Yes_Count = row_yes_counts, 
                     No_Count = row_no_counts,
                     NA_Count = row_na_counts)

total_summary <- table(unlist(viability_check), useNA = "ifany")

row_summary <- as.data.frame(row_summary)
total_summary <- as.data.frame(total_summary)
colnames(total_summary) <- c("结果", "计数")

viability_vector <- unlist(viability_check)
tc_pattern_vector <- unlist(pattern)

contingency_table <- table(Pattern = tc_pattern_vector, Viability = viability_vector, useNA = "ifany")

contingency_df <- as.data.frame.matrix(contingency_table)

CHN_result <- list(
  "劳动生产率增长率" = gamma,
  "资本生产率增长率" = chi,
  "利润份额" = pi,
  "可行性参数" = omega,
  "可行性核查" = viability_check,
  "技术进步模式" = pattern,
  "总体结果" =combined_df,
  "可行性条件分行统计" = row_summary,
  "可行性条件总计" = total_summary,
  "按技术进步模式统计" = contingency_df
)

# 加上行业列（保留行名）
combined_df$Industry <- rownames(combined_df)

# 转为长格式
combined_long <- combined_df %>%
  pivot_longer(
    cols = -Industry,
    names_to = "Year",
    values_to = "Result"
  )

# 分行业计数
industry_count <- combined_long %>%
  group_by(Industry, Result) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Industry, desc(Count))

# 每个行业的总数透视成宽表
industry_count_wide <- industry_count %>%
  pivot_wider(
    names_from = Result,
    values_from = Count,
    values_fill = 0
  )

# 添加到结果列表
CHN_result[["分行业结果统计"]] <- industry_count_wide
# ── 加入结果列表 ──────────────────────────────────────────────
CHN_result[["劳动权重"]] <- s_share
CHN_result[["资本权重"]] <- t_share

# ── 将 between/within 插入对应矩阵 ───────────────────────────────

# 构造补充行（最后一年为 NA，因为无法计算下一期）
rho_between_row <- c(rho_between, NA)
rho_within_row  <- c(rho_within,  NA)
x_between_row   <- c(x_between,   NA)
x_within_row    <- c(x_within,    NA)

# 转为单行 data.frame，列名对齐
rho_between_df <- setNames(as.data.frame(t(rho_between_row)), col1)
rho_within_df  <- setNames(as.data.frame(t(rho_within_row)),  col1)
x_between_df   <- setNames(as.data.frame(t(x_between_row)),   col1)
x_within_df    <- setNames(as.data.frame(t(x_within_row)),    col1)

# 插入资本生产率增长率（chi）矩阵末尾
chi_extended <- bind_rows(chi, rho_between_df, rho_within_df)
rownames(chi_extended) <- c(row1, "rho_between", "rho_within")
CHN_result[["资本生产率增长率"]] <- chi_extended

# 插入劳动生产率增长率（gamma）矩阵末尾
gamma_extended <- bind_rows(gamma, x_between_df, x_within_df)
rownames(gamma_extended) <- c(row1, "x_between", "x_within")
CHN_result[["劳动生产率增长率"]] <- gamma_extended


# 加载绘图包
library(showtext)
showtext_auto()
library(ggplot2)
library(tidyr)
library(dplyr)

# 提取 Total 行数据
plot_data_gamma <- CHN_result[["劳动生产率增长率"]]["Total", ]
plot_data_chi <- CHN_result[["资本生产率增长率"]]["Total", ]
plot_data_pi <- CHN_result[["利润份额"]]["Total", ]

df_plot_dual <- data.frame(
  Year = as.numeric(colnames(plot_data_gamma)),
  Gamma = as.numeric(plot_data_gamma),
  Chi = as.numeric(plot_data_chi),
  # 计算工资份额：Wage_Share = 1 - Profit_Share
  Wage_Share = 1 - as.numeric(plot_data_pi)
) %>%
  filter(!is.na(Year))

