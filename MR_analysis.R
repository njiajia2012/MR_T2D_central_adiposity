label = sprintf("%.2f (%.2f–%.2f)", OR, lower, upper),
label_x = xmax + 0.6
)
# 设置顺序
mvmr_plot$Exposure <- factor(
mvmr_plot$Exposure,
levels = c("Waist circumference", "BMI", "Hip circumference")
)
p <- ggplot(mvmr_plot, aes(y = Exposure, x = OR)) +
# OR=1 参考线
geom_vline(xintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
# 置信区间横线（手动画，避免被ggplot吞掉）
geom_segment(
aes(x = lower, xend = upper_plot, y = Exposure, yend = Exposure),
linewidth = 0.9,
color = "black"
) +
# 左端帽子
geom_segment(
aes(x = lower, xend = lower, y = as.numeric(Exposure) - 0.10, yend = as.numeric(Exposure) + 0.10),
linewidth = 0.9,
color = "black",
inherit.aes = FALSE,
data = mvmr_plot
) +
# 右端帽子（未截断的）
geom_segment(
data = subset(mvmr_plot, trunc == FALSE),
aes(x = upper_plot, xend = upper_plot, y = as.numeric(Exposure) - 0.10, yend = as.numeric(Exposure) + 0.10),
linewidth = 0.9,
color = "black",
inherit.aes = FALSE
) +
# 右端箭头（被截断的）
geom_segment(
data = subset(mvmr_plot, trunc == TRUE),
aes(x = upper_plot - 0.35, xend = upper_plot, y = Exposure, yend = Exposure),
linewidth = 0.9,
color = "black",
arrow = arrow(length = unit(0.18, "cm"), type = "closed")
) +
# 点
geom_point(size = 3.8, color = "black") +
# 右侧文字
geom_text(
aes(x = label_x, label = label),
hjust = 0,
size = 4.2
) +
scale_x_continuous(
limits = c(0, xmax + 4),
breaks = c(0, 1, 2, 5, 10)
) +
labs(
title = "Multivariable Mendelian randomization",
x = "Odds ratio (95% CI)",
y = NULL
) +
theme_classic(base_size = 14) +
theme(
plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
axis.title.x = element_text(face = "bold", size = 14),
axis.text.x = element_text(size = 12),
axis.text.y = element_text(size = 13, face = "bold"),
axis.line = element_line(linewidth = 0.8),
axis.ticks = element_line(linewidth = 0.8),
plot.margin = margin(15, 140, 10, 10)
)
print(p)
ggsave("Figure_2_MVMR_forest_fixed.pdf", plot = p, width = 9, height = 5.5)
ggsave("Figure_2_MVMR_forest_fixed.png", plot = p, width = 9, height = 5.5, dpi = 300)
library(ggplot2)
library(dplyr)
# 数据
mvmr_plot <- data.frame(
Exposure = c("Waist circumference", "BMI", "Hip circumference"),
beta = c(2.1827367, 0.5963764, -1.5925389),
se = c(0.6755742, 0.5721506, 0.3918272)
)
# 计算 OR 和 CI
mvmr_plot <- mvmr_plot %>%
mutate(
OR = exp(beta),
lower = exp(beta - 1.96 * se),
upper = exp(beta + 1.96 * se),
label = sprintf("%.2f (%.2f–%.2f)", OR, lower, upper)
)
# 图中最大显示范围
xmax <- 12
mvmr_plot <- mvmr_plot %>%
mutate(
upper_plot = pmin(upper, xmax),
trunc = upper > xmax,
label_x = xmax + 1.5
)
# 顺序
mvmr_plot$Exposure <- factor(
mvmr_plot$Exposure,
levels = c("Waist circumference", "BMI", "Hip circumference")
)
p <- ggplot(mvmr_plot, aes(y = Exposure, x = OR)) +
geom_vline(xintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
geom_segment(
aes(x = lower, xend = upper_plot, y = Exposure, yend = Exposure),
linewidth = 1
) +
geom_point(size = 3.5) +
geom_text(
aes(x = label_x, label = label),
hjust = 0,
size = 4.5
) +
geom_segment(
data = subset(mvmr_plot, trunc == TRUE),
aes(x = upper_plot - 0.4, xend = upper_plot, y = Exposure, yend = Exposure),
arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
linewidth = 1
) +
scale_x_continuous(
limits = c(0, xmax + 4),
breaks = c(0,1,2,5,10)
) +
labs(
title = "Multivariable Mendelian randomization",
x = "Odds ratio (95% CI)",
y = NULL
) +
theme_classic(base_size = 14) +
theme(
plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
axis.text.y = element_text(size = 14, face = "bold"),
axis.title.x = element_text(size = 14),
plot.margin = margin(10, 250, 10, 10)
)
print(p)
ggsave("Figure_2_MVMR_forest_FINAL.pdf", p, width = 12, height = 5)
ggsave("Figure_2_MVMR_forest_FINAL.png", p, width = 12, height = 5, dpi = 300)
library(TwoSampleMR)
# 1️⃣ 提取T2D作为暴露
exp_dat <- extract_instruments(outcomes = "ieu-a-24")
library(TwoSampleMR)
# T2D作为暴露
exp_dat <- extract_instruments(outcomes = "ieu-a-24")
Sys.setenv(OPENGWAS_JWT="eyJhbGciOiJSUzI1NiIsImtpZCI6ImFwaS1qd3QiLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJhcGkub3Blbmd3YXMuaW8iLCJhdWQiOiJhcGkub3Blbmd3YXMuaW8iLCJzdWIiOiJuamlhamlhMjAxMkAxNjMuY29tIiwiaWF0IjoxNzczNzE4MTYyLCJleHAiOjE3NzQ5Mjc3NjJ9.CP90Hano13OHRfQZBHxxWgs9HPF7Ur6wrHKa2Nvu3szL5kdBRa6Ez3wfkewREt9LppmZkdBaWvfrxFf5LIG6ebH77EHIDtJWbT_OxWpOEQp3es_0slfLgN8GCsTrVRKZDgyM17ZJYxYe1Go-dCblCHmdWNg5QWVF2YBBe0ORbvsZA2J8O-Pi6oz9KEGbMJAAcC5o6TnM2W_uOWoOx4wl2XyjfmlyOFGXJSHmIeGJvcfL9sZWjGPzpEZ43mTqlN5Fjki8inwEArcuovtNotQrNzyyFYuf3xZawTs68QxvhWJFtiJLSUA0EJby42uRBX4Fq-Aj5meFDdsNw-0LPWFWqQ")
library(TwoSampleMR)
# T2D作为暴露
exp_dat <- extract_instruments(outcomes = "ieu-a-24")
# 结局（4个肥胖指标）
out_dat <- extract_outcome_data(
snps = exp_dat$SNP,
outcomes = c("ukb-b-2303",  # BMI
"ukb-b-9405",  # Waist circumference
"ukb-b-15590", # Hip circumference
"ukb-b-19393") # Body fat %
)
# 数据对齐
dat <- harmonise_data(exp_dat, out_dat)
# MR分析
res <- mr(dat)
# 敏感性分析
heterogeneity <- mr_heterogeneity(dat)
pleiotropy <- mr_pleiotropy_test(dat)
# 输出结果
res
# leave-one-out analysis
loo <- mr_leaveoneout(dat)
# plot
mr_leaveoneout_plot(loo)
# 安装一次即可
install.packages("patchwork")
library(ggplot2)
library(patchwork)
# 给每张图加标题
p_bmi <- p_bmi + ggtitle("A. Body mass index")
install.packages("magick")
library(magick)
img1 <- image_read("BMI_funnel_plot.pdf")
img2 <- image_read("Waist_circumference_funnel_plot.pdf")
img3 <- image_read("Hip_circumference_funnel_plot.pdf")
img4 <- image_read("Body_Fat_Percentage_funnel_plot.pdf")
# 横向拼接
top <- image_append(c(img1, img2))
bottom <- image_append(c(img3, img4))
# 纵向拼接
final <- image_append(c(top, bottom), stack = TRUE)
install.packages(c("TwoSampleMR", "patchwork", "ggplot2"))
library(TwoSampleMR)
library(ggplot2)
library(patchwork)
#--------------------------------
# 假设你已有：
# dat_bmi / dat_wc / dat_hc / dat_bfp
#--------------------------------
res_bmi <- mr(dat_bmi)
Sys.setenv(OPENGWAS_JWT = "eyJhbGciOiJSUzI1NiIsImtpZCI6ImFwaS1qd3QiLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJhcGkub3Blbmd3YXMuaW8iLCJhdWQiOiJhcGkub3Blbmd3YXMuaW8iLCJzdWIiOiJuamlhamlhMjAxMkAxNjMuY29tIiwiaWF0IjoxNzczODQ3NTcyLCJleHAiOjE3NzUwNTcxNzJ9.W5AN3yqoFVEbPv2ig8Xy5zG_NpzVtdGU0DIqNDQR_YsBPxSr0A6LAAUVKGDNvgtI-aPyJ-unT359v4Evzo_Q4A6TLyTvPi0vhYUFntuxBdyzsLtozcIhMcF-WHbCjLkwXxso-FTZ48qzY8RROsEgq_U82Ghyx8wKzzSU-baJA2WGIXN9aZVITooAq9dBflEL2-vE9NLbIMax_f3AORMzMUNacOXGNv2xw9ByGlUlTIBfbiIzsK9HjTzf5kn-oPj2kGt6rwPp5Y_jZUTQ2NYS7O84HYteF-XeGIAXKDj6qpSocrBBQawtwdp2TjxUaEXrojOijzOZB2KpqoMz-u-aaA")
library(TwoSampleMR)
extract_instruments(outcomes = "ukb-b-2303")
library(TwoSampleMR)
library(patchwork)
#--------------------------------
# outcome（T2D）
#--------------------------------
outcome_id <- "ieu-a-24"
#--------------------------------
# 4个暴露（你之前用的）
#--------------------------------
exp_bmi <- "ukb-b-2303"
exp_wc  <- "ukb-b-9405"
exp_hc  <- "ukb-b-15590"
exp_bfp <- "ukb-b-19393"
#--------------------------------
# 自动函数（不用改）
#--------------------------------
get_plot <- function(exp_id, title){
exp_dat <- extract_instruments(outcomes = exp_id)
out_dat <- extract_outcome_data(snps = exp_dat$SNP, outcomes = outcome_id)
dat <- harmonise_data(exp_dat, out_dat)
res <- mr(dat)
p <- mr_scatter_plot(res, dat)[[1]] +
ggtitle(title)
return(p)
}
#--------------------------------
# 生成4个图
#--------------------------------
p_bmi <- get_plot(exp_bmi, "A. Body mass index")
p_wc  <- get_plot(exp_wc,  "B. Waist circumference")
p_hc  <- get_plot(exp_hc,  "C. Hip circumference")
p_bfp <- get_plot(exp_bfp, "D. Body fat percentage")
#--------------------------------
# 拼图（关键）
#--------------------------------
fig2 <- (p_bmi | p_wc) / (p_hc | p_bfp) +
plot_layout(guides = "collect")
#--------------------------------
# 保存（不会再裁切）
#--------------------------------
ggsave("Figure2_scatter_multi_panel.png", fig2, width = 12, height = 10, dpi = 300)
ggsave("Figure2_scatter_multi_panel.pdf", fig2, width = 12, height = 10)
install.packages(c("TwoSampleMR", "patchwork", "ggplot2"))
library(TwoSampleMR)
library(patchwork)
library(ggplot2)
# outcome
outcome_id <- "ieu-a-24"
# exposures
exp_bmi <- "ukb-b-2303"
exp_wc  <- "ukb-b-9405"
exp_hc  <- "ukb-b-15590"
exp_bfp <- "ukb-b-19393"
get_plot <- function(exp_id, title, x_label) {
exp_dat <- extract_instruments(outcomes = exp_id)
out_dat <- extract_outcome_data(snps = exp_dat$SNP, outcomes = outcome_id)
dat <- harmonise_data(exp_dat, out_dat)
res <- mr(
dat,
method_list = c(
"mr_ivw",
"mr_egger_regression",
"mr_weighted_median"
)
)
p <- mr_scatter_plot(res, dat)[[1]] +
ggtitle(title) +
xlab(x_label) +
ylab("SNP effect on type 2 diabetes") +
theme_bw(base_size = 12) +
theme(
plot.title = element_text(face = "bold", size = 13),
axis.title = element_text(face = "bold"),
legend.position = "bottom",
legend.title = element_blank(),
legend.box = "horizontal",
legend.margin = margin(t = 5),
plot.margin = margin(10, 10, 20, 10),
panel.grid.minor = element_blank()
)
return(p)
}
p_bmi <- get_plot(exp_bmi, "A. Body mass index", "SNP effect on BMI")
p_wc  <- get_plot(exp_wc,  "B. Waist circumference", "SNP effect on waist circumference")
p_hc  <- get_plot(exp_hc,  "C. Hip circumference", "SNP effect on hip circumference")
p_bfp <- get_plot(exp_bfp, "D. Body fat percentage", "SNP effect on body fat percentage")
fig2 <- (p_bmi | p_wc) / (p_hc | p_bfp) +
plot_layout(guides = "collect") &
theme(legend.position = "bottom")
ggsave(
"Figure2_scatter_multi_panel.png",
fig2,
width = 12,
height = 10,
dpi = 300
)
ggsave(
"Figure2_scatter_multi_panel.pdf",
fig2,
width = 12,
height = 10
)
install.packages(c("TwoSampleMR", "patchwork", "ggplot2"))
library(TwoSampleMR)
library(patchwork)
library(ggplot2)
#----------------------------
# outcome
#----------------------------
outcome_id <- "ieu-a-24"
#----------------------------
# exposures
#----------------------------
exp_bmi <- "ukb-b-2303"
exp_wc  <- "ukb-b-9405"
exp_hc  <- "ukb-b-15590"
exp_bfp <- "ukb-b-19393"
#----------------------------
# function: prepare harmonised data
#----------------------------
get_dat <- function(exp_id) {
exp_dat <- extract_instruments(outcomes = exp_id)
out_dat <- extract_outcome_data(snps = exp_dat$SNP, outcomes = outcome_id)
dat <- harmonise_data(exp_dat, out_dat)
return(dat)
}
#----------------------------
# get data
#----------------------------
dat_bmi <- get_dat(exp_bmi)
dat_wc  <- get_dat(exp_wc)
dat_hc  <- get_dat(exp_hc)
dat_bfp <- get_dat(exp_bfp)
#============================
# S2: Leave-one-out plots
#============================
make_loo_plot <- function(dat, title) {
loo_res <- mr_leaveoneout(dat)
p <- mr_leaveoneout_plot(loo_res)[[1]] +
ggtitle(title) +
theme_bw(base_size = 12) +
theme(
plot.title = element_text(face = "bold", size = 13),
axis.title = element_text(face = "bold"),
plot.margin = margin(10, 10, 15, 10),
panel.grid.minor = element_blank()
)
return(p)
}
p_loo_bmi <- make_loo_plot(dat_bmi, "A. Body mass index")
p_loo_wc  <- make_loo_plot(dat_wc,  "B. Waist circumference")
p_loo_hc  <- make_loo_plot(dat_hc,  "C. Hip circumference")
p_loo_bfp <- make_loo_plot(dat_bfp, "D. Body fat percentage")
fig_s2 <- (p_loo_bmi | p_loo_wc) / (p_loo_hc | p_loo_bfp)
ggsave(
"Supplementary_Figure_S2_leave_one_out.png",
fig_s2,
width = 12,
height = 10,
dpi = 300
)
ggsave(
"Supplementary_Figure_S2_leave_one_out.pdf",
fig_s2,
width = 12,
height = 10
)
#============================
# S3: Funnel plots
#============================
make_funnel_plot <- function(dat, title) {
singlesnp_res <- mr_singlesnp(dat)
p <- mr_funnel_plot(singlesnp_res)[[1]] +
ggtitle(title) +
theme_bw(base_size = 12) +
theme(
plot.title = element_text(face = "bold", size = 13),
axis.title = element_text(face = "bold"),
legend.position = "bottom",
legend.title = element_blank(),
plot.margin = margin(10, 10, 20, 10),
panel.grid.minor = element_blank()
)
return(p)
}
p_fun_bmi <- make_funnel_plot(dat_bmi, "A. Body mass index")
p_fun_wc  <- make_funnel_plot(dat_wc,  "B. Waist circumference")
p_fun_hc  <- make_funnel_plot(dat_hc,  "C. Hip circumference")
p_fun_bfp <- make_funnel_plot(dat_bfp, "D. Body fat percentage")
fig_s3 <- (p_fun_bmi | p_fun_wc) / (p_fun_hc | p_fun_bfp) +
plot_layout(guides = "collect") &
theme(legend.position = "bottom")
ggsave(
"Supplementary_Figure_S3_funnel.png",
fig_s3,
width = 12,
height = 10,
dpi = 300
)
ggsave(
"Supplementary_Figure_S3_funnel.pdf",
fig_s3,
width = 12,
height = 10
)
install.packages(c("TwoSampleMR", "dplyr", "stringr", "openxlsx"))
library(TwoSampleMR)
library(dplyr)
library(stringr)
library(openxlsx)
# 假设你已经有一个数据框 df
# 列名至少包括：Beta, SE
df$OR <- exp(df$Beta)
install.packages("openxlsx")
library(openxlsx)
write.csv(
supp_table_s1,
"Supplementary_Table_S1.csv",
row.names = FALSE
)
options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages("openxlsx", type = "source")
library(TwoSampleMR)
library(dplyr)
outcome_id <- "ieu-a-24"
exp_bmi <- "ukb-b-2303"
exp_wc  <- "ukb-b-9405"
exp_hc  <- "ukb-b-15590"
exp_bfp <- "ukb-b-19393"
get_full_mr_table <- function(exp_id, exposure_name) {
exp_dat <- extract_instruments(outcomes = exp_id)
out_dat <- extract_outcome_data(snps = exp_dat$SNP, outcomes = outcome_id)
dat <- harmonise_data(exp_dat, out_dat)
res <- mr(
dat,
method_list = c(
"mr_egger_regression",
"mr_weighted_median",
"mr_ivw",
"mr_simple_mode",
"mr_weighted_mode"
)
)
res %>%
mutate(
Exposure = exposure_name,
OR = exp(b),
CI_lower = exp(b - 1.96 * se),
CI_upper = exp(b + 1.96 * se),
`OR (95% CI)` = paste0(
sprintf("%.2f", OR), " (",
sprintf("%.2f", CI_lower), "–",
sprintf("%.2f", CI_upper), ")"
),
Method = case_when(
method == "MR Egger" ~ "MR-Egger",
method == "Inverse variance weighted" ~ "Inverse-variance weighted",
TRUE ~ method
),
`P value` = ifelse(
pval < 0.001,
formatC(pval, format = "e", digits = 2),
sprintf("%.3f", pval)
)
) %>%
transmute(
Exposure,
Method,
SNPs = nsnp,
Beta = round(b, 4),
SE = round(se, 4),
`P value`,
`OR (95% CI)`
)
}
tab_bmi <- get_full_mr_table(exp_bmi, "BMI")
Sys.setenv(OPENGWAS_JWT = "eyJhbGciOiJSUzI1NiIsImtpZCI6ImFwaS1qd3QiLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJhcGkub3Blbmd3YXMuaW8iLCJhdWQiOiJhcGkub3Blbmd3YXMuaW8iLCJzdWIiOiJuamlhamlhMjAxMkAxNjMuY29tIiwiaWF0IjoxNzc0MDE1MjE0LCJleHAiOjE3NzUyMjQ4MTR9.EAjOqBIHfapes6QLjlLpXG-6PWMPyTzjezdh7YBGFWX_NELR98ogH8C-BhISJ40Gb-kj5R1ZHBXkm_tCtao4ikboz_4I0Wmyn4HpgoaLm1vuN-lGDnA5hoaWCUL4eRe3ukOWDhlxGC2Odz5HqU78n1ikGvSUah0hr61fseiBNDUBknLmTvU2LBViCQaRqKY87z1238fhfYRZnjLj7i5YHgvo0ghFeuFXxnNrqLADfckPrh9h-FAbvkN9UNFwJfUE3V09YeWA2_QVUycWu_IZ_659DEoUDyhSTsEqOlFraBwbg16Yp8kRBwjC_E0juNLHfD2tHWiHD_4bwaFcT4-Lqw")
ieugwasr::get_opengwas_jwt()
library(TwoSampleMR)
library(ieugwasr)
bmi_exp <- extract_instruments(
outcomes = "ukb-b-2303",   # 这里换成你的 exposure ID
p1 = 5e-8,
clump = TRUE,
r2 = 0.001,
kb = 10000
)
ieugwasr::get_opengwas_jwt()
ieugwasr::check_reset()
bmi_exp <- extract_instruments(
outcomes = "ukb-b-2303",
p1 = 5e-8,
clump = FALSE
)
ieugwasr::get_opengwas_jwt()
ieugwasr::gwasinfo("ukb-b-2303")
bmi_exp <- extract_instruments(
outcomes = "ukb-b-2303",
p1 = 5e-8,
clump = FALSE,
force_server = FALSE
)
View(dat_bmi)
View(exp_dat)
View(exp_dat)
F_stat <- (beta^2) / (se^2)
names(exp_dat)
head(exp_dat)
exp_dat$F_stat <- (exp_dat$beta^2) / (exp_dat$se^2)
summary(exp_dat$F_stat)
min(exp_dat$F_stat, na.rm = TRUE)
median(exp_dat$F_stat, na.rm = TRUE)
mean(exp_dat$F_stat, na.rm = TRUE)
all(exp_dat$F_stat > 10, na.rm = TRUE)
write.csv(exp_dat, "exp_dat_with_F.csv", row.names = FALSE)
