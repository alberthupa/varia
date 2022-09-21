# calculate LTV matrices from data_in file
# colnames for data_in:
# customer_id, paragon, netto, margin, year_month
     
library(writexl)
library(tidyverse)
library(lubridate)

file_out_name <- 'my_file_out_name.xlsx'

year_months_to_do <- sort(unique(data_in$year_month))



df_acquisition_year_month <- data_in %>% 
  group_by(customer_id) %>% 
  summarise(acquisition_year_month = min(year_month))

mat_to_fill_uu <- matrix(, nrow = length(year_months_to_do), ncol = length(year_months_to_do))
mat_to_fill_trans <- matrix(, nrow = length(year_months_to_do), ncol = length(year_months_to_do))
mat_to_fill_netto <- matrix(, nrow = length(year_months_to_do), ncol = length(year_months_to_do))
mat_to_fill_margin <- matrix(, nrow = length(year_months_to_do), ncol = length(year_months_to_do))
mat_to_fill_churn <- matrix(, nrow = length(year_months_to_do), ncol = length(year_months_to_do))


for (i in 1:length(year_months_to_do)) {  
  
  ym_to_do_acq <- year_months_to_do[i]
  members_to_get <- unique(df_acquisition_year_month %>% 
                             filter(acquisition_year_month == ym_to_do_acq) %>% 
                             pull(customer_id))
  
  data_in_temp <- data_in %>% 
    filter(customer_id %in% members_to_get)
  
  for (j in 1:length(year_months_to_do)) {  
    ym_to_do_act_tmp <- year_months_to_do[j]
    if (ym_to_do_act_tmp >= ym_to_do_acq) {
      data_in_temp_i <- data_in_temp %>% filter(year_month == ym_to_do_act_tmp)
      kpi_uu <- length(unique(data_in_temp_i %>% pull(customer_id)))
      kpi_trans <- length(unique(data_in_temp_i %>% pull(paragon)))
      kpi_netto <- data_in_temp_i %>% summarise(netto = sum(netto)) %>% pull(netto)
      kpi_margin <- data_in_temp_i %>% summarise(margin = sum(margin)) %>% pull(margin)
      mat_to_fill_uu[i,j] <- kpi_uu
      mat_to_fill_trans[i,j] <- kpi_trans
      mat_to_fill_netto[i,j] <- kpi_netto
      mat_to_fill_margin[i,j] <- kpi_margin
    }
  }
  
}



# ADD CHURN ----
for (i in 1:dim(mat_to_fill_uu)[1]) {
  if_first <- 0
  first_value <- 0
  for (j in 1:dim(mat_to_fill_uu)[1]) {
    if (if_first < 1) {
      if (!is.na(mat_to_fill_uu[i,j])) {
        first_value <- mat_to_fill_uu[i,j]
        if_first <- if_first + 1
      }
    }
  }
  if_first_two <- 0
  for (j in 1:dim(mat_to_fill_churn)[1]) {
    if (if_first_two < 1) {
      if (!is.na(mat_to_fill_uu[i,j])) {
        mat_to_fill_churn[i,j] <- mat_to_fill_uu[i,j] / first_value
      }
    }
  }
}



# ALIGN MATRIX TO THE LEFT ------
align_left_mat <- function(mat_to_align) {
  aligned_mat <- matrix(, nrow = dim(mat_to_align)[1], ncol = dim(mat_to_align)[1])
  for (i in 1:dim(aligned_mat)[1]) {
    vec_length_counter <- 0
    for (j in 1:dim(aligned_mat)[1]) {
      if (!is.na(mat_to_align[i,j])) {
        vec_length_counter <- vec_length_counter + 1
        aligned_mat[i,vec_length_counter] <- mat_to_align[i,j]
      }
    }
  }
  return(aligned_mat)
}

mat_to_fill_uu_aligned <- align_left_mat(mat_to_fill_uu)
mat_to_fill_trans_aligned <- align_left_mat(mat_to_fill_trans)
mat_to_fill_netto_aligned <- align_left_mat(mat_to_fill_netto)
mat_to_fill_margin_aligned <- align_left_mat(mat_to_fill_margin)
mat_to_fill_churn_aligned <- align_left_mat(mat_to_fill_churn)

calculate_kpi_vector_mean <- function(mat_source, history_months, forecast_months) {
  middle_vector <- c()
  for (i in 1:forecast_months) {
    my_column <- mat_source[,i]
    my_column <- my_column[!is.na(my_column)]
    aaa <- tail(my_column, n = history_months)
    middle_vector <- c(middle_vector, mean(aaa, na.rm = TRUE))
  }
  return(middle_vector)
}

# GET FINAL KPI TABLE ----

kpi_vector_uu <- calculate_kpi_vector_mean(mat_to_fill_uu_aligned, 12, 12)
kpi_vector_trans <- calculate_kpi_vector_mean(mat_to_fill_trans_aligned, 12, 12)
kpi_vector_netto <- calculate_kpi_vector_mean(mat_to_fill_netto_aligned, 12, 12)
kpi_vector_margin <- calculate_kpi_vector_mean(mat_to_fill_margin_aligned, 12, 12)
kpi_vector_churn <- calculate_kpi_vector_mean(mat_to_fill_churn_aligned,12, 12)
kpi_vector_ave_trans_per_user <- kpi_vector_trans / kpi_vector_uu
kpi_vector_ave_netto_per_trans <- kpi_vector_netto / kpi_vector_trans
kpi_vector_ave_margin_per_trans <- kpi_vector_margin / kpi_vector_trans
kpi_netto_ltv_netto <- kpi_vector_churn * kpi_vector_ave_trans_per_user * kpi_vector_ave_netto_per_trans
kpi_netto_ltv_margin <- kpi_vector_churn * kpi_vector_ave_trans_per_user * kpi_vector_ave_margin_per_trans
kpi_netto_ltv_netto_cumsum <- cumsum(kpi_netto_ltv_netto)
kpi_netto_ltv_margin_cumsum <- cumsum(kpi_netto_ltv_margin)

final_df <- data.frame(kpi_vector_uu = kpi_vector_uu,
                       kpi_vector_trans = kpi_vector_trans,
                       kpi_vector_netto = kpi_vector_netto,
                       kpi_vector_margin = kpi_vector_margin,
                       kpi_vector_retention = kpi_vector_churn,
                       kpi_vector_ave_trans_per_user = kpi_vector_ave_trans_per_user,
                       kpi_vector_ave_netto_per_trans = kpi_vector_ave_netto_per_trans,
                       kpi_vector_ave_margin_per_trans = kpi_vector_ave_margin_per_trans,
                       kpi_netto_ltv_netto = kpi_netto_ltv_netto,
                       kpi_netto_ltv_netto_cumsum = kpi_netto_ltv_netto_cumsum,                       
                       kpi_netto_ltv_margin = kpi_netto_ltv_margin,
                       kpi_netto_ltv_margin_cumsum = kpi_netto_ltv_margin_cumsum)



# zapisuj -----
mat_to_fill_uu <- as.data.frame(mat_to_fill_uu)
mat_to_fill_trans <- as.data.frame(mat_to_fill_trans)
mat_to_fill_netto <- as.data.frame(mat_to_fill_netto)
mat_to_fill_margin <- as.data.frame(mat_to_fill_margin)
mat_to_fill_churn <- as.data.frame(mat_to_fill_churn)

colnames(mat_to_fill_uu) <- year_months_to_do
colnames(mat_to_fill_trans) <- year_months_to_do
colnames(mat_to_fill_netto) <- year_months_to_do
colnames(mat_to_fill_margin) <- year_months_to_do
colnames(mat_to_fill_churn) <- year_months_to_do

mat_to_fill_uu <- bind_cols(data.frame(year_months_to_do), mat_to_fill_uu)
mat_to_fill_trans <- bind_cols(data.frame(year_months_to_do), mat_to_fill_trans)
mat_to_fill_netto <- bind_cols(data.frame(year_months_to_do), mat_to_fill_netto)
mat_to_fill_margin <- bind_cols(data.frame(year_months_to_do), mat_to_fill_margin)
mat_to_fill_churn <- bind_cols(data.frame(year_months_to_do), mat_to_fill_churn)

sheets <- list(summary_df = final_df,
               uu = mat_to_fill_uu,
               trans = mat_to_fill_trans,
               netto = mat_to_fill_netto,
               margin = mat_to_fill_margin,
               retention = mat_to_fill_churn)


# WRITE OUTFILE ---
write_xlsx(sheets, file_out_name)
