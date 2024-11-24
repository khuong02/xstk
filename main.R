# Bước 1: Đọc dữ liệu
GPU_data <- read.csv("./archive/All_GPUs.csv")

# Thay thế tất cả các ô trống trong dữ liệu GPU_data bằng NA
GPU_data[GPU_data == ""] <- NA

# Thay thế các giá trị có định dạng "\n-" bằng NA trong tất cả các cột của GPU_data
GPU_data[] <- lapply(GPU_data, function(x) gsub("^\\n- $", NA, x))

# Thay thế các chuỗi "NA" bằng NA
GPU_data[GPU_data == "NA"] <- NA

# Tạo bảng thống kê số lượng và tỷ lệ dữ liệu khuyết
na_summary <- data.frame(
  Column = names(GPU_data),
  NA_Count = sapply(GPU_data, function(x) sum(is.na(x))),
  NA_Percentage = sapply(GPU_data, function(x) mean(is.na(x)) * 100)
)

# Hiển thị bảng thống kê
print(na_summary)

# Gọi thư viện ggplot2 để sử dụng hàm ggplot
library(ggplot2)

# Tạo biểu đồ cột cho tỷ lệ dữ liệu khuyết ở các biến
ggplot(na_summary, aes(x = Column, y = NA_Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(NA_Percentage, 1), "%")), 
            vjust = -0.5, size = 2) +
  labs(title = "Tỷ lệ dữ liệu khuyết ở các biến",
       x = "Biến",
       y = "Tỷ lệ dữ liệu khuyết (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1))  

# Lọc các cột có tỷ lệ dữ liệu khuyết dưới 10%
selected_columns <- na_summary$Column[na_summary$NA_Percentage < 10]

# Giữ lại các cột thỏa mãn điều kiện trong dataframe ban đầu
new_GPU_data <- GPU_data[, selected_columns]

#Xoá các quan sát chứa dữ liệu khuyết
new_GPU_data<-na.omit(new_GPU_data)

# Lựa chọn các biến cần xoá đơn vị
columns_to_clean <- c("L2_Cache","Memory_Bandwidth","Memory_Bus","Memory_Speed")

#Tạo hàm thực hiện xoá đơn vị ở các biến 
remove_units <- function(column) {
  # Sử dụng gsub để xóa tất cả các ký tự không phải số (kể cả đơn vị)
  cleaned_column <- gsub("[^0-9.]", "", column)
  # Chuyển đổi kết quả về kiểu numeric
  cleaned_column <- as.numeric(cleaned_column)
  
  return(cleaned_column)
}

# Áp dụng hàm cho các biến đã chọn
new_GPU_data[columns_to_clean] <- lapply(new_GPU_data[columns_to_clean], remove_units)

head(new_GPU_data)

dim(new_GPU_data)

# Tạo bộ dữ liệu bao gồm các biến chính để phân tích.
main_data<-new_GPU_data[c("Memory_Bandwidth","Memory_Speed","L2_Cache","Memory_Bus","Shader","Dedicated","Manufacturer")]
head(main_data)
