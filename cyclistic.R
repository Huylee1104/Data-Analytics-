### *Nhưng biểu đồ thuộc nghiên cứu về sự khác nhau giữa người thông thường và người dùng hàng năm của Cyclistic					
#Install and uisng "tidyverse"
install.packages("tidyverse")
library(tidyverse)

# Install and using "readr"
install.packages("readr")
library(readr)

# Install and using "dplyr"
install.packages("dplyr")
library(dplyr)

# Install and using "lubridate"
install.packages("lubridate")
library(lubridate)

# Install and using "ggplot2"
install.packages("ggplot2")
library(ggplot2)

# Đọc file 2019 và 2020
df_2019 <- read_csv("Divvy_Trips_2019_Q1 - Divvy_Trips_2019_Q1.csv")
df_2020 <- read_csv("Divvy_Trips_2020_Q1 - Divvy_Trips_2020_Q1.csv")

# Đổi tên cột và chuẩn hóa giá trị member_casual, thêm cột mới
data_2019_clean <- df_2019 %>%
  rename(
    ride_id = trip_id,
    rideable_type = bikeid,
    started_at = start_time,
    ended_at = end_time,
    start_station_name = from_station_name,
    start_station_id = from_station_id,
    end_station_name = to_station_name,
    end_station_id = to_station_id,
    member_casual = usertype
  ) %>%
  mutate(
    #chuẩn hóa dữ liệu
    member_casual = ifelse(member_casual == "Subscriber", "member", "casual"),
    # chỗ này là 1 câu lệnh if else
    start_lat = NA,
    start_lng = NA,
    end_lat = NA,
    end_lng = NA
  )

# Chọn cột cần thiết và sắp xếp thứ tự cột
data_2019_clean <- data_2019_clean %>%
  select(
    ride_id, rideable_type, started_at, ended_at,
    start_station_name, start_station_id,
    end_station_name, end_station_id,
    start_lat, start_lng, end_lat, end_lng,
    member_casual
  )
# 2 bảng có nhiều cột khác kiểu dữ liệu => đổi thành cùng kiểu dữ liệu
data_2019_clean <- data_2019_clean %>%
  mutate(ride_id = as.character(ride_id),
         rideable_type = as.character(rideable_type))
df_2020 <- df_2020 %>%
  mutate(ride_id = as.character(ride_id),
         rideable_type = as.character(rideable_type))

# hàm bind_rows: gộp theo chiều dọc (nối các hàng với nhau)
all_trips <- bind_rows(data_2019_clean, df_2020)

# Chuyển định dạng của thời gian started_at và ended_at sang datatype datetime
all_trips$started_at <- as.POSIXct(all_trips$started_at, format = "%Y-%m-%d %H:%M:%S")
all_trips$ended_at <- as.POSIXct(all_trips$ended_at, format = "%Y-%m-%d %H:%M:%S")

# Tạo cột ride_length để lưu thời gian di chuyển của 1 chuyến đi
all_trips <- all_trips %>%
  mutate(ride_length = ended_at - started_at)

# Tạo cột day_of_weed để lưu dữ liệu từ datetime sang thứ 2 ->  chủ nhật
all_trips$day_of_week <- as.numeric(format(all_trips$started_at, "%w")) + 1

# Kiểm tra dữ liệu
# Kiểm tra giá trị NA
colSums(is.na(all_trips))

# Kiểm tra trùng lặp
sum(duplicated(all_trips))

# Kiểm tra thời gian âm
sum(all_trips$ended_at < all_trips$started_at)

# Phát hiện dữ liệu âm -> Cần loại bỏ những giá trị này
all_trips <- all_trips %>% filter(ended_at >= started_at)

## Thực hiện một vài phân tích để tính toán
# Thời gian di chuyển trung bình.
mean(all_trips$ride_length, na.rm = TRUE)/60

# Thời gian di chuyển trung bình phân theo nhóm người
all_trips %>% group_by(member_casual) %>%
  summarise(avg_ride = mean(ride_length, na.rm = TRUE)/60)

# Kiểm tra số chuyến theo nhóm
all_trips %>% group_by(member_casual) %>%
  summarise(n = n())

# Kiểm tra và loại bỏ các chuyến di có thơi gian hơn 8 tiếng và bé hơn 90 giây
all_trips %>% filter(ride_length > 7200*4) %>% group_by(member_casual) %>%
    summarise(n = n(), max = max(ride_length))
all_trips %>% filter(ride_length < 90) %>% group_by(member_casual) %>%
    summarise(n = n(), min = min(ride_length))

all_trips <- all_trips %>%
  filter(ride_length <= 3600*8)
all_trips <- all_trips %>%
  filter(ride_length >= 90)

# Tìm giá trị ride_length dài nhất và ngắn nhất theo nhóm người 
all_trips %>% group_by(member_casual)%>% 
  summarise(max = max(ride_length), min = min(ride_length))

# Tính mỗi ngày có khoảng bao nhiêu chuyến và trung bình thời gian sử dụng dịch vụ là bao lâu
all_trips_summary <- all_trips %>%
  group_by(day_of_week, member_casual) %>%
  summarise(trip_count = n(),
  avg_ride_length = mean(ride_length, na.rm = TRUE)/60,
 .groups = "drop") %>%
  arrange(day_of_week, member_casual)


## Tính theo năm
# Năm 2019
mean(all_trips$ride_length[year(all_trips$started_at)== 2019],
	 na.rm = TRUE)

all_trips %>%
  filter(year(started_at) == 2019) %>% 
  group_by(member_casual) %>%
  summarise(
    n = n(), trip_count = n(),
    avg_ride_length = mean(ride_length, na.rm = TRUE),
    .groups = "drop"
  )

all_trips_2019_summary <- all_trips %>%
  filter(year(started_at) == 2019) %>%
  group_by(day_of_week, member_casual) %>%
  summarise(trip_count = n(),
  avg_ride_length = mean(ride_length, na.rm = TRUE)/60,
 .groups = "drop") %>%
  arrange(day_of_week, member_casual)


# Năm 2020
mean(all_trips$ride_length[year(all_trips$started_at)== 2020],
	 na.rm = TRUE)

all_trips %>%
  filter(year(started_at) == 2020) %>% 
  group_by(member_casual) %>%
  summarise(
    n = n(), trip_count = n(),
    avg_ride_length = mean(ride_length, na.rm = TRUE),
    .groups = "drop"
  )

all_trips_2020_summary <- all_trips %>%
  filter(year(started_at) == 2020) %>%
  group_by(day_of_week, member_casual) %>%
  summarise(trip_count = n(),
  avg_ride_length = mean(ride_length, na.rm = TRUE)/60,
 .groups = "drop") %>%
  arrange(day_of_week, member_casual)

## Vẽ biều dồ bằng ggplot2
# Thêm cột year
all_trips <- all_trips %>%
  mutate(year = year(started_at))

# Tạo và in ra biểu dồ thời gian di chuyển trung bình theo thứ trong tuần 
# phân loại theo nhóm người dùng từ năm 2019 - 2020
png("avg_ride_length.png", width = 1200, height = 600)
all_trips %>%
  filter(year %in% c(2019, 2020)) %>% # lọc theo trong năm 2019 và 2020
  group_by(year, day_of_week, member_casual) %>% # nhóm
  summarise(avg_ride_length = mean(ride_length, na.rm = TRUE), .groups = "drop") %>% # thêm tạm thời biến tính trung bình thời gian di chuyển
  ggplot(aes(x = factor(day_of_week, levels = 1:7), # chuyển day_of_weeks thành các factor cố định là 1, 2, 3, 4, 5, 6, 7
             y = avg_ride_length, 
             fill = factor(year))) + # factor() là 1 hàm phân loại. trong trường hợp này phân chia theo năm để tô màu
  geom_col(position = "dodge") + # vị trí canh nhau
  facet_wrap(~ member_casual) +
  scale_x_discrete(labels = c("1" = "CN", "2" = "T2", "3" = "T3", "4" = "T4", "5" = "T5", "6" = "T6", "7" = "T7")) +
  labs(title = "Thời gian di chuyển trung bình theo thứ trong tuần\nPhân loại theo người dùng: casual và member: 2019 vs 2020",
       x = "Thứ trong tuần",
       y = "Thời gian trung bình (giây)",
       fill = "Năm") +
  theme_minimal()
print()    
dev.off()

# Tạo và in ra biểu đồ số chuyến đi theo thứ trong tuần 
# phân loại theo nhóm người dùng từ năm 2019 - 2020
png("sum_rides.png", width = 1200, height = 600)
all_trips %>%
  filter(year %in% c(2019, 2020)) %>%
  group_by(year, day_of_week, member_casual) %>%
  summarise(sum_rides= n(),.groups = "drop") %>%
  ggplot(aes(x = factor(day_of_week, levels = 1:7), # chuyển day_of_weeks thành các factor cố định là 1, 2, 3, 4, 5, 6, 7
             y = sum_rides, 
             fill = factor(year))) + # factor() là 1 hàm phân loại. trong trường hợp này phân chia theo năm để tô màu
  geom_col(position = "dodge") + # đứng cạnh nhau
  facet_wrap(~ member_casual) +
  scale_x_discrete(labels = c("1" = "CN", "2" = "T2", "3" = "T3", "4" = "T4", "5" = "T5", "6" = "T6", "7" = "T7")) +
  labs(title = "Số chuyến đi theo thứ trong tuần\nPhân loại theo người dùng: casual và member: 2019 vs 2020",
       x = "Thứ trong tuần",
       y = "Số chuyến đi (chuyến)",
       fill = "Năm") +
  theme_minimal()
print()    
dev.off()

# Tạo và in ra biểu đồ số chuyến đi theo thứ trong tuần phân loại theo nhóm người dùng 
png("sum_rides.png", width = 800, height = 600)
all_trips %>%
  group_by(day_of_week, member_casual) %>%
  summarise(sum_rides= n(),.groups = "drop") %>%
  ggplot(aes(x = factor(day_of_week, levels = 1:7), # chuyển day_of_weeks thành các factor cố định là 1, 2, 3, 4, 5, 6, 7
             y = sum_rides)) + 
  geom_col(position = "dodge") + # đứng cạnh nhau
  facet_wrap(~ member_casual) +
  scale_x_discrete(labels = c("1" = "CN", "2" = "T2", "3" = "T3", "4" = "T4", "5" = "T5", "6" = "T6", "7" = "T7")) +
  labs(title = "Số chuyến đi theo thứ trong tuần\nPhân loại theo người dùng: casual và member",
       x = "Thứ trong tuần",
       y = "Số chuyến đi (chuyến)") +
  theme_minimal()
print()    
dev.off()

# Tạo và in ra biểu đồ thời gian di chuyển trung bình theo thứ trong tuần phân loại theo nhóm người dùng 
png("avg_ride_length.png", width = 800, height = 600)
all_trips %>%
  group_by(day_of_week, member_casual) %>%
  summarise(avg_ride_length = mean(ride_length, na.rm = TRUE),.groups = "drop") %>%
  ggplot(aes(x = factor(day_of_week, levels = 1:7), # chuyển day_of_weeks thành các factor cố định là 1, 2, 3, 4, 5, 6, 7
             y = avg_ride_length)) + 
  geom_col(position = "dodge") + # đứng cạnh nhau
  facet_wrap(~ member_casual) +
  scale_x_discrete(labels = c("1" = "CN", "2" = "T2", "3" = "T3", "4" = "T4", "5" = "T5", "6" = "T6", "7" = "T7")) +
  labs(title = "Thời gian di chuyển trung bình theo thứ trong tuần\nPhân loại theo người dùng: casual và member",
       x = "Thứ trong tuần",
       y = "Thời gian di chuyển trung bình (giây)") +
  theme_minimal()
print()    
dev.off()
