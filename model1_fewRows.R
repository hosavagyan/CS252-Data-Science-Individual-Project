library(dplyr)
library(stringr)
data <- read.csv("gsmarena_mobile_devices.csv")
unique(data$brand)
unique(data$cpu)
unique(data$gpu)


  

mobile_devices  <- read.csv('final.csv')



  



brands <- unique(mobile_devices$brand)
network_types <- unique(mobile_devices$network_types)
mobile_devices_filtered <- mobile_devices %>%
  mutate(gpu = ifelse(gpu=='3D Graphics HW Accelerator','Nokia 3D Graphics HW Accelerator' ,gpu)) %>%
  mutate(gpu_brand = str_extract(gpu,'^([A-Za-z]+)')) %>%
  mutate(ram = ram_gb_size) %>%
  mutate(ram_mb_size = ifelse(str_detect(ram,'GB$'),
                              as.numeric(str_extract(ram,'^[0-9]+'))*1024,
                              str_extract(ram,'^[0-9]+'))) %>%
  mutate(memory = memory_gb_size) %>%
  mutate(memory_mb_size = ifelse(str_detect(memory,'GB$'),
                                 as.numeric(str_extract(memory,'^[0-9]+'))*1024,
                                 str_extract(memory,'^[0-9]+'))) 
mobile_devices_filtered <- mobile_devices_filtered %>%
  filter(video_pixels == str_subset(video_pixels, pattern = "\\d+p$")) %>%
  mutate(video_pixels = str_remove_all(video_pixels, pattern = "p"))

mobile_devices_filtered <- mobile_devices_filtered %>%
  mutate(website_views = as.numeric(str_remove_all(website_views, ",")))





regData<- mobile_devices_filtered %>%
  select(c(3,5,7,8,9,10,11,14,30,33,35))
regData <- na.omit(regData)

i <- c(1:11)
regData[ , i] <- apply(regData[ , i], 2,            
                       function(x) as.numeric(x))

regModel1 <- lm(price~  camera_megapixels +
                  battery_mah_size  + memory_mb_size + 
                  video_pixels, data = regData)

summary(regModel1)

