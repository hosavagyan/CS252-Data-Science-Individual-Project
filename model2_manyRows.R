library(dplyr)
library(stringr)
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
  mutate(website_views = as.numeric(str_remove_all(website_views, ","))) %>%
  filter(display_pixel_width != "1080,720", display_pixel_height != "1920,1280")

model2Data<- mobile_devices_filtered %>%
  select(c(3,5,7,8,9,10,14,30,33,35))
model2Data <- na.omit(model2Data)

i <- c(1:10)
model2Data[ , i] <- apply(model2Data[ , i], 2,            
                       function(x) as.numeric(x))

regModel2 <- lm(price~ display_size  + camera_megapixels +
                  battery_mah_size + ram_mb_size + memory_mb_size,
                data = model2Data)
summary(regModel2)
