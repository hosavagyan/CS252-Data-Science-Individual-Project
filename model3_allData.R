library(dplyr)
library(stringr)
library(ggplot2)
mobile_devices <- read.csv("all.csv")


brands <- unique(mobile_devices$brand)
network_types <- unique(mobile_devices$network_types)
mobile_devices_filtered <- mobile_devices %>%
  mutate(gpu = ifelse(gpu=='3D Graphics HW Accelerator','Nokia 3D Graphics HW Accelerator' ,gpu)) %>%
  mutate(gpu_brand = str_extract(gpu,'^([A-Za-z]+)')) %>%
  mutate(ram = ram_gb_size) %>%
  mutate(ram_mb_size = ifelse(str_detect(ram,'GB$'),
                              as.numeric(str_extract(ram,'^[0-9]+'))*1024,
                              as.numeric(str_extract(ram,'^[0-9]+')))) %>%
  mutate(memory = memory_gb_size) %>%
  mutate(memory_mb_size = ifelse(str_detect(memory,'GB$'),
                                 as.numeric(str_extract(memory,'^[0-9]+'))*1024,
                                 as.numeric(str_extract(memory,'^[0-9]+')))) %>%
  filter(video_pixels == str_subset(video_pixels, pattern = "\\d+p$"),
         price <= 750, camera_megapixels <=20, ram_mb_size<=7500) %>%
  mutate(video_pixels = as.numeric(str_remove_all(video_pixels, pattern = "p")),
         website_views = as.numeric(str_remove_all(website_views, ",")))

model3Data<- mobile_devices_filtered %>%
  select(c(3,5,7,8,9,10,11,14,30,33,35))

model3Data <- na.omit(model3Data)

i <- c(1:10)
model3Data[ , i] <- apply(model3Data[ , i], 2,            
                          function(x) as.numeric(x))

# model3Data <- model3Data %>%
#   filter(display_pixel_width <= 2000)
colnames(model3Data)[4:5] <- c("display_pixel_height", "display_pixel_width")

regModel3 <- lm(price~ display_size +camera_megapixels+ 
                  battery_mah_size + ram_mb_size + memory_mb_size +
                  video_pixels,
                data = model3Data)
summary(regModel3)
plot(regModel3)

regModel4<- lm(price~ camera_megapixels*video_pixels + display_size + battery_mah_size+
                  ram_mb_size + memory_mb_size, data = model3Data)
    
summary(regModel4)

model3Data %>%
  ggplot(aes(x = ram_mb_size, y = price)) + geom_point() + xlim(c(0,8000)) +
  ylim(c(0,600)) + geom_smooth(method = "lm")
options(scipen = 5)

model3Data %>%
  ggplot(aes(x = release_year, y = website_views)) + 
  geom_histogram(stat = "identity") + xlim(c(2009,2020)) 


  



