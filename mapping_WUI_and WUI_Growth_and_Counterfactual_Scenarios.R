# ---------------------------------------------------------------------------- #

# Packages
# install.packages(raster)
library(raster)

# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# import input features
# ---------------------------------------------------------------------------- #

# data input path
in_path = ".../data/"
out_path = ".../data/"


# ---------------------------------------------------------------------------- #
# Landsat based land cover fraction
# ---------------------------------------------------------------------------- #

# processing steps

# - pixel with > 20 % water are set to 0 % impervious fractions
# - pixels with 1 % to 20 % impervious fractions are set to 0 % impervious fractions
# - calculate 500m buffer average of impervious fraction


# ---------------------------------------------------------------------------- #
# building density approximated via impervious fractions
# ---------------------------------------------------------------------------- #
#   processing steps: 
#    - pixel with > 20 % water are set to 0 % impervious fractions
#    - pixels with 1 % to 20 % impervious fractions are set to 0% impervious fractions
#    - then calculate 500 m buffer average of impervious fraction
impervious_500m_avg_1990 = stack(paste0(in_path, "impervious_500m_avg_1990.tif"))
impervious_500m_avg_2020 = stack(paste0(in_path, "impervious_500m_avg_2020.tif"))

# rasters are scaled between 0 to 10,000
# re-scale raster to percentage scale

impervious_500m_avg_1990 = impervious_500m_avg_1990 / 100
impervious_500m_avg_2020 = impervious_500m_avg_2020 / 100
# ---------------------------------------------------------------------------- #
# intermix vegetation 
# ---------------------------------------------------------------------------- #

#   woody vegetation processing steps: 
#    - pixel with > 20 % water are set to 0 % fractions
#    - pixels with > 30 % impervious fractions are set to 0 % woody fractions (assumed non-natural due to human presence and avoid misclassification of suburbs as intermix)
#    - then calculate 500 m buffer average of woody fraction
#    - then estimate where a pixel average >= 50 % 
woody_500m_avg_1990 = stack(paste0(in_path, "woody_500m_avg_1990.tif"))
woody_500m_avg_2020 = stack(paste0(in_path, "woody_500m_avg_2020.tif"))

# rasters are scaled between 0 to 10,000
# re-scale raster to percentage scale
woody_500m_avg_1990 = woody_500m_avg_1990 / 100
woody_500m_avg_2020 = woody_500m_avg_2020 / 100

# ---------------------------------------------------------------------------- #
#   non-woody vegetation processing steps: 
#    - pixel with > 20 % water are set to 0 % fractions
#    - pixels with > 30 % impervious fractions are set to 0 % non-woody fractions (assumed non-natural due to human presence and avoid misclassification of suburbs as intermix)
#    - pixel with identified cropland (for Adeladie via EVI skewness) 0 % fractions
#    - then calculate 500 m buffer average of woody fraction
#    - then estimate where a pixel average >= 50 % 
nonwoody_500m_avg_1990 = stack(paste0(in_path, "nonwoody_500m_avg_1990.tif"))
nonwoody_500m_avg_2020 = stack(paste0(in_path, "nonwoody_500m_avg_2020.tif"))

# rasters are scaled between 0 to 10,000
# re-scale raster to percentage scale
nonwoody_500m_avg_1990 = nonwoody_500m_avg_1990 / 100
nonwoody_500m_avg_2020 = nonwoody_500m_avg_2020 / 100

# ---------------------------------------------------------------------------- #
# interface vegetation 
# ---------------------------------------------------------------------------- #

#   woody vegetation processing steps: 
#    - pixel with > 20 % water are set to 0 % fractions
#    - pixels with > 30 % impervious fractions are set to 0 % non-woody fractions (assumed non-natural due to human presence and avoid misclassification of suburbs as intermix)
#    - then find clusters of > 5sqkm with > 50 % woody fraction
#    - buffer indentified clusters by 2400m 
woody_cluster5sqkm_buffer2400m_1990 = stack(paste0(in_path, "woody_interface_cluster5sqkm_buffer2400m_1990.tif"))
woody_cluster5sqkm_buffer2400m_2020 = stack(paste0(in_path, "woody_interface_cluster5sqkm_buffer2400m_2020.tif"))

# ---------------------------------------------------------------------------- #
#   non-woody vegetation processing steps: 
#    - pixel with > 20 % water are set to 0 % fractions
#    - pixels with > 30 % impervious fractions are set to 0 % non-woody fractions (assumed non-natural due to human presence and avoid misclassification of suburbs as intermix)
#    - pixel with identified cropland (for Adeladie via EVI skewness) 0 % fractions
#    - then find clusters of > 5sqkm with > 50 % non-woody fraction
#    - buffer indentified clusters by 2400m 
nonwoody_cluster5sqkm_buffer2400m_1990 = stack(paste0(in_path, "nonwoody_interface_cluster5sqkm_buffer2400m_1990.tif"))
nonwoody_cluster5sqkm_buffer2400m_2020 = stack(paste0(in_path, "nonwoody_interface_cluster5sqkm_buffer2400m_2020.tif"))


# ---------------------------------------------------------------------------- #

# import water mask
water_mask = stack(paste0(in_path, "water.tif")) #???

# ---------------------------------------------------------------------------- #
# Mapping the WUI
# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
# Factual WUI classification (hierarchical)
# ---------------------------------------------------------------------------- #

# 1. WUI in 1990 (impervious from 1990 & vegetation from 1990)

# create empty raster for the WUI map with similar characteristics as the impervious raster
wui_1990 = raster(impervious_500m_avg_2020)
# set all raster values to zero
wui_1990 = setValues(wui_1990, 0) 


# non-woody interface WUI
wui_1990[impervious_500m_avg_1990 >= 0.2 & nonwoody_cluster5sqkm_buffer2400m_1990 > 0] = 4
# woody interface WUI
wui_1990[impervious_500m_avg_1990 >= 0.2 & woody_cluster5sqkm_buffer2400m_1990 > 0] = 2

# non-woody intermix WUI
wui_1990[impervious_500m_avg_1990 >= 0.2 & nonwoody_500m_avg_1990 > 50] = 3
# woody intermix WUI
wui_1990[impervious_500m_avg_1990 >= 0.2 & woody_500m_avg_1990 > 50] = 1


# masked water > 20 %
wui_1990[water_mask > 20] <- NA # water mask

plot(wui_1990)

# ------------------

# 2. WUI in 2020 (impervious from 2020 & vegetation from 2020)

# create empty raster for the WUI map with similar characteristics as the impervious raster
wui_2020 = raster(impervious_500m_avg_2020)
# set all raster values to zero
wui_2020 = setValues(wui_2020, 0) 


# non-woody interface WUI
wui_2020[impervious_500m_avg_2020 >= 0.2 & nonwoody_cluster5sqkm_buffer2400m_2020 > 0] = 4
# woody interface WUI
wui_2020[impervious_500m_avg_2020 >= 0.2 & woody_cluster5sqkm_buffer2400m_2020 > 0] = 2

# non-woody intermix WUI
wui_2020[impervious_500m_avg_2020 >= 0.2 & nonwoody_500m_avg_2020 > 50] = 3
# woody intermix WUI
wui_2020[impervious_500m_avg_2020 >= 0.2 & woody_500m_avg_2020 > 50] = 1


# masked water > 20 %
wui_2020[water_mask > 20] <- NA # water mask

plot(wui_2020)




# ---------------------------------------------------------------------------- #
# Counterfactual WUI classification (hierarchical)
# ---------------------------------------------------------------------------- #


# 1. Building Scenario (impervious from 2020 & vegetation from 1990)

# create empty raster for the WUI map with similar characteristics as the impervious raster
building_scenario = raster(impervious_500m_avg_2020)
# set all raster values to zero
building_scenario = setValues(building_scenario, 0) 

# non-woody interface WUI
building_scenario[impervious_500m_avg_2020 >= 0.2 & nonwoody_cluster5sqkm_buffer2400m_1990 > 0] = 4
# woody interface WUI
building_scenario[impervious_500m_avg_2020 >= 0.2 & woody_cluster5sqkm_buffer2400m_1990 > 0] = 2

# non-woody intermix WUI
building_scenario[impervious_500m_avg_2020 >= 0.2 & nonwoody_500m_avg_1990 > 50] = 3
# woody intermix WUI
building_scenario[impervious_500m_avg_2020 >= 0.2 & woody_500m_avg_1990 > 50] = 1


# masked water > 20 %
building_scenario[water_mask > 20] <- NA # water mask

plot(building_scenario)

# ------------------

# 2. Vegetation Scenario (impervious from 1990 & vegetation from 2020)

# create empty raster for the WUI map with similar characteristics as the impervious raster
vegetation_scenario = raster(impervious_500m_avg_2020)
# set all raster values to zero
vegetation_scenario = setValues(vegetation_scenario, 0) 


# non-woody interface WUI
vegetation_scenario[impervious_500m_avg_1990 >= 0.2 & nonwoody_cluster5sqkm_buffer2400m_2020 > 0] = 4
# woody interface WUI
vegetation_scenario[impervious_500m_avg_1990 >= 0.2 & woody_cluster5sqkm_buffer2400m_2020 > 0] = 2

# non-woody intermix WUI
vegetation_scenario[impervious_500m_avg_1990 >= 0.2 & nonwoody_500m_avg_2020 > 50] = 3
# woody intermix WUI
vegetation_scenario[impervious_500m_avg_1990 >= 0.2 & woody_500m_avg_2020 > 50] = 1


# masked water > 20 %
vegetation_scenario[water_mask > 20] <- NA # water mask

plot(vegetation_scenario)



# ---------------------------------------------------------------------------- #
# write the factual and counterfactual WUI classification to raster file (GeoTiff)
# ---------------------------------------------------------------------------- #

writeRaster(wui_1990, 
            paste0(out_path, "wui_1990.tif"),
            overwrite = TRUE)

writeRaster(wui_2020, 
            paste0(out_path, "wui_2020.tif"),
            overwrite = TRUE)

writeRaster(building_scenario, 
            paste0(out_path, "building_scenario.tif"),
            overwrite = TRUE)

writeRaster(vegetation_scenario, 
            paste0(out_path, "vegetation_scenario.tif"),
            overwrite = TRUE)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# for creating a nice map use package tmap
# install.packages("tmap")
library(tmap)

# ---------------------------------------------------------------------------- #
# plot factual and counterfactual WUI map with tmap package
# ---------------------------------------------------------------------------- #

# WUI in 1990

tm_shape(wui_1990)+
  tm_raster(breaks = c(0, 1, 2, 3, 4),
            style = "cat",
            palette = c("#FFFFFF",
                        "#ed8100", 
                        "#FFFF00",
                        "#5f5134",
                        "#ae932e"),
            labels = c("Non-WUI","1 = intermix WUI (woody)",
                       "2 = interface WUI (woody)",
                       "3 = intermix WUI (non-woody)",
                       "4 = interface WUI (non-woody)"),
            title = "legend")+
  tm_layout(panel.labels = c("Landsat-based WUI in 1990"))+
  tm_layout(legend.position = c("left", "bottom"))

# ---------------------------------------------------------------------------- #

# WUI in 2020

tm_shape(wui_2020)+
  tm_raster(breaks = c(0, 1, 2, 3, 4),
            style = "cat",
            palette = c("#FFFFFF",
                        "#ed8100", 
                        "#FFFF00",
                        "#5f5134",
                        "#ae932e"),
            labels = c("Non-WUI","1 = intermix WUI (woody)",
                       "2 = interface WUI (woody)",
                       "3 = intermix WUI (non-woody)",
                       "4 = interface WUI (non-woody)"),
            title = "legend")+
  tm_layout(panel.labels = c("Landsat-based WUI in 2020"))+
  tm_layout(legend.position = c("left", "bottom"))

# ---------------------------------------------------------------------------- #

# Building Scenario

tm_shape(building_scenario)+
  tm_raster(breaks = c(0, 1, 2, 3, 4),
            style = "cat",
            palette = c("#FFFFFF",
                        "#ed8100", 
                        "#FFFF00",
                        "#5f5134",
                        "#ae932e"),
            labels = c("Non-WUI","1 = intermix WUI (woody)",
                       "2 = interface WUI (woody)",
                       "3 = intermix WUI (non-woody)",
                       "4 = interface WUI (non-woody)"),
            title = "legend")+
  tm_layout(panel.labels = c("Building Scenario WUI"))+
  tm_layout(legend.position = c("left", "bottom"))

# ---------------------------------------------------------------------------- #

# Vegetation Scenario

tm_shape(vegetation_scenario)+
  tm_raster(breaks = c(0, 1, 2, 3, 4),
            style = "cat",
            palette = c("#FFFFFF",
                        "#ed8100", 
                        "#FFFF00",
                        "#5f5134",
                        "#ae932e"),
            labels = c("Non-WUI","1 = intermix WUI (woody)",
                       "2 = interface WUI (woody)",
                       "3 = intermix WUI (non-woody)",
                       "4 = interface WUI (non-woody)"),
            title = "legend")+
  tm_layout(panel.labels = c("Vegetation Scenario WUI"))+
  tm_layout(legend.position = c("left", "bottom"))




# ---------------------------------------------------------------------------- #
# WUI changes
# ---------------------------------------------------------------------------- #

# create binary WUI classes (0 = non-WUI; 1 = WUI (WUI classes ))
# 0 & NA = non-WUI
# 1 = Intermix WUI (woody)
# 2 = Intermix WUI (non-woody)
# 3 = Interface WUI (woody)
# 4 = Interface WUI (non-woody)
wui_1990[wui_1990 > 1] = 1
wui_2020[wui_2020 > 1] = 1


# create empty raster with 0
rs_wui_change = raster(wui_2020)
setValues(rs_wui_change, 0)

rs_wui_change[wui_1990 == 1 & wui_2020 == 1] = 1 # stable WUI
rs_wui_change[wui_1990 == 0 & wui_2020 == 1] = 2 # WUI growth
rs_wui_change[wui_1990 == 1 & wui_2020 == 0] = 3 # WUI loss
rs_wui_change[wui_1990 == 0 & wui_2020 == 0] = 0 # stable non-WUI

plot(rs_wui_change)

# Plot WUI Change

tm_shape(rs_wui_change)+
  tm_raster(breaks = c(0, 1, 2, 3),
            style = "cat",
            palette = c("#CCCCCC",
                        "#ffffff",
                        "#CC0000", 
                        "#0000CC"),
            labels = c("0 = Non-WUI",
                       "1 = Stable WUI (woody)",
                       "2 = WUI growth",
                       "3 = WUI loss"),
            title = "legend")+
  tm_layout(panel.labels = c("WUI Change from 1990 to 2020"))+
  tm_layout(legend.position = c("left", "bottom"))


# ---------------------------------------------------------------------------- #
# Comparing WUI maps with the two factual WUI maps from 1990 & 2020
# ---------------------------------------------------------------------------- #


# stack layers together (factual and counterfactual WUI maps)

rs <- stack(wui_1990,
            wui_2020,
            building_scenario,
            vegetation_scenario
            )

# convert raster to data.frame()

df <- as.data.frame(rs, xy = TRUE) # xy = TRUE add coordinate; xy = FALSE does not add coordinates
names(df) <- c("x", "y", "wui_1990", "wui_2020", "building_scenario", "vegetation_scenario")

# ---------------------------------------------------------------------------- #
# do statistical comparison
# ---------------------------------------------------------------------------- #

# selecting reference map and scenario map
reference <- df$wui_2020
prediction <- df$building_scenario

# Keep pixels with valid values in both maps
valid <- !is.na(reference) & !is.na(prediction)
reference <- reference[valid]
prediction <- prediction[valid]


TP <- sum(reference == 1 & prediction == 1)  # Both WUI
TN <- sum(reference == 0 & prediction == 0)  # Both non-WUI
FP <- sum(reference == 0 & prediction == 1)  # Scenario says WUI
FN <- sum(reference == 1 & prediction == 0)  # Scenario misses WUI

# --------------

# Overall accuracy (OA)
# Proportion of all pixels classified identically.

OA <- (TP + TN) / (TP + TN + FP + FN)
OA

# --------------

# Jaccard index (intersection over union)
# Measures overlap between WUI pixels in the two maps, ignoring agreement on non-WUI pixels.

Jaccard <- TP / (TP + FP + FN)
Jaccard
# --------------

# Correlation (Pearson / phi coefficient for binary maps)
# Measures the strength and direction of association between the two binary maps.

Correlation <- cor(reference, prediction)
Correlation


# ---------------------------------------------------------------------------- #
# the end.
# ---------------------------------------------------------------------------- #


