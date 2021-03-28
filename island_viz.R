install.packages(c("rayshader", "raster", "sp"))
install.packages("rgdal")
install.packages("webshot2")
install.packages("magick")
install.packages("rayrender")
library(rayshader)
library(sp)
library(raster)
library(scales)
library(dplyr)
library(webshot2)
library(magick)
library(rayrender)

#bora bora

islands_elevation = raster::raster("C:\\Users\\dowoo\\PycharmProjects\\r_3d_islands\\S17W152.hgt")

height_shade(raster_to_matrix(islands_elevation)) %>%
  plot_map()

islands_r = raster::raster("C:\\Users\\dowoo\\PycharmProjects\\r_3d_islands\\borabora_data\\LC08_L2SR_055071_20201009_20201016_02_T1_SR_B4.TIF")
islands_g = raster::raster("C:\\Users\\dowoo\\PycharmProjects\\r_3d_islands\\borabora_data\\LC08_L2SR_055071_20201009_20201016_02_T1_SR_B3.TIF")
islands_b = raster::raster("C:\\Users\\dowoo\\PycharmProjects\\r_3d_islands\\borabora_data\\LC08_L2SR_055071_20201009_20201016_02_T1_SR_B2.TIF")

islands_rbg = raster::stack(islands_r, islands_g, islands_b)
raster::plotRGB(islands_rbg, scale=255^2)

islands_rbg_corrected = sqrt(raster::stack(islands_r, islands_g, islands_b))
raster::plotRGB(islands_rbg_corrected)

crs(islands_r)
islands_elevation_utm = raster::projectRaster(islands_elevation, crs = crs(islands_r), method = "bilinear")
crs(islands_elevation_utm)

bottom_left = c(y=-151.82186, x=-16.56649)
top_right   = c(y=-151.67371, x=-16.42922)

extent_latlong = sp::SpatialPoints(rbind(bottom_left, top_right), proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
extent_utm = sp::spTransform(extent_latlong, raster::crs(islands_elevation_utm))

e = raster::extent(extent_utm)
e

islands_rgb_cropped = raster::crop(islands_rbg_corrected, e)
elevation_cropped = raster::crop(islands_elevation_utm, e)

names(islands_rgb_cropped) = c("r","g","b")

islands_r_cropped = rayshader::raster_to_matrix(islands_rgb_cropped$r)
islands_g_cropped = rayshader::raster_to_matrix(islands_rgb_cropped$g)
islands_b_cropped = rayshader::raster_to_matrix(islands_rgb_cropped$b)

islandsel_matrix = rayshader::raster_to_matrix(elevation_cropped)

islands_rgb_array = array(0,dim=c(nrow(islands_r_cropped),ncol(islands_r_cropped),3))

islands_rgb_array[,,1] = islands_r_cropped/255 #Red layer
islands_rgb_array[,,2] = islands_g_cropped/255 #Blue layer
islands_rgb_array[,,3] = islands_b_cropped/255 #Green layer

islands_rgb_array = aperm(islands_rgb_array, c(2,1,3))

plot_map(islands_rgb_array)

islands_rgb_contrast = scales::rescale(islands_rgb_array,to=c(0,1))

plot_map(islands_rgb_contrast)

plot_3d(islands_rgb_contrast, islandsel_matrix, windowsize = c(1100,900), zscale = 15, shadowdepth = -50,
        zoom=0.5, phi=45,theta=-45,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B")
render_highquality()
render_camera(theta=-45,phi=60,fov=60,zoom=0.8)
render_highquality(lightdirection = c(0,120,240), lightaltitude=45, 
                   lightcolor=c("red","green","blue"),
                   scene_elements = rayrender::sphere(z=-60,y=0,
                                                      radius=20,material=rayrender::metal()))

render_snapshot(title_text = "Bora Bora, French Polynesia | Imagery: Landsat | DEM: 30m SRTM",
                title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)

angles= seq(0,360,length.out = 1441)[-1]
for(i in 1:1440) {
  render_camera(theta=-45+angles[i])
  render_snapshot(filename = sprintf("borabora%i.png", i), 
                  title_text = "Bora Bora, French Polynesia | Imagery: Landsat | DEM: 30m SRTM",
                  title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)
}
rgl::rgl.close()

#av::av_encode_video(sprintf("zionpark%d.png",seq(1,1440,by=1)), framerate = 30,
# output = "zionpark.mp4")

rgl::rgl.close()
system("ffmpeg -framerate 60 -i borabora%d.png -pix_fmt yuv420p borabora.mp4")

##########################################################################################################

#reunion

elevation_1 = raster::raster("C:\\Users\\dowoo\\PycharmProjects\\r_3d_islands\\reunion_elevation_data\\S21E055.hgt") #update
elevation_2 = raster::raster("C:\\Users\\dowoo\\PycharmProjects\\r_3d_islands\\reunion_elevation_data\\S22E055.hgt") #update

islands_elevation <- raster::merge(elevation_1,elevation_2)
 
height_shade(raster_to_matrix(islands_elevation)) %>%
  plot_map()

#update

islands_r = raster::raster("C:\\Users\\dowoo\\PycharmProjects\\r_3d_islands\\reunion_data\\LC08_L2SP_153075_20210301_20210311_02_T1_SR_B4.TIF")
islands_g = raster::raster("C:\\Users\\dowoo\\PycharmProjects\\r_3d_islands\\reunion_data\\LC08_L2SP_153075_20210301_20210311_02_T1_SR_B3.TIF")
islands_b = raster::raster("C:\\Users\\dowoo\\PycharmProjects\\r_3d_islands\\reunion_data\\LC08_L2SP_153075_20210301_20210311_02_T1_SR_B2.TIF")

islands_rbg = raster::stack(islands_r, islands_g, islands_b)
raster::plotRGB(islands_rbg, scale=255^2)

islands_rbg_corrected = sqrt(raster::stack(islands_r, islands_g, islands_b))
raster::plotRGB(islands_rbg_corrected)

crs(islands_r)
islands_elevation_utm = raster::projectRaster(islands_elevation, crs = crs(islands_r), method = "bilinear")
crs(islands_elevation_utm)

bottom_left = c(y=55.14826, x=-21.40786) #update
top_right   = c(y=55.88984, x=-20.84487) #update 

extent_latlong = sp::SpatialPoints(rbind(bottom_left, top_right), proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
extent_utm = sp::spTransform(extent_latlong, raster::crs(islands_elevation_utm))

e = raster::extent(extent_utm)
e

islands_rgb_cropped = raster::crop(islands_rbg_corrected, e)
elevation_cropped = raster::crop(islands_elevation_utm, e)

names(islands_rgb_cropped) = c("r","g","b")

islands_r_cropped = rayshader::raster_to_matrix(islands_rgb_cropped$r)
islands_g_cropped = rayshader::raster_to_matrix(islands_rgb_cropped$g)
islands_b_cropped = rayshader::raster_to_matrix(islands_rgb_cropped$b)

islandsel_matrix = rayshader::raster_to_matrix(elevation_cropped)

islands_rgb_array = array(0,dim=c(nrow(islands_r_cropped),ncol(islands_r_cropped),3))

islands_rgb_array[,,1] = islands_r_cropped/255 #Red layer
islands_rgb_array[,,2] = islands_g_cropped/255 #Blue layer
islands_rgb_array[,,3] = islands_b_cropped/255 #Green layer

islands_rgb_array = aperm(islands_rgb_array, c(2,1,3))

plot_map(islands_rgb_array)

islands_rgb_contrast = scales::rescale(islands_rgb_array,to=c(0,1))

plot_map(islands_rgb_contrast)

plot_3d(islands_rgb_contrast, islandsel_matrix, windowsize = c(1100,900), zscale = 5, shadowdepth = -50,
        zoom=0.5, phi=45,theta=-45,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B")
render_snapshot(title_text = "La Reunion, Indian Ocean | Imagery: Landsat 8 | DEM: 30m SRTM",
                title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)

plot_3d(islands_rgb_contrast, islandsel_matrix, windowsize = c(1100,900), zscale = 5, shadowdepth = -50,
        zoom=0.5, phi=45,theta=-45,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B")
render_highquality()
render_snapshot(title_text = "La Reunion, Indian Ocean | Imagery: Landsat 8 | DEM: 30m SRTM",
                title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)

angles= seq(0,360,length.out = 1441)[-1]
for(i in 1:1440) {
  render_camera(theta=-45+angles[i])
  render_snapshot(filename = sprintf("lareunion%i.png", i), 
                  title_text = "La Reunion, Indian Ocean | Imagery: Landsat 8 | DEM: 30m SRTM",
                  title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)
}
rgl::rgl.close()

#av::av_encode_video(sprintf("zionpark%d.png",seq(1,1440,by=1)), framerate = 30,
# output = "zionpark.mp4")

rgl::rgl.close()
system("ffmpeg -framerate 60 -i lareunion%d.png -pix_fmt yuv420p lareunion.mp4")

##########################################################################################################

#guadeloupe


elevation_1 = raster::raster("C:\\Users\\dowoo\\PycharmProjects\\r_3d_islands\\guadeloupe_elevation_data\\N15W062.hgt") #update
elevation_2 = raster::raster("C:\\Users\\dowoo\\PycharmProjects\\r_3d_islands\\guadeloupe_elevation_data\\N16W062.hgt") #update
elevation_3 = raster::raster("C:\\Users\\dowoo\\PycharmProjects\\r_3d_islands\\guadeloupe_elevation_data\\N16W063.hgt") #update

islands_elevation <- raster::merge(elevation_1, elevation_2, elevation_3)

height_shade(raster_to_matrix(islands_elevation)) %>%
  plot_map()

#update

islands_r = raster::raster("C:\\Users\\dowoo\\PycharmProjects\\r_3d_islands\\guadeloupe_data\\LC08_L2SP_001049_20201031_20201106_02_T1_SR_B4.TIF")
islands_g = raster::raster("C:\\Users\\dowoo\\PycharmProjects\\r_3d_islands\\guadeloupe_data\\LC08_L2SP_001049_20201031_20201106_02_T1_SR_B3.TIF")
islands_b = raster::raster("C:\\Users\\dowoo\\PycharmProjects\\r_3d_islands\\guadeloupe_data\\LC08_L2SP_001049_20201031_20201106_02_T1_SR_B2.TIF")

islands_rbg = raster::stack(islands_r, islands_g, islands_b)
raster::plotRGB(islands_rbg, scale=255^2)

islands_rbg_corrected = sqrt(raster::stack(islands_r, islands_g, islands_b))
raster::plotRGB(islands_rbg_corrected)

crs(islands_r)
islands_elevation_utm = raster::projectRaster(islands_elevation, crs = crs(islands_r), method = "bilinear")
crs(islands_elevation_utm)

bottom_left = c(y=-61.83614, x=15.93849) #update
top_right   = c(y=-61.17215, x=16.55618) #update

extent_latlong = sp::SpatialPoints(rbind(bottom_left, top_right), proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
extent_utm = sp::spTransform(extent_latlong, raster::crs(islands_elevation_utm))

e = raster::extent(extent_utm)
e

islands_rgb_cropped = raster::crop(islands_rbg_corrected, e)
elevation_cropped = raster::crop(islands_elevation_utm, e)

names(islands_rgb_cropped) = c("r","g","b")

islands_r_cropped = rayshader::raster_to_matrix(islands_rgb_cropped$r)
islands_g_cropped = rayshader::raster_to_matrix(islands_rgb_cropped$g)
islands_b_cropped = rayshader::raster_to_matrix(islands_rgb_cropped$b)

islandsel_matrix = rayshader::raster_to_matrix(elevation_cropped)

islands_rgb_array = array(0,dim=c(nrow(islands_r_cropped),ncol(islands_r_cropped),3))

islands_rgb_array[,,1] = islands_r_cropped/255 #Red layer
islands_rgb_array[,,2] = islands_g_cropped/255 #Blue layer
islands_rgb_array[,,3] = islands_b_cropped/255 #Green layer

islands_rgb_array = aperm(islands_rgb_array, c(2,1,3))

plot_map(islands_rgb_array)

islands_rgb_contrast = scales::rescale(islands_rgb_array,to=c(0,1))

plot_map(islands_rgb_contrast)

plot_3d(islands_rgb_contrast, islandsel_matrix, windowsize = c(1100,900), zscale = 15, shadowdepth = -50,
        zoom=0.5, phi=45,theta=-45,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B")
render_snapshot(title_text = "Guadeloupe, French Caribbean | Imagery: Landsat 8 | DEM: 30m SRTM",
                title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)

angles= seq(0,360,length.out = 1441)[-1]
for(i in 1:1440) {
  render_camera(theta=-45+angles[i])
  render_snapshot(filename = sprintf("guadeloupe%i.png", i), 
                  title_text = "Guadeloupe, French Caribbean | Imagery: Landsat 8 | DEM: 30m SRTM",
                  title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)
}
rgl::rgl.close()

#av::av_encode_video(sprintf("guadeloupe%d.png",seq(1,1440,by=1)), framerate = 30,
# output = "zionpark.mp4")

rgl::rgl.close()
system("ffmpeg -framerate 60 -i guadeloupe%d.png -pix_fmt yuv420p guadeloupe.mp4")

##########################################################################################################

#yasawas


elevation_1 = raster::raster("C:\\Users\\dowoo\\PycharmProjects\\r_3d_islands\\yasawa_elevation_data\\S17E177.hgt") #update
elevation_2 = raster::raster("C:\\Users\\dowoo\\PycharmProjects\\r_3d_islands\\yasawa_elevation_data\\S18E176.hgt") #update
elevation_3 = raster::raster("C:\\Users\\dowoo\\PycharmProjects\\r_3d_islands\\yasawa_elevation_data\\S18E177.hgt") #update

islands_elevation <- raster::merge(elevation_1,elevation_2, elevation_3)

height_shade(raster_to_matrix(islands_elevation)) %>%
  plot_map()

#update

islands_r = raster::raster("C:\\Users\\dowoo\\PycharmProjects\\r_3d_islands\\yasawa_data\\LC08_L2SR_075072_20200903_20200923_02_T1_SR_B4.TIF")
islands_g = raster::raster("C:\\Users\\dowoo\\PycharmProjects\\r_3d_islands\\yasawa_data\\LC08_L2SR_075072_20200903_20200923_02_T1_SR_B3.TIF")
islands_b = raster::raster("C:\\Users\\dowoo\\PycharmProjects\\r_3d_islands\\yasawa_data\\LC08_L2SR_055071_20210214_20210301_02_T1_SR_B2.TIF")

islands_rbg = raster::stack(islands_r, islands_g, islands_b)
raster::plotRGB(islands_rbg, scale=255^2)

islands_rbg_corrected = sqrt(raster::stack(islands_r, islands_g, islands_b))
raster::plotRGB(islands_rbg_corrected)

crs(islands_r)
islands_elevation_utm = raster::projectRaster(islands_elevation, crs = crs(islands_r), method = "bilinear")
crs(islands_elevation_utm)

bottom_left = c(y=176.85976, x=-17.86746) #update
top_right   = c(y=177.62301, x=-16.66571) #update 

extent_latlong = sp::SpatialPoints(rbind(bottom_left, top_right), proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
extent_utm = sp::spTransform(extent_latlong, raster::crs(islands_elevation_utm))

e = raster::extent(extent_utm)
e

islands_rgb_cropped = raster::crop(islands_rbg_corrected, e)
elevation_cropped = raster::crop(islands_elevation_utm, e)

names(islands_rgb_cropped) = c("r","g","b")

islands_r_cropped = rayshader::raster_to_matrix(islands_rgb_cropped$r)
islands_g_cropped = rayshader::raster_to_matrix(islands_rgb_cropped$g)
islands_b_cropped = rayshader::raster_to_matrix(islands_rgb_cropped$b)

islandsel_matrix = rayshader::raster_to_matrix(elevation_cropped)

islands_rgb_array = array(0,dim=c(nrow(islands_r_cropped),ncol(islands_r_cropped),3))

islands_rgb_array[,,1] = islands_r_cropped/255 #Red layer
islands_rgb_array[,,2] = islands_g_cropped/255 #Blue layer
islands_rgb_array[,,3] = islands_b_cropped/255 #Green layer

islands_rgb_array = aperm(islands_rgb_array, c(2,1,3))

plot_map(islands_rgb_array)

islands_rgb_contrast = scales::rescale(islands_rgb_array,to=c(0,1))

#plot_map(islands_rgb_contrast)

plot_3d(islands_rgb_contrast, islandsel_matrix, windowsize = c(1100,900), zscale = 15, shadowdepth = -50,
        zoom=0.5, phi=45,theta=-45,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B")
render_snapshot(title_text = "Yasawa Islands, Fiji | Imagery: Landsat 8 | DEM: 30m SRTM",
                title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)

angles= seq(0,360,length.out = 1441)[-1]
for(i in 1:1440) {
  render_camera(theta=-45+angles[i])
  render_snapshot(filename = sprintf("yasawas%i.png", i), 
                  title_text = "Yasawa Islands, Fiji | Imagery: Landsat 8 | DEM: 30m SRTM",
                  title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)
}
rgl::rgl.close()

#av::av_encode_video(sprintf("yasawas%d.png",seq(1,1440,by=1)), framerate = 30,
# output = "zionpark.mp4")

rgl::rgl.close()
system("ffmpeg -framerate 60 -i yasawas%d.png -pix_fmt yuv420p yasawas.mp4")