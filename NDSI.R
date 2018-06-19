library(sp)
library(raster)
library(rgdal)
library(landsat)
library(RStoolbox)

rasterOptions(maxmemory = 20000)

setwd("/Volumes/vale_a/0000_Imagenes_sur/233_83/233_83_extras_") # cambiar ruta para cada set de imagenes de acuerdo a la carpeta que corresponde

r_files <- list.files(full.names = TRUE) # volver a correr cada vez que se cambia la ruta de trabajo
r_names <- list.files() # volver a correr cada vez que se cambia la ruta de trabajo
n = length(r_names) # volver a correr cada vez que se cambia la ruta de trabajo

# NDSI (band2-band5)/(band2+band5)
m <- c(-999, 0.4, 0,  0.4, 999, 1) # se corre una sola vez
rclmat <- matrix(m, ncol=3, byrow=TRUE) # se corre una sola vez

######### Leer DEM

DEM = raster("/Volumes/vale_a/0000_Imagenes_sur/DEM/DEM_sur/DEM_233_83.tif") # cambiar ruta para cada set de imagenes de acuerdo a DEM correspondiente
crs = "+proj=utm +zone=19 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

######### Agua y cordillera mask

agua = raster("/Volumes/vale_a/0000_Imagenes_sur/233_83/Shapefile/agua_233_83.tif") # se corre una sola vez y depende de donde este este raster en el compu
#cordillera = raster("D:/NDSI/rasters/DEM/cordillera.tif") # se corre una sola vez y depende de donde este este raster en el compu

for (k in 23:n)    {
  beginCluster(type = "SOCK")
  
  start.time = Sys.time()
  
  temp_file = paste(r_files[k],'/',r_names[k], "_MTL.txt", sep = '')
  metafile = readMeta(temp_file)
  temp_raster = stackMeta(temp_file, quantity = "dn", category = "image", allResolutions = FALSE)
  # hazeDN = estimateHaze(temp_raster, hazeBands = 1:5, darkProp = 0.01, plot = FALSE)
  gc()
  if(metafile$SATELLITE == "LANDSAT8") { temp_raster = radCor(temp_raster, metaData = temp_file, method = "sdos", hazeBands = 1:7)} else {
    temp_raster = radCor(temp_raster, metaData = temp_file, method = "dos")
  }

  gc()
  
  # writeRaster(temp_raster, paste("D:/NDSI/rasters/233_79_copiapo/L7/", r_names[k], "_preRProj.tif", sep = ''), overwrite = TRUE)
  
  temp_raster = projectRaster(temp_raster[[1:6]], crs = crs, res = 30)
  gc()
  
  # writeRaster(temp_raster, paste("D:/NDSI/rasters/233_79_copiapo/L7/", r_names[k], "_postRProj.tif", sep = ''), overwrite = TRUE)
  
  e=extent(temp_raster)
  
  sl = resample(DEM, temp_raster[[1]], method = "bilinear")
  # dem.local = crop(dem.local, e)
  sl = terrain(sl, opt = c('slope', 'aspect'))
  
  gc()
  
  temp_raster = topCor(temp_raster, sl, temp_file, method = "minnaert")
  gc()
  
  if(metafile$SATELLITE == "LANDSAT8") {temp_raster[[7]] = (temp_raster[[3]] - temp_raster[[6]]) / (temp_raster[[3]] + temp_raster[[6]])} else {
    temp_raster[[7]] = (temp_raster[[2]] - temp_raster[[5]]) / (temp_raster[[2]] + temp_raster[[5]])} 
  
  temp_raster[[7]] = reclassify(temp_raster[[7]], rclmat)
  
  agua.local = projectRaster(agua, temp_raster)
  # cordillera.local = projectRaster(cordillera, temp_raster)
  
  temp_raster = temp_raster * agua.local
  # temp_raster = temp_raster * cordillera.local
  
  # temp_raster = areaSieve(temp_raster, thresh = 5000, directions = 8)
  
  
  writeRaster(temp_raster[[7]], paste("/Volumes/vale_a/0000_Imagenes_sur/233_83/NDSI_extra/", r_names[k], "_NDSI.tif", sep = ''),
              overwrite = TRUE) # cambiar ruta para cada set de imagenes nuevo, para que lo guarde en su carpeta correspondiente
  
  
  temp_raster = raster()
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(paste(k, "/" ,time.taken))
  
  endCluster()
  
  removeTmpFiles(h = 0)
  
}





