#' Download Map Tiles to local directory
#'
#' \code{GetMapTiles} is a smart wrapper that queries the Google Maps,
#' OpenStreetMap, Stamen Maps or Naver Map servers for a map.  This download map
#'  tiles from specified map tile servers such as openstreetmap or Google
#'  Query the server for map tiles, defined uniquely by their X and Y ID and zoom.
#'  For offline usage, these map tiles are stored in a local directory
#'  THIS CODE WAS MODIFIED FROM THE FUNCTION IN THE RgoogleMaps Package
#'
#' @param bbox Extent over which to pull tiles
#' @param zoom map zoom, an integer from 3 (continent) to 21
#'   (building), default value 10 (city).  openstreetmaps limits a
#'   zoom of 18, and the limit on stamen maps depends on the
#'   maptype.  "auto" automatically determines the zoom for bounding
#'   box specifications, and is defaulted to 10 with center/zoom
#'   specifications.  maps of the whole world currently not
#'   supported.
#' @param urlBase path of tile server to query
#' @param CheckExistingFiles will check if current tiles are already downloaded and skip if are
#' @param TotalSleep ##<< overall time (in seconds) that one is willing to add in
#'   between downloads. This is intended to lower the risk of a server denial. 
#'   If NULL no call to \link{Sys.sleep} is executed
#' @param tileDir Where are local tiles stored.  Only relevant if doOffline == TRUE
#' @param tileExt file extension of files from tile server
#' @param verbose level of verbosity 
#' @return a ggmap object (a classed raster object with a bounding
#'   box attribute)
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \code{\link{ggmap}}, \code{\link{GetMap}} in package
#'   RgoogleMaps
#' @export
#' @examples GetMapTiles(zoom=9
#'                       ,bbox = sbbox
#'                       ,verbose=1
#'                       ,urlBase = "http://tile.stamen.com/terrain/"
#'                       ,tileDir= "./mapTiles/stamen/terrain/")

GetMapTiles <- function(bbox = c(left = -95.80204
                                 ,bottom = 29.38048
                                 ,right = -94.92313
                                 ,top = 30.14344)
                        ,zoom =10 
                        ,urlBase =  "http://tile.stamen.com/terrain/"
                        ,CheckExistingFiles = TRUE 
                        ,TotalSleep = NULL 
                        ,tileExt = ".png" 
                        ,tileDir= "~/mapTiles/stamen/" 
                        ,verbose=0){
  
  
  # tile2long = function(x,z){ return (x/(2^z)*360-180); }
  # tile2lat= function(y,z) { n=pi-2*pi*y/(2^z);
  #                           return (180/pi*atan(0.5*(exp(n)-exp(-n)))); }
  
  lonR <- c(bbox['left'],bbox['right'])
  latR <- c(bbox['bottom'],bbox['top'])
  
  nTiles = c(0,0)
  
  if (!missing(lonR) & !missing(latR)) {
    XYmin = RgoogleMaps::LatLon2XY(lat=latR[1], lon=lonR[1],zoom=zoom)
    XYmax = RgoogleMaps::LatLon2XY(lat=latR[2], lon=lonR[2],zoom=zoom)
    nTiles[1] = abs(XYmax$Tile[1,1]-XYmin$Tile[1,1])+1
    nTiles[2] = abs(XYmax$Tile[1,2]-XYmin$Tile[1,2])+1

    center = c(lat=mean(latR),lon=mean(lonR))
    
    if (verbose){
      cat("nTiles=",nTiles,", center=", round(center,3), "\n")
    }
  } 
  
  XY = RgoogleMaps::LatLon2XY(lat=center["lat"], lon=center["lon"],zoom=zoom)
  tileXY = XY$Tile + as.numeric(XY$Coords > 256)

  if (nTiles[1] %% 2 == 0) {#even
    X = (tileXY[1,1]-nTiles[1]/2):(tileXY[1,1]+nTiles[1]/2-1);
  } else {
    X = (tileXY[1,1]-(nTiles[1]-1)/2):(tileXY[1,1]+(nTiles[1]-1)/2);
  }
  if (nTiles[2] %% 2 == 0) {#even
    Y = (tileXY[1,2]-nTiles[2]/2):(tileXY[1,2]+nTiles[2]/2-1);
  } else {
    Y = (tileXY[1,2]-(nTiles[2]-1)/2):(tileXY[1,2]+(nTiles[2]-1)/2);
  }
  
  if (!dir.exists(tileDir)) {
    if (verbose) cat("trying to create dir",tileDir, "\n")
    dir.create(tileDir, recursive = TRUE)
  }
  if (CheckExistingFiles) ExistingFiles=list.files(path=tileDir)
  
  NumTiles = length(X)*length(Y)
  if (verbose) cat (NumTiles, "tiles to download \n")
    
  DOWNLOAD=TRUE
  k=1
  tiles=list()
  y = 0
  
  for (x_count in 1:length(X)){
    
    cat(paste0('Currently on tile ', ((x_count-1) * length(Y))+1,' of ',NumTiles,'\n'))
    
    x <- X[x_count]
    
    for (y_count in 1:length(Y)){
      
      y <- Y[y_count]
      
      if (grepl("openstreetmap",urlBase) | grepl("stamen",urlBase)){
        url <- paste0(urlBase, zoom, "/",x , "/", y, ".png")
      } else if (grepl("google",urlBase)){
        url <- paste0(urlBase, "&x=", x, "&y=", y, "&z=", zoom)
      } 

      f=paste(zoom, x, y, sep="_")
		  if (CheckExistingFiles) 
        if (paste0(f,tileExt) %in% ExistingFiles) {
          if (verbose) cat("     NOT downloading existing file ",f, tileExt, "\n",sep="" )
          DOWNLOAD=FALSE
        } else {
          if (verbose) cat("     downloading file ",f, tileExt, "\n",sep="" )
          DOWNLOAD=TRUE
        }
      destfile = file.path(tileDir, f)
      mapFile=paste0(destfile,tileExt)
      
      if (DOWNLOAD){
  		  if (!is.null(TotalSleep)){
  		    Sys.sleep(round(runif(1,max=2*TotalSleep/NumTiles),1))
  		  }
  		  try(download.file(url, mapFile, mode="wb", quiet = TRUE));
      }
		  k=k+1
    }
  }
  mt = list(X=X,Y=Y,zoom=zoom,tileDir=tileDir,tileExt=tileExt,tiles=tiles)
  class(mt) =  "mapTiles"
  invisible(mt)	
}