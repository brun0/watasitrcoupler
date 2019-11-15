
# -------------------------------------------------------------------
# Given a filepath to the HRU shapefile (folder and filename)
# > reproject data (Lamber93->LamberIIe) to fit SAFRAN data
# > retrieve the shapefile extend and apply an "increase" factor
# -------------------------------------------------------------------
# FIX ME: easier management of file name and file path
# easier management of projections.
# J2K projection code should be a global variable
# SAFRAN projection code can still be provided as an argument
# TYPO: EXTEND ==> EXTENT
# -------------------------------------------------------------------
# Ivan Horner | 2018-06-22
# -------------------------------------------------------------------
GetSpatialExtend = function(hrufolder, hrufilename="hrus",
    projection="+init=epsg:2154", reprojection="+init=epsg:27572", fact=0.1, verbose=TRUE)
{

    # step 1: load and re-project hrus sp
    if (verbose) {message("Reading shapefile..."); flush.console()}
    hrus = readOGR(dsn=hrufolder, layer=hrufilename, p4s=projection, verbose=FALSE)
    if (!is.null(reprojection)) {
        if (verbose) {message("Re-projecting shapefile..."); flush.console()}
        hrus = spTransform(hrus, CRS(reprojection))
    }
    # step 2: get extent of hrus, increase extend using factor
    if (verbose) {message("Computing shapefile extend..."); flush.console()}
    extend = t(bbox(hrus))
    increasextend = apply(extend, 2, diff)*fact
    increasextend = matrix(c(-increasextend, increasextend), 2, 2, byrow=TRUE)
    extend = extend + increasextend
    # step 3: return extend
    return(extend)
}

# -------------------------------------------------------------------
# Given the object shp created by the j2kReadGIS() function:
# > reproject data (Lamber93->LamberIIe) to fit SAFRAN data
# > retrieve the shapefile extend and apply an "increase" factor
# -------------------------------------------------------------------
# FIX ME: easier management of file name and file path
# easier management of projections.
# J2K projection code should be a global variable
# SAFRAN projection code can still be provided as an argument
# -------------------------------------------------------------------
# Ivan Horner | 2018-06-22 | 2019-04-25
# -------------------------------------------------------------------
GetSpatialExtent = function(shp, reprojection="+init=epsg:27572", fact=0.1, verbose=TRUE)
{

    # step 1: load and re-project hrus sp
    # if (verbose) {message("Reading shapefile..."); flush.console()}
    hrus = shp$hru
    if (!is.null(reprojection)) {
        if (verbose) {message("Re-projecting shapefile..."); flush.console()}
        hrus = spTransform(hrus, CRS(reprojection))
    }
    # step 2: get extent of hrus, increase extent using factor
    if (verbose) {message("Computing shapefile extent..."); flush.console()}
    extent = t(bbox(hrus))
    increasextend = apply(extent, 2, diff)*fact
    increasextend = matrix(c(-increasextend, increasextend), 2, 2, byrow=TRUE)
    extent = extent + increasextend
    # step 3: return extent
    return(extent)
}

# -------------------------------------------------------------------
# Given the filepath to one of the NCDF files and the extend box of 
# the given model area (retrieved with j2kGetSpatialExtend), it
# returns a data.frame with SAFRAN cell IDs and their corresponding
# X and Y coordinates in Lambert93
# -------------------------------------------------------------------
# FIX ME: easier management of file name and file path
# TYPO: EXTEND ==> EXTENT
# -------------------------------------------------------------------
# Ivan Horner | 2018-06-22
# Adapted by I. Braud to only get SAFRAN data over a given catchment independently of J2K
# -------------------------------------------------------------------
GetCellsFromExtent = function(ncdffp, extent, verbose=TRUE)
{
    if (verbose) { message("Opening NCDF file ..."); flush.console() }
    # open NCDF file
    ncdf = nc_open(ncdffp)
	# We get the ids of the cells that are located in France
	idcellsFR <- ncvar_get(ncdf,varid="Cell number")[ncvar_get(ncdf,varid="in.France")==1]
    # get cell ids that are within the provided extent
    if (verbose) { message("Getting cells ids from spatial extent ..."); flush.console() }
    XY = data.frame("x"=ncvar_get(ncdf, varid="LambXg"), "y"=ncvar_get(ncdf, varid="LambYg"))
    ids = which(XY[, 1]>=extent[1, 1] & XY[, 1]<=extent[2, 1] & XY[, 2]>=extent[1, 2] & XY[, 2]<=extent[2, 2])
    cells = ncdf$dim[["Cell number"]]$vals[ids]
	# To be identical to the cell numbers in the .shp file, we replace the cell number by the index of idcellsFR
	cells <- which(idcellsFR %in% cells)
    if (length(cells)==0) {
        stop("No cells are within the provided spatial extent. Check projection.")
    }
    cells = data.frame(ID=cells, X=XY[ids, 1], Y=XY[ids, 2]) # Cells coordinates remain in extended LambertII
    return(cells)
}

# -------------------------------------------------------------------
# Function to retrieve data from a NCDF file. NCDF format is as 
# J.P. Vidal created the NCDF file in June 2018.
# -------------------------------------------------------------------
# Ivan Horner | 2018-06-22
# Adapted by I. Braud to only get SAFRAN data over a given catchment independently of J2K
# -------------------------------------------------------------------
GetSafranNcdfData = function(fp, from, to, cells, verbose=TRUE)
{
    if (verbose) { message("Opening NCDF file ..."); flush.console() }
    # open NCDF file
    ncdf = nc_open(fp)

 #   # if extent is provided
 #   if (!missing(extent)) {
 #       if (verbose) { message("Getting cells ids from spatial extent ..."); flush.console() }
 #       XY = data.frame("x"=ncvar_get(ncdf, varid="LambXg"), "y"=ncvar_get(ncdf, varid="LambYg"))
 #       ids = which(XY[, 1]>=extent[1, 1] & XY[, 1]<=extent[2, 1] & XY[, 2]>=extent[1, 2] & XY[, 2]<=extent[2, 2])
 #       cells = ncdf$dim[["Cell number"]]$vals[ids]
 #       if (length(cells)==0) stop("No cells are within the provided spatial extent. Check projection.")
 #   }
#	print(cells)
    
    # get time reference
    ref_time = ncdf$dim[["Time"]]$units
    ref_time = unlist(strsplit(ref_time, " "))
    ref_time = ref_time[length(ref_time)]
    ref_time = as.POSIXct(strptime(ref_time, format=c("%Y-%m-%d"), tz="UTC"))

    # compute time vector
    time_dim_length = ncdf$dim[[3]]$len
    time_vector = seq(from=ref_time, length.out=time_dim_length+1, by="days")
    time_vector = time_vector[-1]

    # Get 'start' and 'count' from specified wanted time range
    if (missing(from)) from = time_start
    i_start = which(time_vector == from)
    if (length(i_start) == 0) {
        warning("'from' is invalid. First date found taken instead.")
        i_start = 1
    }
    if (missing(to)) to = time_end
    i_end = which(time_vector == to)
    if (length(i_end) == 0) {
        warning("'to' is invalid. Last date found taken instead.")
        i_end = ncdf$dim$Time$len
    }
    count = i_end - i_start + 1
    time_vector = time_vector[i_start:i_end]

    # check provided cell ids
    if (missing(cells)) cells = 1:ncdf$dim$cell$len
    cell_ids = ncdf$dim[["Cell number"]]$vals
    matching_cells = intersect(cells, cell_ids)
    i_nomatch = which(is.na(match(cells, matching_cells)))
    if (length(i_nomatch)>0) {
        warning(paste0("Some wanted cells were not found. Please check provided cell ids.\nThe unvalid id(s) is/are: ",
            paste(cells[i_nomatch], collapse=", ")))
    }

    # retrieve data
    myvar = matrix(NA, count, length(matching_cells))
    if (verbose) {
        # cat("\n")
        for (k in 1:length(matching_cells)) {
            cat(paste0("\r[", round(k/length(matching_cells)*100, 0), " %] Retrieving cell ", matching_cells[k], "...     ")); flush.console()
            myvar[, k] = ncvar_get(ncdf, start=c(matching_cells[k], i_start), count=c(1, count))
        }
        cat("\n")
    } else { # if verbose is False, it will increase retrieval speed in some cases (if number of cell is large and time range short)
        for (k in 1:length(matching_cells)) {
            myvar[, k] = ncvar_get(ncdf, start=c(matching_cells[k], i_start), count=c(1, count))
        }
    }
    colnames(myvar) = matching_cells

    # close ncdf and return time and values
    nc_close(ncdf)

    return(list("time"=time_vector, "data"=myvar))
}

# -------------------------------------------------------------------
# FIX ME: add 'verbose' argument
# -------------------------------------------------------------------
# Wrapper function to create the J2K forcing files from the NCDF files
# It needs the directoru to the SAFRAN data, the SAFRAN file names, 
# the directory of the J2K project, the time range, the required 
# SAFRAN cells, the output forcing filenames and a header comment as 
# arguments.
# extent is a (2,2) matrix with xmin,xmax in the first column and ymin, ymax in the second column.
# -------------------------------------------------------------------
# Ivan Horner | 2018-06-22
# Adapted by I. Braud to only get SAFRAN data over a given catchment independently of J2K
# -------------------------------------------------------------------
FromSafranToData = function(safDataDir, time_range, extent,
    ncfilename=c("safran_new_Rainf.nc", "safran_new_Snowf.nc", "safran_new_ET0.nc", "safran_new_Tair.nc"))
{
    # test if packages were loaded (XML and ncdf)
    if (!require(ncdf4)) stop("The 'ncdf4' package is required. Please run 'install.packages(\"ncdf4\")' to install it.")  

    # open NCDF file
	# We assume the information is the same for all netcdf files
	ncdf = nc_open(paste0(safDataDir, ncfilename[1])) # To get the info about grid points 
		
	# First get information of the cells that are within the extent of the study area
    XY = data.frame("x"=ncvar_get(ncdf, varid="LambXg"), "y"=ncvar_get(ncdf, varid="LambYg"))
    ids = which(XY[, 1]>=extent[1, 1] & XY[, 1]<=extent[2, 1] & XY[, 2]>=extent[1, 2] & XY[, 2]<=extent[2, 2])
    cells = ncdf$dim[["Cell number"]]$vals[ids]
    if (length(cells)==0) stop("No cells are within the provided spatial extent. Check projection.")
	print(cells)
	
    # Precipitation: ----------------------------------------------------
    message("Reading Rain and Snow NCDF files..."); flush.console()
    R = GetSafranNcdfData(fp=paste0(safDataDir, ncfilename[1]), from=time_range[1], to=time_range[2], cells=cells)
    S = GetSafranNcdfData(fp=paste0(safDataDir, ncfilename[2]), from=time_range[1], to=time_range[2], cells=cells)
    P = list(time=R$time, data=R$data+S$data)

    # Evapotranspiration of reference: ----------------------------------
    message("Reading Reference evapotranspiration NCDF files..."); flush.console()
    E = GetSafranNcdfData(fp=paste0(safDataDir, ncfilename[3]), from=time_range[1], to=time_range[2], cells=cells)

    # Air temperature: --------------------------------------------------
    message("Reading Air temperature NCDF files..."); flush.console()
    T = GetSafranNcdfData(fp=paste0(safDataDir, ncfilename[4]), from=time_range[1], to=time_range[2], cells=cells)
    T$data = T$data - 273.15

	# Get information of the cells that are within the extent of the study area but in the French grid, not the whole grid
	cell_infos <- GetCellsFromExtent(paste0(safDataDir, ncfilename[1]),extent)
    
	data <- list("cell_infos"=cell_infos,"P"=P,"E"=E,"T"=T)
	data
}


