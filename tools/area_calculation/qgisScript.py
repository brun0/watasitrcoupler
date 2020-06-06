import time
import os, subprocess
import random
from osgeo import gdal, ogr, osr
from osgeo.gdalnumeric import *
from osgeo.gdalconst import *
import processing
import numpy as np
from scipy import signal

def fillsinkSagaProcessing(dem_path, ldd_out_path, demFilledPath):
    processing.run('saga:fillsinkswangliu', {
        'ELEV': dem_path,
        'FDIR' : ldd_out_path,
        'FILLED' : demFilledPath,
        'MINSLOPE' : 0.01,
        'WSHED' : 'TEMPORARY_OUTPUT'
    })

def upslopeAreaSagaProcessing(filledDemPath, studyExtendedPath, sagaCatchment):
    processing.run('saga:upslopearea', {
        'AREA' : sagaCatchment,
        'CONVERGE' : 1.1,
        'ELEVATION' : filledDemPath,
        'METHOD' : 0,
        'SINKROUTE' : None,
        'TARGET' : studyExtendedPath
        #'TARGET_PT_X' : 0,
        #'TARGET_PT_Y' : 0
    })

def world_to_pixel(geo_matrix, x, y):
    """
    Uses a gdal geomatrix (gdal.GetGeoTransform()) to calculate
    the pixel location of a geospatial coordinate
    """
    ul_x= geo_matrix[0]
    ul_y = geo_matrix[3]
    x_dist = geo_matrix[1]
    y_dist = geo_matrix[5]
    pixel = int((x - ul_x) / x_dist)
    line = -int((ul_y - y) / y_dist)
    return pixel, line
    
def makeArea(i, xsig, ysig, nbpix):
    dem = gdal.Open('/home/julien/sources/area_calculation/MNT25m_93_buech.tif')
    band1 = dem.GetRasterBand(1)
    data1 = BandReadAsArray(band1)
    demnodata = band1.GetNoDataValue()
    #ulx, xres, xskew, uly, yskew, yres  = dem.GetGeoTransform()

    # Extract target reference from the tiff file
    target = osr.SpatialReference(wkt=dem.GetProjection())

    source = osr.SpatialReference()
    source.ImportFromEPSG(2154)

    transform = osr.CoordinateTransformation(source, target)

    point = ogr.Geometry(ogr.wkbPoint)
    point.AddPoint(xsig, ysig)
    #point.AddPoint(916121, 6390656)
    point.Transform(transform)

    x, y = world_to_pixel(dem.GetGeoTransform(), point.GetX(), point.GetY())
    #print('PLOP %s'%i)
    #print('%s %s'%(xsig,ysig))
    #print('%s %s'%(x,y))

    studyData = np.where(data1==0, 0, 0)
    studyData[y, x] = 1
    #studyData[0, 0] = 1
    #print(studyData[y, x+1])
    #print(dem.RasterXSize, dem.RasterYSize)
    if nbpix == 1:
        studyData3 = studyData
    else:
        studyData2 = signal.convolve2d(studyData, np.ones((nbpix,nbpix)), mode='same')
        studyData3 = np.where(studyData2 > 0, 1, 0)

    driver = gdal.GetDriverByName('GTiff')
    studyOut = driver.Create('/home/julien/sources/area_calculation/study%s.tif'%i, dem.RasterXSize, dem.RasterYSize, 1, band1.DataType)
    CopyDatasetInfo(dem, studyOut)
    bandOut = studyOut.GetRasterBand(1)
    bandOut.SetNoDataValue(0)
    BandWriteArray(bandOut, studyData3)

    del bandOut
    del studyOut
    studyOut = None
    bandOut = None

    del band1
    del dem
    band1 = None
    dem = None
    
    del studyData
    
    #time.sleep(3)

    upslopeAreaSagaProcessing(
        '/home/julien/sources/area_calculation/filled.tif',
        '/home/julien/sources/area_calculation/study%s.tif'%i,
        '/home/julien/sources/area_calculation/upslope%s.sdat'%i
    )
    # get area
    upslope = gdal.Open('/home/julien/sources/area_calculation/upslope%s.sdat'%i)
    band1 = upslope.GetRasterBand(1)
    data1 = BandReadAsArray(band1)
    upslopenodata = band1.GetNoDataValue()
    ulx, xres, xskew, uly, yskew, yres  = upslope.GetGeoTransform()
    
    #print('xres %s yres %s'%(xres, yres))
    nb = np.count_nonzero(data1)
    pixelArea = abs(xres) * abs(yres)
    area = int(nb * pixelArea / 1000000)
    
    del band1
    del upslope
    band1 = None
    upslope = None
    
    return area
    
def computeAreaShape(nbpix):

    os.system('rm -f /home/julien/sources/area_calculation/upslope*')
    os.system('rm -f /home/julien/sources/area_calculation/study*')

    # open shape
    driver = ogr.GetDriverByName('ESRI Shapefile')
    dataSource = driver.Open('/home/julien/sources/area_calculation/shpForHRUdelin_fus.shp', 1)
    layer = dataSource.GetLayer()
    featureCount = layer.GetFeatureCount()
    print(featureCount)
    areas = {}
    c=0
    nbLayers = len(layer)
    for feature in layer:
        xsig = feature.GetField('X')
        ysig = feature.GetField('Y')
        id = feature.GetField('ID')
        areas[id] = makeArea(id, xsig, ysig, nbpix)
        c+=1
        print('computing layer %s/%s ID:%s'%(c, nbLayers, id))
    #    if c==4:
    #        break
    #    #feature.SetField('S_BH',1)
    layer.ResetReading()

    print(areas)

    # compute areas
    #makeArea(1, 916121, 6390656)
    #makeArea(2, 918824, 6377727)
    #makeArea(3, 924131, 6356403)
    #makeArea(4, 935500, 6348900)
    #makeArea(5, 930543, 6388509)

    # WRITE output shape with area values

    outShapefile = "/home/julien/sources/area_calculation/out%s.shp"%nbpix
    outDriver = ogr.GetDriverByName("ESRI Shapefile")

    if os.path.exists(outShapefile):
        outDriver.DeleteDataSource(outShapefile)

    outDataSource = outDriver.CreateDataSource(outShapefile)
    outLayer = outDataSource.CreateLayer("plop", geom_type=ogr.wkbPoint)

    inLayerDefn = layer.GetLayerDefn()
    for i in range(0, inLayerDefn.GetFieldCount()):
        fieldDefn = inLayerDefn.GetFieldDefn(i)
        fieldName = fieldDefn.GetName()
        outLayer.CreateField(fieldDefn)

    # Get the output Layer's Feature Definition
    outLayerDefn = outLayer.GetLayerDefn()

    # Add features to the ouput Layer
    for feature in layer:
        # Create output Feature
        outFeature = ogr.Feature(outLayerDefn)

        # Add field values from input Layer
        for i in range(0, outLayerDefn.GetFieldCount()):
            fieldDefn = outLayerDefn.GetFieldDefn(i)
            fieldName = fieldDefn.GetName()
            outFeature.SetField(outLayerDefn.GetFieldDefn(i).GetNameRef(),
                feature.GetField(i))
                
        idField = feature.GetField('ID')
        if idField in areas:
            #print('%s %s'%(idField, areas[idField]))
            outFeature.SetField('S_BH', areas[idField])

        # Set geometry as centroid
        geom = feature.GetGeometryRef()
        outFeature.SetGeometry(geom.Clone())
        # Add new feature to output Layer
        outLayer.CreateFeature(outFeature)
        outFeature = None



    # Save and close DataSource
    dataSource = None
    outDataSource = None

#computeAreaShape(1)
#computeAreaShape(2)
#computeAreaShape(3)
#computeAreaShape(4)
#computeAreaShape(6)
#computeAreaShape(7)
#computeAreaShape(8)
computeAreaShape(10)
computeAreaShape(15)
computeAreaShape(20)
