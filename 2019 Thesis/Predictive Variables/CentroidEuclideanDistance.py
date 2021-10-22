#-------------------------------------------------------------------------------
# Name:        module1
# Purpose:
#
# Author:      dknorr
#
# Created:     10/06/2018
# Copyright:   (c) dknorr 2018
# Licence:     <your licence>
#-------------------------------------------------------------------------------

import arcpy, os
from arcpy import env
from os import path

arcpy.env.overwriteOutput = True
mxd = arcpy.mapping.MapDocument("CURRENT")
arcpy.env.workspace = "C:/School/Research/DistanceAnalysis"
workspace = "C:/School/Research/DistanceAnalysis"

outName = "CentroidDist.shp"
outLoc= workspace
conFC = "Davidson_CensusTracts.shp"

Centroids = arcpy.FeatureToPoint_management(conFC, outName,
                                "CENTROID")

dists = arcpy.PointDistance_analysis (in_features = Centroids, near_features = "Downtown_dest.shp" , out_table = path.join(workspace, "Analysis.gdb/CentroidDistToDowntown"))
finaltbl = arcpy.TableToTable_conversion(dists, path.join(workspace, "Analysis.gdb"), "CentroidDistToDowntownFinal")
final = "C:/School/Research/DistanceAnalysis/Analysis.gdb/CentroidDistToDowntownFinal"
arcpy.JoinField_management(final, "OBJECTID", Centroids, "FID")