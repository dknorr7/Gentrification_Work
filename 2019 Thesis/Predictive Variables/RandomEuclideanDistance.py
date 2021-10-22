#-------------------------------------------------------------------------------
# Name:        module1
# Purpose: generate random sampling points within census blocks, join to original census blocks to keep fields
#
# Author:      dknorr
#
# Created:     12/10/2017
# Copyright:   (c) dknorr 2017
# Licence:     <your licence>
#-------------------------------------------------------------------------------
import arcpy, os
from os import path

arcpy.env.overwriteOutput = True
mxd = arcpy.mapping.MapDocument("CURRENT")
arcpy.env.workspace = "C:/School/Research/DistanceAnalysis"
workspace = "C:/School/Research/DistanceAnalysis"

outName = "RandomPts10"
outLoc= workspace
conFC = "Davidson_CensusTracts.shp"
numPoints = 10
randpts = arcpy.CreateRandomPoints_management(outLoc, outName, conFC, "", numPoints)
joinedrandpts = arcpy.SpatialJoin_analysis(randpts, conFC, match_option = "WITHIN", out_feature_class = "RandomJoinedPts.shp")

dists = arcpy.PointDistance_analysis (in_features = joinedrandpts, near_features = "Downtown_dest.shp" , out_table = path.join(workspace, "Analysis.gdb/DistanceToDowntown"))
finaltbl = arcpy.TableToTable_conversion(dists, path.join(workspace, "Analysis.gdb"), "DistanceToDowntownFinal")
final = "C:/School/Research/DistanceAnalysis/Analysis.gdb/DistanceToDowntownFinal"
arcpy.JoinField_management(final, "OBJECTID", joinedrandpts, "FID")



