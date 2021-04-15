﻿using Autodesk.AutoCAD.Runtime;
using Autodesk.AutoCAD.ApplicationServices.Core;
using Autodesk.AutoCAD.DatabaseServices;
using Autodesk.AutoCAD.Geometry;
using System.Linq;
using System.Collections.Generic;
using System;
using AABase.Logic;

namespace AABase
{

    public static class AutocadBaseCommands
    {
        public static void PerformAutocadCommand(string commandName, string commandDesc, Func<bool> action)
        {
            Active.WriteDebugMessage(2, "START "+commandName+": "+commandDesc);

            // enclose everything in one transaction, so that one "Undo" will undo everything
            // other transactions will be nested
            Active.Database.UsingTransaction((Transaction trParent) =>
            {
                return action();
            });

            Active.WriteDebugMessage(2, "END "+commandName);
        }
        
        [CommandMethod("AAC-COUNT-OBJECTS", CommandFlags.UsePickSet)]
        public static void TotalCountOfSelection()
        {
            PerformAutocadCommand("AAC-COUNT-OBJECTS", "", () =>
            {
                int totalObjects = 0;
                IEnumerable<IEntity> entities = Active.Document.GetSelectedSetObjects();
                totalObjects = entities.Count();
                Application.ShowAlertDialog("Number of objects in Pickfirst selection: " +
                                                totalObjects.ToString());

                return true;
            });
        }

        // TODO: replace autolisp version with this one; rename when releasing the DLL
        [CommandMethod("AAC-TOTAL-LENGTH-BETA", CommandFlags.UsePickSet)]
        public static void TotalLengthOfSelection()
        {
            PerformAutocadCommand("AAC-TOTAL-LENGTH-BETA", "", () =>
            {
                double totalLength = 0; 
                IEnumerable<IEntity> entities = Active.Document.GetSelectedSetObjects();
                foreach (IEntity ent in entities)
                  Active.WriteDebugMessage(2,$"isCurve? {(ent.GetAcEntity() is Curve)}");
                string unsupportedTypes = entities
                    .Where(ent => !(ent.GetAcEntity() is Curve))
                    .Select(ent => ent.GetDxfName())
                    .Distinct()
                    .Aggregate("", (i,j) => i + (i.Equals("")?"":", ") + j);
                if (unsupportedTypes.Length>0)
                {
                   Application.ShowAlertDialog($"Could not calculate length for certain selected objects: {unsupportedTypes}");
                   return false;
                }
                totalLength = entities
                    .Where(obj => (obj.GetAcEntity() is Curve))
                    .Select(obj => obj.GetLength())
                    .Sum();
                Application.ShowAlertDialog($"Total length of selected objects: {totalLength.ToString("N4")}");
                return true;
            });
        }

        [CommandMethod("AAX-POLYLINE-ORIGIN-BETA", CommandFlags.UsePickSet)]
        public static void PolylineOriginCommand()
        {
            PerformAutocadCommand("AAC-POLYLINE-ORIGIN-BETA", "Reassign vertex 1 of selected polyline", () =>
            {
                IPolyline pl = Active.Document.GetSelectedPolyline();
                if (pl == null)
                {
                    Application.ShowAlertDialog($"No polylines selected.  Cancelling...");
                    return false;
                }
                AaPoint3d newOrigin = Active.Document.GetSelectedPoint();
                if (newOrigin == null)
                {
                    Active.WriteMessage($"No point selected.  Cancelling...");
                    return false;
                }
                Polyline acPL = ((Polyline)pl.GetAcEntity());
                double swid;
                double ewid;
                double bulge;
                Point2d pt2d;
                int startIndex = (int)acPL.GetParameterAtPoint(newOrigin.GetAutocadPoint3d());

                Active.Database.UsingTransaction((Transaction tr) =>
                {
                    BlockTable acBlkTbl = (BlockTable)tr.GetObject(Active.Database.BlockTableId, OpenMode.ForRead);
                    BlockTableRecord acBlkTblRec = (BlockTableRecord)tr.GetObject(acBlkTbl[BlockTableRecord.ModelSpace], OpenMode.ForWrite);

                    Polyline acNewPoly = new Polyline();
                    acNewPoly.SetDatabaseDefaults();
                    int indexOldPoly = startIndex;
                    int indexNewPoly;

                    for (indexNewPoly = 0; indexNewPoly < pl.NumberOfVertices; indexNewPoly++)
                    {
                        swid = acPL.GetStartWidthAt(indexOldPoly);
                        ewid = acPL.GetEndWidthAt(indexOldPoly);
                        pt2d = acPL.GetPoint2dAt(indexOldPoly);
                        bulge = acPL.GetBulgeAt(indexOldPoly);
                        acNewPoly.AddVertexAt(indexNewPoly, pt2d, bulge, swid, ewid);

                        acNewPoly.ColorIndex = acPL.ColorIndex;
                        indexOldPoly++;
                        if (indexOldPoly >= acPL.NumberOfVertices) indexOldPoly = 0;
                    }
                    acNewPoly.Closed = acPL.Closed;
                    acNewPoly.Layer = acPL.Layer;
                    acNewPoly.Color = acPL.Color;

                    acBlkTblRec.AppendEntity(acNewPoly);
                    tr.AddNewlyCreatedDBObject(acNewPoly, true);
                    tr.EraseObject(acPL.ObjectId);

                    return true;
                });

                Active.WriteDebugMessage(1, $"Polyline origin reassigned to: {newOrigin.ToString()}");
                return true;
            });
        }
    }
}