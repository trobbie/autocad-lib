using System;
using System.Linq;
using Autodesk.AutoCAD.DatabaseServices;
using Autodesk.AutoCAD.Geometry;
using System.Collections.Generic;

namespace AABase.Logic
{
    public class AaPolyline : AaCurve, IPolyline
    {
        public AaPolyline(Polyline pl) : base(pl) { }

        public AaPolyline(IEntity entity) : base((Polyline)entity.GetAcEntity()) { }

        public static AaPolyline Create(AaGeCurve firstCurve, ILogWriter logger)
        {
            Database db = Active.Database;
            Polyline acPolyline = null;
            
            db.UsingTransaction((Transaction tr) =>
            {
                BlockTable acBlkTbl = (BlockTable)tr.GetObject(db.BlockTableId, OpenMode.ForRead);
                BlockTableRecord modelspace = (BlockTableRecord)tr.GetObject(acBlkTbl[BlockTableRecord.ModelSpace], OpenMode.ForWrite);
                
                acPolyline = new Polyline();
                acPolyline.SetDatabaseDefaults();
                acPolyline.AddVertexAt(0, firstCurve.StartPoint.GetAcPoint2d(), firstCurve.Bulge, 0, 0);
                acPolyline.AddVertexAt(1, firstCurve.EndPoint.GetAcPoint2d(), 0, 0, 0);

                // Add the new object to Model space and the transaction
                modelspace.AppendEntity(acPolyline);
                tr.AddNewlyCreatedDBObject(acPolyline, true);

                logger.WriteLine(LogLevel.Debug, $"Created Polyline with first curve: {firstCurve.ToString()}");
                return true;
            });
            return new AaPolyline(acPolyline);
        }

        private Polyline GetPolyline() { return (Polyline)_dbobject; }

        public bool Closed { get { return GetPolyline().Closed; } }
        public int NumberOfVertices { get { return GetPolyline().NumberOfVertices; } }

        public AaPoint3d GetPoint3dAt(int vertexIndex)
        {
            return GetPolyline().GetPoint3dAt(vertexIndex).GetAaPoint();
        }
        
        /// <summary>
        /// Get Curve at specified start index. This could be a line.
        /// </summary>
        /// <returns>
        /// Returns the AaGeCurve of the segment at the start index. If index is out of bounds, returns null.
        /// </returns>
        /// <param name="vertexIndexStart"></param>
        /// <returns></returns>
        public AaGeCurve GetGeCurveAt(int vertexIndexStart)
        {
            if (vertexIndexStart >= NumberOfVertices) return null;
            int vertexIndexEnd = vertexIndexStart + 1;
            if (vertexIndexEnd >= NumberOfVertices)
            {
                if (Closed)
                    vertexIndexEnd = 0;
                else
                    return null;
            }
            if (IsArcSegment(vertexIndexStart))
            {
                CircularArc3d arc = GetPolyline().GetArcSegmentAt(vertexIndexStart);
                // find angle that was the reference point for angle=0 used for StartAngle/EndAngle
                // arc's referenceVector is unit vector that points to angle=0 on unit circle
                double angleRef = new Vector3d(1,0,0).GetAngleTo(arc.ReferenceVector, new Vector3d(0,0,1));  // returns [0,2PI]
                if (Utility.EqualsWithEpsilon(angleRef, 2*Math.PI, 0.00000001)) angleRef = 0;
                return new AaGeCurve(arc.Center.GetAaPoint(), arc.Radius, angleRef+arc.StartAngle, angleRef+arc.EndAngle, 
                    new AaPoint3d(arc.Normal.ToArray()));
            }
            else
            {
                return new AaGeCurve(GetPoint3dAt(vertexIndexStart), GetPoint3dAt(vertexIndexEnd));
            }
        }
        
        public bool IsLineSegment(int vertexIndex) { return GetPolyline().GetSegmentType(vertexIndex).Equals(SegmentType.Line); }
        
        public bool IsArcSegment(int vertexIndex) { return GetPolyline().GetSegmentType(vertexIndex).Equals(SegmentType.Arc); }

        public IEnumerable<AaPoint3d> GetPointsOnExtentsOfSegmentAt(int vertexIndex)
        {
            CircularArc3d arcDef = GetPolyline().GetArcSegmentAt(vertexIndex);
            List<AaPoint3d> ptList = new List<AaPoint3d>();
            
            // find the start and end angles such that the arc is always going counter-clockwise
            double startAngle = arcDef.ReferenceVector.AngleOnPlane(new Plane(arcDef.Center, arcDef.Normal)) + arcDef.StartAngle;
            startAngle = Utility.GetNormalizedAngle(startAngle);
            double endAngle = arcDef.ReferenceVector.AngleOnPlane(new Plane(arcDef.Center, arcDef.Normal)) + arcDef.EndAngle;
            endAngle = Utility.GetNormalizedAngle(endAngle);
           
            if (arcDef.Normal.Z < 0)  // if neg, then angles go clockwise, else counter-clockwise
            {
                double newEndAngle = Utility.GetNextCoterminalAngle(startAngle);
                startAngle = endAngle;
                endAngle = newEndAngle;
            }
            // TODO: why is ClosestPointToPlane returning two points?  Until figure out, just use one point.  The 2nd was giving bad results.
            // add topmost points
            if (Utility.IsAngleExclusivelyBetwStartAndEndAngles(Math.PI * 0.5, startAngle, endAngle))
                ptList.AddRange(arcDef.ClosestPointToPlane(new Plane(arcDef.BoundBlock.GetMaximumPoint(), new Vector3d(0, 1, 0))).GetPoints().Take(1));
            // add leftmost points
            if (Utility.IsAngleExclusivelyBetwStartAndEndAngles(Math.PI * 1.0, startAngle, endAngle))
                ptList.AddRange(arcDef.ClosestPointToPlane(new Plane(arcDef.BoundBlock.GetMinimumPoint(), new Vector3d(1, 0, 0))).GetPoints().Take(1));
            // add bottommost points
            if (Utility.IsAngleExclusivelyBetwStartAndEndAngles(Math.PI * 1.5, startAngle, endAngle))
                ptList.AddRange(arcDef.ClosestPointToPlane(new Plane(arcDef.BoundBlock.GetMinimumPoint(), new Vector3d(0,1,0))).GetPoints().Take(1));
            // add rightmost points
            if (Utility.IsAngleExclusivelyBetwStartAndEndAngles(Math.PI * 2.0, startAngle, endAngle))
                ptList.AddRange(arcDef.ClosestPointToPlane(new Plane(arcDef.BoundBlock.GetMaximumPoint(), new Vector3d(1,0,0))).GetPoints().Take(1));
            return ptList;
        }

        /// <summary>
        /// Add simple curve to this polyline if the curve can connect to an edge point of this polyline that is not on a list of non-join points
        /// </summary>
        public bool AddCurveIfAtEdge(AaGeCurve curveConsidered, bool reverseCurveDirection, IEnumerable<AaPoint3d> pointsNonJoin)
        {
            AaGeCurve curve = reverseCurveDirection ? curveConsidered : curveConsidered.AsReverseCurve();
                
            if (!pointsNonJoin.Contains(StartPoint) && StartPoint.Equals(curve.EndPoint))
            {
                AddCurveToStart(curve);
                return true;
            }
            if (!pointsNonJoin.Contains(EndPoint) && EndPoint.Equals(curve.StartPoint))
            {
                AddCurveToEnd(curve);
                return true;
            }
            return false;
        }


        public bool JoinPolylineIfAtEdge(AaPolyline plConsidered, bool reverseCurveDirection, IEnumerable<AaPoint3d> pointsNonJoin)
        {
            AaPoint3d startPointOther = reverseCurveDirection ? plConsidered.EndPoint : plConsidered.StartPoint;
            AaPoint3d endPointOther = reverseCurveDirection ? plConsidered.StartPoint : plConsidered.EndPoint;
                                    
            // don't "travel" through any point in pointsOnMultipleCurves
            // consider appending to a polyline's edge only if that edge point is not in pointsOnMultipleCurves
            if (!pointsNonJoin.Contains(StartPoint) && StartPoint.Equals(endPointOther))
            {
                Join(plConsidered);
                return true;
            }
            if (!pointsNonJoin.Contains(EndPoint) && EndPoint.Equals(startPointOther))
            {
                Join(plConsidered);
                return true;
            }
            return false;
        }
        
        public void AddCurveToStart(AaGeCurve curve)
        {
            Database db = Active.Database;
            db.UsingTransaction((Transaction tr) =>
            {
                // TODO: check that end vertex matches?
                GetPolyline().UpgradeOpen();
                GetPolyline().AddVertexAt(0, curve.StartPoint.GetAcPoint2d(), curve.Bulge, 0, 0);
                return true;
            });
        }
        public void AddCurveToEnd(AaGeCurve curve)
        {
            if (NumberOfVertices == 0) {
                AddCurveToStart(curve);
                return;
            }

            Database db = Active.Database;
            db.UsingTransaction((Transaction tr) =>
            {
                // TODO: check that start vertex matches?
                GetPolyline().UpgradeOpen();
                GetPolyline().SetBulgeAt(NumberOfVertices-1, curve.Bulge);
                GetPolyline().AddVertexAt(NumberOfVertices, curve.EndPoint.GetAcPoint2d(), 0, 0, 0);
                return true;
            });
        }
        public bool Join(AaPolyline pl)
        {
            GetPolyline().UpgradeOpen();
            try {
                GetPolyline().JoinEntity(pl.GetPolyline());
            }
            catch (Autodesk.AutoCAD.Runtime.Exception)
            {
                // TODO: add error printout
            }
            pl.Erase();
            return true;
        }

    }
}
