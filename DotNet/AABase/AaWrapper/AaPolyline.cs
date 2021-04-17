﻿using System;
using Autodesk.AutoCAD.DatabaseServices;
using Autodesk.AutoCAD.Geometry;
using System.Collections.Generic;

namespace AABase.Logic
{
    public class AaPolyline : AaCurve, IPolyline
    {
        public AaPolyline(Polyline pl) : base(pl) { }

        public AaPolyline(IEntity entity) : base((Polyline)entity.GetAcEntity()) { }

        private Polyline GetPolyline() { return (Polyline)_dbobject; }

        public bool Closed { get { return GetPolyline().Closed; } }
        public int NumberOfVertices { get { return GetPolyline().NumberOfVertices; } }

        public AaPoint3d GetPoint3dAt(int vertexIndex)
        {
            return GetPolyline().GetPoint3dAt(vertexIndex).GetPoint();
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
            // add topmost points
            if (Utility.IsAngleExclusivelyBetwStartAndEndAngles(Math.PI * 0.5, startAngle, endAngle))
                ptList.AddRange(arcDef.ClosestPointToPlane(new Plane(arcDef.BoundBlock.GetMaximumPoint(), new Vector3d(0, 1, 0))).GetPoints());
            // add leftmost points
            if (Utility.IsAngleExclusivelyBetwStartAndEndAngles(Math.PI * 1.0, startAngle, endAngle))
                ptList.AddRange(arcDef.ClosestPointToPlane(new Plane(arcDef.BoundBlock.GetMinimumPoint(), new Vector3d(1, 0, 0))).GetPoints());
            // add bottommost points
            if (Utility.IsAngleExclusivelyBetwStartAndEndAngles(Math.PI * 1.5, startAngle, endAngle))
                ptList.AddRange(arcDef.ClosestPointToPlane(new Plane(arcDef.BoundBlock.GetMinimumPoint(), new Vector3d(0,1,0))).GetPoints());
            // add rightmost points
            if (Utility.IsAngleExclusivelyBetwStartAndEndAngles(Math.PI * 2.0, startAngle, endAngle))
                ptList.AddRange(arcDef.ClosestPointToPlane(new Plane(arcDef.BoundBlock.GetMaximumPoint(), new Vector3d(1,0,0))).GetPoints());
            return ptList;
        }

    }

}