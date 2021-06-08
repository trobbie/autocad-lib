using System.Linq;
using System.Collections.Generic;
using Autodesk.AutoCAD.Geometry;
using Autodesk.AutoCAD.DatabaseServices;

namespace AABase.Logic
{
    public static class IGeCurveExtensions
    {
        public static List<Curve> ToAcCurvesWithColor(this IEnumerable<IGeCurve> thisEnumerable, int colorIndex)
        {
            List<Curve> acCurves = new List<Curve>();
            Curve acCurve;
            foreach (IGeCurve geCurve in thisEnumerable)
            {
                if (geCurve.IsArc)
                {
                    AaGeArc arc = (AaGeArc)geCurve;
                    acCurve = new Arc(arc.Center.GetAcPoint3d(), new Vector3d(arc.PlaneNormal.AsArray()), arc.Radius, arc.StartAngle, arc.EndAngle);
                }
                else
                {
                    AaGeLine line = (AaGeLine)geCurve;
                    acCurve = new Line(line.StartPoint.GetAcPoint3d(), line.EndPoint.GetAcPoint3d());
                }
                acCurve.ColorIndex = 1; // red
                acCurves.Add(acCurve);
            }
            return acCurves;
        }

        /// <summary>
        /// Return list of "multi-path" points, those found on more than two curves (meaning more than one path can be traversed through the point) 
        /// </summary>
        public static List<AaPoint3d> FindMultiPathPoints(this IEnumerable<IGeCurve> simpleCurves)
        {
            return simpleCurves
                    .SelectMany((IGeCurve curve) => new List<AaPoint3d>() { curve.StartPoint, curve.EndPoint })
                    .GroupBy(x => x.GetHashCode())  // rely on HashCode for point "equality"
                    .Where(g => g.Count() > 2)
                    .SelectMany(g => g)
                    .Distinct()
                    .ToList();
        }
        
        /// <summary>
        /// Create polylines from these simple curves such that any ambiguous (multi-path) paths are not followed and left un-joined.
        /// </summary>
        /// <param name="simpleCurves">Extension object</param>
        /// <param name="includeJoinCurves">If true, join curves that touch the multi-path intersections.  If false, leave these curves un-joined.</param>
        /// <returns></returns>
        public static List<AaPolyline> CreateSinglePathPolylines(this IEnumerable<IGeCurve> simpleCurves, IEnumerable<AaPoint3d> pointsMultiPath, bool includeJoinCurves)
        {
            // join all curves without going through pointsMultiPath

            List<AaPolyline> joinedPolylines = new List<AaPolyline>();
            bool curveAdded;
            foreach (IGeCurve curve in simpleCurves)
            {
                curveAdded = false;
                AaPolyline plAddedTo = null;
                if (includeJoinCurves 
                   || !(pointsMultiPath.Contains(curve.StartPoint) || pointsMultiPath.Contains(curve.EndPoint)))
                { 
                    foreach (AaPolyline pl in joinedPolylines)
                    {
                        if (includeJoinCurves 
                        || !(pointsMultiPath.Contains(pl.StartPoint) || pointsMultiPath.Contains(pl.EndPoint)))
                        {
                            plAddedTo = pl;
                            curveAdded = pl.AddCurveIfAtEdge(curve, false, pointsMultiPath);
                            if (curveAdded) break;
                            curveAdded = pl.AddCurveIfAtEdge(curve, true, pointsMultiPath);
                            if (curveAdded) break;
                        }
                    }
                }
                if (curveAdded)
                {
                    // check to see if this polyline can now be joined with other existing polylines
                    foreach (AaPolyline pl in joinedPolylines)
                    {
                        if (pl == plAddedTo) continue;
                        if (pl.JoinPolylineIfAtEdge(plAddedTo, false, pointsMultiPath)) break;
                        if (pl.JoinPolylineIfAtEdge(plAddedTo, true, pointsMultiPath)) break;
                    }
                }
                else
                {
                    // if still not added, create a new polyline
                    AaPolyline newPL = AaPolyline.Create(curve);
                    joinedPolylines.Add(newPL);
                }
            }
            return joinedPolylines;
        }

    }
}
