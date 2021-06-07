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
        /// Create polylines from these simple curves such that any ambiguous (multi-path) paths are not followed and left un-joined.
        /// </summary>
 
        
        /// <summary>
        /// Create polylines from these simple curves such that any ambiguous (multi-path) paths are not followed and left un-joined.
        /// </summary>
        /// <param name="simpleCurves">Extension object</param>
        /// <param name="includeJoinCurves">If true, join curves that touch the multi-path intersections.  If false, leave these curves un-joined.</param>
        /// <returns></returns>
        public static List<AaPolyline> CreateSinglePathPolylines(this IEnumerable<IGeCurve> simpleCurves, bool includeJoinCurves)
        {
            // points found on more than two curves are on the edges of multi-paths 
            IEnumerable<AaPoint3d> pointsOnMultipleCurves = simpleCurves
                    .SelectMany((IGeCurve curve) => new List<AaPoint3d>() { curve.StartPoint, curve.EndPoint })
                    .GroupBy(x => x.GetHashCode())  // rely on HashCode for point "equality"
                    .Where(g => g.Count() > 2)
                    .SelectMany(g => g)  // .Select (g => g.Select(v => v.Value)) ???
                    .Distinct()
                    .ToList();

            // join all curves without going through points above

            List<AaPolyline> joinedPolylines = new List<AaPolyline>();
            bool curveAdded;
            foreach (IGeCurve curve in simpleCurves)
            {
                curveAdded = false;
                AaPolyline plAddedTo = null;
                if (includeJoinCurves 
                   || !(pointsOnMultipleCurves.Contains(curve.StartPoint) || pointsOnMultipleCurves.Contains(curve.EndPoint)))
                { 
                    foreach (AaPolyline pl in joinedPolylines)
                    {
                        if (includeJoinCurves 
                        || !(pointsOnMultipleCurves.Contains(pl.StartPoint) || pointsOnMultipleCurves.Contains(pl.EndPoint)))
                        {
                            plAddedTo = pl;
                            curveAdded = pl.AddCurveIfAtEdge(curve, false, pointsOnMultipleCurves);
                            if (curveAdded) break;
                            curveAdded = pl.AddCurveIfAtEdge(curve, true, pointsOnMultipleCurves);
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
                        if (pl.JoinPolylineIfAtEdge(plAddedTo, false, pointsOnMultipleCurves)) break;
                        if (pl.JoinPolylineIfAtEdge(plAddedTo, true, pointsOnMultipleCurves)) break;
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
