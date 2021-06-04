using System.Linq;
using System.Collections.Generic;
using Autodesk.AutoCAD.Geometry;
using Autodesk.AutoCAD.DatabaseServices;

namespace AABase.Logic
{
    public static class AaGeCurveExtensions
    {
        public static List<Curve> ToAcCurvesWithColor(this IEnumerable<AaGeCurve> thisEnumerable, int colorIndex)
        {
            List<Curve> acCurves = new List<Curve>();
            Curve acCurve;
            foreach (AaGeCurve geCurve in thisEnumerable)
            {
                if (geCurve.IsArc)
                    acCurve = new Arc(geCurve.Center.GetAcPoint3d(), new Vector3d(geCurve.PlaneNormal.AsArray()), geCurve.Radius, geCurve.StartAngle, geCurve.EndAngle);
                else
                    acCurve = new Line(geCurve.StartPoint.GetAcPoint3d(), geCurve.EndPoint.GetAcPoint3d());
                acCurve.ColorIndex = 1; // red
                acCurves.Add(acCurve);
            }
            return acCurves;
        }

        /// <summary>
        /// Create polylines from these simple curves such that any ambiguous (multi-path) paths are not followed and left un-joined.
        /// </summary>
        public static List<AaPolyline> CreateSinglePathPolylines(this IEnumerable<AaGeCurve> simpleCurves, ILogWriter logger)
        {
            // points found on more than two curves are on the edges of multi-paths 
            IEnumerable<AaPoint3d> pointsOnMultipleCurves = simpleCurves
                    .SelectMany((AaGeCurve curve) => new List<AaPoint3d>() { curve.StartPoint, curve.EndPoint })
                    .GroupBy(x => x.GetHashCode())  // rely on HashCode for point "equality"
                    .Where(g => g.Count() > 2)
                    .SelectMany(g => g)  // .Select (g => g.Select(v => v.Value)) ???
                    .Distinct()
                    .ToList();

            // join all curves without going through points above

            List<AaPolyline> joinedPolylines = new List<AaPolyline>();
            bool curveAdded;
            foreach (AaGeCurve curve in simpleCurves)
            {
                curveAdded = false;
                AaPolyline plAddedTo = null;
                foreach (AaPolyline pl in joinedPolylines)
                {
                    plAddedTo = pl;
                    curveAdded = pl.AddCurveIfAtEdge(curve, false, pointsOnMultipleCurves);
                    if (curveAdded) break;
                    curveAdded = pl.AddCurveIfAtEdge(curve, true, pointsOnMultipleCurves);
                    if (curveAdded) break;

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
                    AaPolyline newPL = AaPolyline.Create(curve, logger);
                    joinedPolylines.Add(newPL);
                }
            }
            return joinedPolylines;
        }

    }
}
