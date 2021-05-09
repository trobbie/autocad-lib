using Autodesk.AutoCAD.Geometry;
using System.Collections.Generic;
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
    }
}
