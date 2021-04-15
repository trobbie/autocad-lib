using Autodesk.AutoCAD.Geometry;
using System.Collections.Generic;
using System.Linq;

namespace AABase.Logic
{
    public static class AaPoint3dExtensions
    {
        public static Point3d GetAutocadPoint3d(this AaPoint3d ptAutocad)
        {
            return new Point3d(ptAutocad.AsArray());
        }

        public static IEnumerable<Point3d> GetAcPoints(this AaPoint3d[] ptArrayAutocad)
        {
            return ptArrayAutocad.Select(pt => pt.GetAutocadPoint3d());
        }
    }
}
