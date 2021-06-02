using Autodesk.AutoCAD.Geometry;
using System.Collections.Generic;
using System.Linq;

namespace AABase.Logic
{
    public static class AaPoint3dExtensions
    {
        public static Point3d GetAcPoint3d(this AaPoint3d aaPoint3d)
        {
            return new Point3d(aaPoint3d.AsArray());
        }

        public static IEnumerable<Point3d> GetAcPoints(this AaPoint3d[] ptArrayAutocad)
        {
            return ptArrayAutocad.Select(pt => pt.GetAcPoint3d());
        }

        /// <summary>
        /// Get point as Autocad Point2d, ignoring z-value
        /// </summary>
        public static Point2d GetAcPoint2d(this AaPoint3d aaPoint3d)
        {
            return new Point2d(aaPoint3d.AsArray());
        }
    }
}
