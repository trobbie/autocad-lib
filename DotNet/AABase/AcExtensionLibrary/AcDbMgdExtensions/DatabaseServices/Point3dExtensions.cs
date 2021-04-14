using Autodesk.AutoCAD.Geometry;
using System.Collections.Generic;
using System.Linq;
using AABase.Logic;

namespace Autodesk.AutoCAD.DatabaseServices
{
    public static class Point3dExtensions
    {
        public static AaPoint3d GetPoint(this Point3d ptAutocad)
        {
            return new AaPoint3d(ptAutocad.X, ptAutocad.Y, ptAutocad.Z);
        }
        public static IEnumerable<AaPoint3d> GetPoints(this Point3d[] ptArrayAutocad)
        {
            return ptArrayAutocad.Select(pt => pt.GetPoint());
        }
    }
}
