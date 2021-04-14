using System;
using AABase.Logic.AaInterface;

namespace Autodesk.AutoCAD.DatabaseServices
{
    /// <summary>
    /// 
    /// </summary>
    public static class Extents3dExtensions
    {
        public static Size3d GetSize(this Extents3d extents)
        {
            return new Size3d(
                extents.MaxPoint.X - extents.MinPoint.X,
                extents.MaxPoint.Y - extents.MinPoint.Y,
                extents.MaxPoint.Z - extents.MinPoint.Z);
        }

    }
}
