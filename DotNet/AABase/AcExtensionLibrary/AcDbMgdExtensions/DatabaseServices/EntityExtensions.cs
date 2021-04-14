using AABase.Logic;

namespace Autodesk.AutoCAD.DatabaseServices
{
    public static class EntityExtensions
    {
        public static AaEntity AsAaEntity(this Entity e)
        {
            // Note: make sure more specific types are checked before parent types
            if (e is Arc) return new AaArc((Arc)e);
            else if (e is Polyline) return new AaPolyline((Polyline)e);
            else if (e is Curve) return new AaCurve((Curve)e);
            else if (e is Dimension) return new AaDimension((Dimension)e);
            else return new AaEntity(e);
        }
    }
}
