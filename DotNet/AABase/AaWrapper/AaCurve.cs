using Autodesk.AutoCAD.DatabaseServices;

namespace AABase.Logic
{
    public class AaCurve : AaEntity, ICurve
    {
        public AaCurve(Curve curve) : base(curve) { }

        public AaCurve(IEntity entity) : base((Curve)entity.GetAcEntity()) { }

        private Curve GetCurve() { return (Curve)_dbobject; }

        public AaPoint3d StartPoint { get { return GetCurve().StartPoint.GetAaPoint(); } }

        public AaPoint3d EndPoint { get { return GetCurve().EndPoint.GetAaPoint(); } }

    }

}
