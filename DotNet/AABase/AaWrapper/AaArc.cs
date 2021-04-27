using Autodesk.AutoCAD.DatabaseServices;

namespace AABase.Logic
{
    public class AaArc : AaCurve, IArc
    {
        public AaArc(Arc arc) : base(arc) { }

        public AaArc(IEntity entity) : base((Arc)entity.GetAcEntity()) { }

        private Arc GetArc() { return (Arc)_dbobject; }
        public AaPoint3d Center { get { return GetArc().Center.GetAaPoint(); } }
        public double Radius { get { return GetArc().Radius; } }
        public double StartAngle { get { return GetArc().StartAngle; } }
        public double EndAngle { get { return GetArc().EndAngle; } }
        public AaPoint3d PlaneNormal { get { return new AaPoint3d(GetArc().Normal.ToArray()); } }

    }

}
