using Autodesk.AutoCAD.DatabaseServices;

namespace AABase.Logic
{
    public class AaCircle : AaCurve, ICircle
    {
        public AaCircle(Circle arc) : base(arc) { }

        public AaCircle(IEntity entity) : base((Circle)entity.GetAcEntity()) { }

        private Circle GetCircle() { return (Circle)_dbobject; }
        public AaPoint3d Center { get { return GetCircle().Center.GetAaPoint(); } }
        public double Radius { get { return GetCircle().Radius; } }
        public AaPoint3d PlaneNormal { get { return new AaPoint3d(GetCircle().Normal.ToArray()); } }

    }

}
