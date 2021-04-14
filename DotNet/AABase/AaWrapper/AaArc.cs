using Autodesk.AutoCAD.DatabaseServices;

namespace AABase.Logic
{
    public class AaArc : AaCurve, IArc
    {
        public AaArc(Arc arc) : base(arc) { }

        public AaArc(IEntity entity) : base((Arc)entity.GetAutocadEntity()) { }

        private Arc GetArc() { return (Arc)_dbobject; }
        public AaPoint3d Center { get { return GetArc().Center.GetPoint(); } }
        public double Radius { get { return GetArc().Radius; } }

    }

}
