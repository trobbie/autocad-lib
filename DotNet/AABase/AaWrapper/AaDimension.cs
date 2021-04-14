using Autodesk.AutoCAD.DatabaseServices;

namespace AABase.Logic
{
    public class AaDimension : AaEntity, IDimension
    {
        public AaDimension(Dimension dim) : base(dim) { }
        private Dimension GetDimension() { return (Dimension)_dbobject; }

    }

}
