using Autodesk.AutoCAD.DatabaseServices;
using System;

namespace AABase.Logic
{
    public class AaEntity : AaDBObject, IEntity, IDBObject
    {
        public AaEntity(Entity entity) : base(entity) { }
        public object GetAcEntity() { return (Entity)_dbobject; }

        public IExtents3d GeometricExtents { get { return new AaExtents3d(((Entity)GetAcEntity()).GeometricExtents); } }

        public double GetLength() {
            if (_dbobject is Curve)
                return ((Curve)_dbobject).GetLength();
            else
                throw new NotImplementedException("Contact developer.  Did not implement for a supported class.");
        }

        public void SetLayer(string layerName)
        {
            ((Entity)_dbobject).Layer = layerName;
        }

    }
}
