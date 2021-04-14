using Autodesk.AutoCAD.DatabaseServices;
using System;

namespace AABase.Logic.AaInterface
{
    public class AaEntity : AaDBObject, IEntity, IDBObject
    {
        public AaEntity(Entity entity) : base(entity) { }
        public object GetAutocadEntity() { return (Entity)_dbobject; }

        public IExtents3d GeometricExtents { get { return new AaExtents3d(((Entity)GetAutocadEntity()).GeometricExtents); } }

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
