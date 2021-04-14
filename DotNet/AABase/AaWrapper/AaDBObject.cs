using Autodesk.AutoCAD.DatabaseServices;

namespace AABase.Logic
{
    /*
     * Using adapter pattern to aid unit testing.
     */
    public class AaDBObject : IDBObject
    {
        protected readonly DBObject _dbobject;
        public AaDBObject(DBObject dbobject) { _dbobject = dbobject; }

        public object GetID() { return _dbobject.Id; }

        public string GetDxfName() { return _dbobject.Id.ObjectClass.DxfName; }
    }

}
