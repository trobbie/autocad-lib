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

        public object getID() { return _dbobject.Id; }

        public string getDxfName() { return _dbobject.Id.ObjectClass.DxfName; }
    }

}
