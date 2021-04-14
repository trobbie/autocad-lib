
namespace Autodesk.AutoCAD.DatabaseServices
{
    public static class TransactionExtensions
    {
        public static void EraseObject(this Transaction tr, ObjectId objectId)
        {
            DBObject objToErase = tr.GetObject(objectId, OpenMode.ForWrite);
            objToErase.Erase();
        }
    }
}
