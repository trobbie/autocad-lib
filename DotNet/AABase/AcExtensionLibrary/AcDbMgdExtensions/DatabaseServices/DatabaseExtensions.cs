using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using AABase;
using AABase.Logic.AaInterface;
using Autodesk.AutoCAD.Runtime;

namespace Autodesk.AutoCAD.DatabaseServices
{
    public static class DatabaseExtensions
    {
        /// <summary>
        /// Perform action using transaction on the database.
        /// </summary>
        public static void UsingTransaction(this Database database,
            Func<Transaction, bool> action)
        {
            using (var tr = database.TransactionManager.StartTransaction())
            {
                try
                {
                    // Invoke the method
                    if (action(tr))
                        tr.Commit();
                    else
                        tr.Abort();
                } catch(Runtime.Exception ex)
                {
                    tr.Abort();
                    Active.Document.Editor.WriteMessage("\nError: {0}\nTrace: {1}", ex.Message, ex.StackTrace);
                }
            }
        }

        /// <summary>
        /// Perform function using transaction on the database, returning the
        /// list of instantiated DBObjects that is returned from function.
        /// </summary>
        public static List<T> UsingTransaction<T>(this Database database,
            Func<Transaction, IEnumerable<T>> func)
            where T : DBObject
        {
            List<T> listResult = null;

            using (var tr = database.TransactionManager.StartTransaction())
            {
                try
                {
                    // Invoke the method
                    IEnumerable<T> funcResult = func(tr);
                    listResult = funcResult.ToList<T>();
                    tr.Commit();
                }
                catch (Runtime.Exception ex)
                {
                    tr.Abort();
                    Active.Document.Editor.WriteMessage(ex.ToString());
                }

            }

            return listResult ?? new List<T>();
        }

        /// <summary>
        /// Perform action upon database's model space using a transaction.
        /// </summary>
        public static void UsingModelSpace(this Database database, Action<Transaction, BlockTableRecord> action)
        {
            database.UsingTransaction((Transaction tr) =>
            {
                // Get the block table for the current database
                var blockTable = (BlockTable)tr.GetObject(database.BlockTableId, OpenMode.ForRead);
                // Get the model space block table record
                var modelSpace = (BlockTableRecord)tr.GetObject(blockTable[BlockTableRecord.ModelSpace], OpenMode.ForRead);
                // Invoke the method
                action(tr, modelSpace);
                return true;
            });
        }

        /// <summary>
        /// Perform action upon all entities in the database's modelspace that match entity type T
        /// </summary>
        /// <typeparam name="T">Desired entity type for filtering</typeparam>
        public static void ForEach<T>(this Database database, Action<T> action) where T : Entity
        {
            database.UsingModelSpace((Transaction tr, BlockTableRecord modelSpace) =>
            {
                var entities = modelSpace.AllOfType<T>(tr);
                foreach (var entity in entities)
                {
                    action(entity);
                }
            });
        }

        /// <summary>
        /// Returns an enumerable of DBObject objects of type T found in this enumerable using a Autocad transaction.
        /// </summary>
        /// <typeparam name="T">must be subtype of DBObject</typeparam>
        /// <param name="enumerable">caller object (using class extension)</param>
        /// <param name="tr">transaction used for retrieving the DBObject objects</param>
        /// <returns>
        /// Enumerable of DBObject instances that are read-only.  Use UpgradeOpen() later to edit object.
        /// </returns>
        /// <remarks>
        /// Using Autodesk.AutoCAD.DatabaseServices namespace because this extension is desired whenever a Transaction is used.
        /// </remarks>
        public static IEnumerable<T> AllOfType<T>(this IEnumerable enumerable,
            Transaction tr)
            where T : DBObject
        {
            RXClass rxClass = RXObject.GetClass(typeof(T));
            return from ObjectId objectId in enumerable
                   where objectId.ObjectClass.IsDerivedFrom(rxClass)
                   select (T)tr.GetObject(objectId, OpenMode.ForRead);
        }

        /// <summary>
        /// Returns an enumerable of AaEntity objects requested by the given function, all instantiated.
        /// </summary>
        /// <remarks>
        /// The supplied function is responsible for specifying which Autocad Entities
        /// to create, returning a list.  But the database objects are not instantiated
        /// until after this function is run.
        /// </remarks>
        /// <param name="database">caller object (using class extension)</param>
        /// <param name="func">delegate function returning enumerable to Autocad Entity objects to be created in database</param>
        /// <typeparam name="T">AaEntity subclass that wraps func's returned entities</typeparam>

        public static IEnumerable<T> CreateEntities<T>(this Database database,
            Func<IEnumerable<Entity>> func)
            where T : AaEntity
        {
            List<Entity> entityList = database.UsingTransaction<Entity>((Transaction tr) => {
                BlockTable acBlkTbl = (BlockTable)tr.GetObject(database.BlockTableId, OpenMode.ForRead);
                BlockTableRecord modelspace = (BlockTableRecord)tr.GetObject(acBlkTbl[BlockTableRecord.ModelSpace], OpenMode.ForWrite);

                IEnumerable<Entity> requestedList = func();

                foreach (Entity e in requestedList)
                {
                    modelspace.AppendEntity(e);
                    tr.AddNewlyCreatedDBObject(e, true);
                }

                return requestedList;
            });


            // use toList() to create instances _now_, while transaction still open
            return entityList.Select(e => (T)Activator.CreateInstance(typeof(T), e)).ToList<T>();
        }

        /// <summary>
        /// Returns an objectId of a created Autocad entity after having run supplied action upon it
        /// </summary>
        /// <remarks>
        /// </remarks>
        /// <param name="database">caller object (using class extension)</param>
        /// <param name="action">delegate action that runs upon an Entity after it is created</param>
        /// <typeparam name="T">Entity subclass that is created and acted upon</typeparam>
        /// <author>Thanks to Scott McFarlane (2012)</author>

        public static ObjectId CreateObjectWithAction<T>(this Database database,
            Action<T> action)
            where T : Entity, new()
        {
            using (var tr = database.TransactionManager.StartTransaction())
            {
                try
                {
                    BlockTable acBlkTbl = (BlockTable)tr.GetObject(database.BlockTableId, OpenMode.ForRead);
                    BlockTableRecord modelspace = (BlockTableRecord)tr.GetObject(acBlkTbl[BlockTableRecord.ModelSpace], OpenMode.ForWrite);

                    var obj = new T();
                    obj.SetDatabaseDefaults();
                    action(obj);

                    var objectId = modelspace.AppendEntity(obj);
                    tr.AddNewlyCreatedDBObject(obj, true);
                    tr.Commit();

                    return objectId;
                }
                catch (System.Exception)
                {
                    tr.Abort();
                    throw;
                }
            }
        }

    }
}
