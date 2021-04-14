using System;
using System.Collections.Generic;
using System.Linq;
using AABase;


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

    }
}
