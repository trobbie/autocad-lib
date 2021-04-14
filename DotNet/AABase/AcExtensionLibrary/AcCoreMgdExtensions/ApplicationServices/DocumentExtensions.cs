using AABase.Logic.AaInterface;
using AABase.Logic.Model;
using System;
using System.Collections.Generic;
using System.Linq;
using Autodesk.AutoCAD.DatabaseServices;
using Autodesk.AutoCAD.EditorInput;


namespace Autodesk.AutoCAD.ApplicationServices.Core
{
    public static class DocumentExtensions
    {
        
        /// <summary>
        /// Perform action upon this document's modelspace.
        /// </summary>
        public static void UsingAllEntitiesOfType<T>(this Document acDoc,
            Action<IEnumerable<T>> action)
            where T : DBObject
        {
            acDoc.UsingAllEntitiesOfType<T>((IEnumerable<T> listA, Transaction tr) => {
                action(listA); // note: not passing the transaction parameter
            });
        }

        /// <summary>
        /// Perform action upon this document's modelspace.  Provide transaction as parameter.
        /// </summary>
        public static void UsingAllEntitiesOfType<T>(this Document acDoc,
            Action<IEnumerable<T>, Transaction> action)
            where T : DBObject
        {
            Database db = acDoc.Database;
            acDoc.Database.UsingTransaction((Transaction tr) =>
            {
                // open the block table which contains all the BlockTableRecords (block definitions and spaces)
                BlockTable blockTable = (BlockTable)tr.GetObject(db.BlockTableId, OpenMode.ForRead);

                // open the model space BlockTableRecord
                BlockTableRecord modelSpace = (BlockTableRecord)tr.GetObject(blockTable[BlockTableRecord.ModelSpace], OpenMode.ForRead);

                action(modelSpace.AllOfType<T>(tr), tr);
                return true;
            });
        }

        /// <summary>
        /// Perform action upon each entity in list, given a list of priorly pulled Autocad objects.
        /// </summary>
        /// 
        public static void UsingEntities<T>(this Document acDoc,
            IEnumerable<IEntity> entities,
            Func<Entity, Transaction, bool> action)
            where T : DBObject
        {
            Database db = acDoc.Database;
            acDoc.Database.UsingTransaction((Transaction tr) =>
            {
                bool doCommit = true;
                foreach (IEntity ent in entities)
                {
                    doCommit = doCommit && action((Entity)ent.GetAutocadEntity(), tr);
                    if (!doCommit) break;
                }
                return doCommit;
            });
        }

        /// <summary>
        /// Perform action upon this document's selection set.
        /// </summary>
        /// <remarks>
        /// Ask user for selection set if none currently selected in editor.
        /// </remarks>
        public static void UsingSelectionSet(this Document acDoc,
            Func<Transaction, SelectionSet, bool> action)
        {

            PromptSelectionResult acSSPrompt = acDoc.Editor.GetTargetSelection();
            if (acSSPrompt.Status == PromptStatus.OK)
            {
                acDoc.Database.UsingTransaction((Transaction tr) =>
                {
                    return action(tr, acSSPrompt.Value);
                });
            }
            // do nothing if nothing had been selected
        }

        public static IEnumerable<T> UsingSelectionSet<T>(this Document acDoc,
            Func<Transaction, SelectionSet, IEnumerable<T>> f)
            where T : DBObject
        {
            PromptSelectionResult acSSPrompt = acDoc.Editor.GetTargetSelection();
            if (acSSPrompt.Status == PromptStatus.OK)
            {
                return acDoc.Database.UsingTransaction<T>((Transaction tr) =>
                {
                    return f(tr, acSSPrompt.Value);
                });
            }
            // do nothing if nothing had been selected
            return new List<T>();
        }

        public static void UsingSelectionSetObjects<T>(this Document acDoc,
            Action<IEnumerable<T>> action)
            where T : DBObject
        {
            action(acDoc.UsingSelectionSet<T>((Transaction tr, SelectionSet ss) =>
            {
                return ss?.GetObjectIds().AllOfType<T>(tr);
            }));
        }
        
        /// <summary>
        /// Ask user to select a point and return the point coordinates.  Often used with snap points.
        /// </summary>
        /// <returns></returns>
        public static AaPoint3d GetSelectedPoint(this Document acDoc)
        {
            AaPoint3d ptSelected = null;
            bool cancel = false;

            while ((ptSelected == null) && !cancel)
            {
                PromptPointOptions ptOptions = new PromptPointOptions("")
                {
                    Message = "\nSelect a point on the polyline: "
                };
                PromptPointResult ptResult = acDoc.Editor.GetPoint(ptOptions);
                if (ptResult.Status == PromptStatus.Cancel)
                {
                    cancel = true;
                } else
                {
                    ptSelected = ptResult.Value.GetPoint();
                }
            }
            return ptSelected;
        }
        /// <summary>
        /// Return list of all objects in modelspace.
        /// </summary>
        /// Evalulates list immediately (no lazy initialization) so that Autocad interactions are ensured done within a transaction.
        /// <returns></returns>
        public static IEnumerable<IEntity> GetAllModelspaceObjects(this Document acDoc)
        {
            IEnumerable<IEntity> allObjects = null;
            acDoc.UsingAllEntitiesOfType<Entity>((IEnumerable<Entity> entities) =>
            {
                if (entities.Count() != 0)
                {
                    allObjects = entities.Select((ent) => ent.AsAaEntity()).ToList();
                }
            });
            return allObjects;
        }

        /// <summary>
        /// Return list of all selected objects.
        /// </summary>
        /// Evalulates list immediately (no lazy initialization) so that Autocad interactions are ensured done within a transaction.
        /// <returns></returns>
        public static IEnumerable<IEntity> GetSelectedSetObjects(this Document acDoc)
        {
            IEnumerable<IEntity> selectedObjects = null;
            acDoc.UsingSelectionSetObjects<Entity>((IEnumerable<Entity> entities) =>
            {
                if (entities.Count() != 0)
                {
                    selectedObjects = entities.Select((ent) => ent.AsAaEntity()).ToList();
                }
            });
            return selectedObjects;
        }


    }
}
