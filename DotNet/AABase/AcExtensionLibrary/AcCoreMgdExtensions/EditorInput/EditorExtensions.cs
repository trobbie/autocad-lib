using Autodesk.AutoCAD.DatabaseServices;

namespace Autodesk.AutoCAD.EditorInput
{
    public static class EditorExtensions
    {
        /// <summary>
        /// Get a PromptSelectionResult from the active document's selection to
        /// be used as a target collection to be acted upon for other routines.
        /// </summary>
        /// <remarks>
        /// If any objects are already selected, return them; else ask user to select objects, returning that result.
        /// User is only asked once; if nothing is then selected, PromptSelectionResult's status will not be OK.
        /// </remarks>
        /// <returns>
        /// A PromptSelectionResult whose Value contains the SelectionSet of objects selected.  If no objects
        /// where selected, the Status is not OK.
        /// </returns>
        public static PromptSelectionResult GetTargetSelection(this Editor acDocEd)
        {
            // To get Pickfirst selection sets, the following conditions must hold true:
            // 1) PICKFIRST system variable must be set to 1
            // 2) UsePickSet command flag must be defined with the command that should use the Pickfirst selection set
            // 3) Call the SelectImplied method to obtain the PickFirst selection set
            // TODO: check conditions of the above
            int PickFirstValue = System.Convert.ToInt32(Application.GetSystemVariable("PICKFIRST"));
            if (!PickFirstValue.Equals(1))
            {
                Application.ShowAlertDialog($"PICKFIRST Autocad Variable set to {PickFirstValue.ToString()}\n" + 
                                            "Needs to be set to 1 for this command to work.");
            }

            // Get the PickFirst selection set
            PromptSelectionResult acSSPrompt;
            acSSPrompt = acDocEd.SelectImplied();

            // If the prompt status is OK, objects were selected
            if (acSSPrompt.Status == PromptStatus.OK)
            {
                return acSSPrompt;
            }
            else
            {
                // Clear the PickFirst selection set
                ObjectId[] idarrayEmpty = new ObjectId[0];
                acDocEd.SetImpliedSelection(idarrayEmpty);

                // Request for objects to be selected in the drawing area
                acSSPrompt = acDocEd.GetSelection();

                // If the prompt status is OK, objects were selected
                return acSSPrompt;
            }
        }
    }
}
