using Autodesk.AutoCAD.ApplicationServices;
using Autodesk.AutoCAD.DatabaseServices;
using Autodesk.AutoCAD.EditorInput;
using Application = Autodesk.AutoCAD.ApplicationServices.Core.Application;

namespace AABase
{
    /// <summary>
    /// Provides convenient access to several "active" objects in the AutoCAD runtime environment.
    /// </summary>
    /// <author>Autodesk University</author>
    public static class Active
    {
        private const int DebugLevel = 3; 

        /// <summary>
        /// Returns the active Editor object.
        /// </summary>
        public static Editor Editor
        {
            get { return Document.Editor; }
        }
        /// <summary>
        /// Returns the active Document object.
        /// </summary>
        public static Document Document
        {
            get { return Application.DocumentManager.MdiActiveDocument; }
        }
        /// <summary>
        /// Returns the active Database object.
        /// </summary>
        public static Database Database
        {
            get { return Document.Database; }
        }
        /// <summary>
        /// Sends a string to the command line in the active Editor
        /// </summary>
        /// <param name="message">The message to send.</param>
        public static void Write(string message)
        {
            Editor.WriteMessage(message);
        }

        /// <summary>
        /// Sends a string to the command line in the active Editor, followed by newline character
        /// </summary>
        /// <param name="message">The message to send.</param>
        public static void WriteLine(string message)
        {
            Editor.WriteMessage(message+"\n");
        }
        /// <summary>
        /// Sends a string to the command line in the active Editor using String.Format, followed by newline character
        /// </summary>
        /// <param name="message">The message containing format specifications.</param>
        /// <param name="parameter">The variables to substitute into the format string.</param>
        public static void WriteLine(string message, params object[] parameter)
        {
            Editor.WriteMessage(message+"\n", parameter);
        }

        /// <summary>
        /// Sends a debug string to the command line in the active Editor, if supplied
        /// level is lower than or equal to the global debug level.
        /// </summary>
        /// <param name="message">The message to send.</param>
        public static void WriteDebugMessage(int level, string message)
        {
            if (level <= DebugLevel)
                WriteLine("DEBUG"+level.ToString()+": "+message);
        }

        /// <summary>
        /// Sends a debug string to the command line in the active Editor, if supplied
        /// level is lower than or equal to the global debug level.
        /// </summary>
        /// <param name="message">The message to send.</param>
        /// <param name="parameter">The variables to substitute into the format string.</param>
        public static void WriteDebugMessage(int level, string message, params object[] parameter)
        {
            if (level <= DebugLevel)
                WriteLine("DEBUG"+level.ToString()+": "+message, parameter);
        }

    }
}
