using System.IO;

namespace AABase.Logic.Logging
{
    public interface ILogWriter
    {   
        /// <summary>
        /// Sends a log message using logLevel to designate its importance.
        /// </summary>
        /// <param name="logLevel">The log level of the message</param>
        /// <param name="message">The message to send.</param>
        /// <param name="parameter">The variables to substitute into the format string.</param>
         void WriteLine(LogLevel logLevel, string message, params object[] parameter);
    }
}