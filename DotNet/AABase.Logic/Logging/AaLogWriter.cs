using AABase.Logic;
using System;
using System.Diagnostics;

namespace AABase
{
    public class AaLogWriter : ILogWriter
    {
        // TODO: move this to AutoCad environment variable
        private const LogLevel DebugLevel = LogLevel.Debug; 

        /// <summary>
        /// Sends a log message to the command line in the active Editor; ignore if supplied
        /// level is lower than global debug level.
        /// </summary>
        /// <param name="logLevel">The log level of the message</param>
        /// <param name="message">The message to send.</param>
        /// <param name="parameters">The variables to substitute into the format string.</param>
        public void WriteLine(LogLevel logLevel, string message, params object[] parameters)
        {
            if (logLevel < DebugLevel) return;
                
            string prefix = "";
            switch (logLevel)
            {
                case LogLevel.Trace:
                    prefix = "TRACE";
                    break;
                case LogLevel.Debug:
                    prefix = "DEBUG";
                    break;
                case LogLevel.Information:
                    prefix = "INFO";
                    break;
                case LogLevel.Warning:
                    prefix = "WARNING:";
                    break;
                case LogLevel.Error:
                    prefix = "ERROR";
                    break;
                case LogLevel.Critical:
                    prefix = "CRITICAL";
                    break;
                case LogLevel.None:
                    return;
            }
            Trace.WriteLine($"{prefix}: "+String.Format(message, parameters));
            // Active.WriteLine($"{prefix}: "+message, parameter);
        }
    }
}