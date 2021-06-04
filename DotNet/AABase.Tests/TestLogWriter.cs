using AABase.Logic;
using AABase.Logic.Logging;

namespace AABase.Tests
{
    public class TestLogWriter : ILogWriter
    {
        public void WriteLine(LogLevel logLevel, string message, params object[] parameters)
        {
            return; // do nothing
        }
    }
}