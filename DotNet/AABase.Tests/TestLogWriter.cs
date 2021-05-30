using AABase.Logic;

namespace AABase.Tests
{
    public class TestLogWriter : ILogWriter
    {
        public void WriteLine(LogLevel logLevel, string message, params object[] parameter)
        {
            return; // do nothing
        }
    }
}