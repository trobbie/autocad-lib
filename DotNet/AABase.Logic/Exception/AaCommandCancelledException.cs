using System;

namespace AABase.Logic
{
    public class AaCommandCancelledException : Exception
    {
        public AaCommandCancelledException()
        {
        }
        public AaCommandCancelledException(string message) : base(message)
        {
        }
        public AaCommandCancelledException(string message, Exception inner) : base(message, inner)
        {
        }
    }
}