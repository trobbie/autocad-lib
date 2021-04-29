using System;

namespace AABase.Logic
{
    public class UserCancelledException : Exception
    {
        public UserCancelledException()
        {
        }
        public UserCancelledException(string message) : base(message)
        {
        }
        public UserCancelledException(string message, Exception inner) : base(message, inner)
        {
        }
    }

}