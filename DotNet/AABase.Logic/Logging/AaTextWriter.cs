using System.IO;
using System.Text;

namespace AABase.Logic.Logging
{
    public abstract class AaTextWriter : TextWriter
    {
        public override abstract void Write(char value);
        public override Encoding Encoding { get { return Encoding.UTF8; }}
    }
}