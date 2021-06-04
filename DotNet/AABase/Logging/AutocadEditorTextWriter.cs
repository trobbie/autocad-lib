
namespace AABase.Logic.Logging
{
    public class AutocadEditorTextWriter : AaTextWriter
    {
        public override void Write(char value)
        {
            Active.Write($"{value}");
        }
    }
}