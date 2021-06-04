using AABase.Logic.Logging;

namespace AABase.Logic
{

    public static class AaBaseLogic
    {
        // place globals here

        public static readonly AaLogWriter Logger = new AaLogWriter();

        // maximum precision of coordinates that we consider.  Equality checks uses this for fuzz factor.
        public const int MaxPointPrecision = 6;
    }

}
