using AABase.Logic;

namespace AABase.Tests
{
    internal class FakeLineEntity : FakeCurveEntity
    {
        public FakeLineEntity(double startPointX, double startPointY, double endPointX, double endPointY)
          : base(startPointX, startPointY, endPointX, endPointY)
        {
        }

        public override string GetDxfName()
        {
            return "LINE";
        }
    }
}