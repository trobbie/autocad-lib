using AABase.Logic;

namespace AABase.Tests
{
    internal class FakeCurveEntity : FakeEntity, ICurve
    {
        private AaPoint3d StartPoint;
        
        private AaPoint3d EndPoint;
        protected FakeCurveEntity(double startPointX, double startPointY, double endPointX, double endPointY)
        {
            StartPoint = new AaPoint3d(startPointX, startPointY,0);
            EndPoint = new AaPoint3d(endPointX, endPointY,0);;
        }
        
        AaPoint3d ICurve.StartPoint => StartPoint;

        AaPoint3d ICurve.EndPoint => EndPoint;
    }
}