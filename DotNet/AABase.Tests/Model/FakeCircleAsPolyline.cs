using AABase.Logic;

namespace AABase.Tests
{
    internal class FakeCircleAsPolyline : FakePolylineEntity
    {
        public FakeCircleAsPolyline(AaPoint3d center, double radius)
        {
            _closed = false;
            _numOfVertices = 3;
            _listVertices.Add(new AaPoint3d(center.X, center.Y-radius));
            _listVertices.Add(new AaPoint3d(center.X, center.Y+radius));
            _listVertices.Add(new AaPoint3d(center.X, center.Y-radius));
        }
        public override bool IsArcSegment(int vertexIndex) => true;
        public override bool IsLineSegment(int vertexIndex) => false;
        
    }
}