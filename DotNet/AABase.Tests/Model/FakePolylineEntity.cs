using AABase.Logic;
using System.Collections.Generic;

namespace AABase.Tests
{
    internal class FakePolylineEntity : FakeEntity, IPolyline
    {
        public int NumberOfVertices => throw new System.NotImplementedException();

        public bool Closed => throw new System.NotImplementedException();

        public AaPoint3d StartPoint => throw new System.NotImplementedException();

        public AaPoint3d EndPoint => throw new System.NotImplementedException();

        public AaPoint3d GetPoint3dAt(int vertexIndex)
        {
            throw new System.NotImplementedException();
        }

        public IEnumerable<AaPoint3d> GetPointsOnExtentsOfSegmentAt(int vertexIndex)
        {
            throw new System.NotImplementedException();
        }

        public bool IsArcSegment(int vertexIndex)
        {
            throw new System.NotImplementedException();
        }

        public bool IsLineSegment(int vertexIndex)
        {
            throw new System.NotImplementedException();
        }
    }
}