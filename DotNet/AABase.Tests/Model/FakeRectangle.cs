using AABase.Logic;
using System.Collections.Generic;

namespace AABase.Tests
{
    internal class FakeRectangle : FakePolylineEntity
    {
        public FakeRectangle(AaPoint3d originBL, double width, double height, bool closed)
        {
            _closed = closed;
            _numOfVertices = _closed ? 4 : 5;
            _listVertices.Add(originBL);
            _listVertices.Add(new AaPoint3d(originBL.X+width, originBL.Y));
            _listVertices.Add(new AaPoint3d(originBL.X+width, originBL.Y+height));
            _listVertices.Add(new AaPoint3d(originBL.X, originBL.Y+height));
            if (!closed) {
                _listVertices.Add(originBL);
            }
        }
        public override bool IsArcSegment(int vertexIndex) => false;
        public override bool IsLineSegment(int vertexIndex) => true;
        
    }
}