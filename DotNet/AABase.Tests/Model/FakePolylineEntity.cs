using AABase.Logic;
using System.Collections.Generic;

namespace AABase.Tests
{
    internal class FakePolylineEntity : FakeEntity, IPolyline
    { 
        protected List<AaPoint3d> _listVertices = new List<AaPoint3d>();
        protected int _numOfVertices;
        protected bool _closed;

        public int NumberOfVertices { get { return _numOfVertices; }}
        public bool Closed { get { return _closed; }}

        public override string GetDxfName()
        {
            return "LWPOLYLINE";
        }
        public virtual AaPoint3d StartPoint => NumberOfVertices==0 ? null : _listVertices[0];

        public virtual AaPoint3d EndPoint => NumberOfVertices==0 ? null : GetPoint3dAt(NumberOfVertices-1);

        public AaPoint3d GetPoint3dAt(int vertexIndex) => _listVertices[vertexIndex];

        public AaGeCurve GetGeCurveAt(int vertexIndexStart) => throw new System.NotImplementedException();

        public IEnumerable<AaPoint3d> GetPointsOnExtentsOfSegmentAt(int vertexIndex) => throw new System.NotImplementedException();

        public virtual bool IsArcSegment(int vertexIndex) => false;
        public virtual bool IsLineSegment(int vertexIndex) => true;
    }
}