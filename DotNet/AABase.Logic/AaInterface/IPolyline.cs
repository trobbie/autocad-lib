using AABase.Logic.Model;
using System;
using System.Collections.Generic;

namespace AABase.Logic.AaInterface
{
    public interface IPolyline : ICurve
    {
        int NumberOfVertices { get; }
        AaPoint3d GetPoint3dAt(int vertexIndex);

        bool Closed { get; }
        bool IsLineSegment(int vertexIndex);
        bool IsArcSegment(int vertexIndex);
        IEnumerable<AaPoint3d> GetPointsOnExtentsOfSegmentAt(int vertexIndex);
    }
}
