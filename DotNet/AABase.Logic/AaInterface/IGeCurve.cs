using System.Collections.Generic;

namespace AABase.Logic
{
    public interface IGeCurve
    {
        bool IsArc { get; }

        AaPoint3d StartPoint { get; }
        AaPoint3d EndPoint { get; }
        AaPoint3d Center { get; }
        double Radius { get; }
        double StartAngle { get; }
        double EndAngle { get; }
        AaPoint3d PlaneNormal { get; }

        double Slope { get; }
        double Yintercept { get; }
        double Bulge { get; }

        IGeCurve GetCurveOrdered();
        IGeCurve AsReverseCurve();

        bool IsEqualTo(IGeCurve curve, bool ignorePointOrder);
        int CompareTo(IGeCurve other);

        bool ContainsPoint(AaPoint3d pt);
        IEnumerable<AaGeCurveOverlapResult> FindOverlapResults(IEnumerable<IGeCurve> listCurves);
        AaGeCurveOverlapResult FindOverlap(IGeCurve other);
    }
}