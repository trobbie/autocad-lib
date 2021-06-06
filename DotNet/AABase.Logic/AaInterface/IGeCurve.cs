using System;
using System.Collections.Generic;

namespace AABase.Logic
{
    public interface IGeCurve : IComparable<IGeCurve> //, IEquatable<AaGeCurve>
    {
        /// <summary>
        /// Is the curve an arc.  If not, it is a line.
        /// </summary>
        bool IsArc { get; }

        AaPoint3d StartPoint { get; }
        AaPoint3d EndPoint { get; }

        /// <summary>
        /// The slope of the curve, if a simple line. If an arc, return NaN
        /// </summary>
        double Slope { get; }
        /// <summary>
        /// The y-intercept of the curve, if a simple line. If an arc, return NaN
        /// </summary>
        double Yintercept { get; }
        double Bulge { get; }

        IGeCurve GetCurveOrdered();
        IGeCurve AsReverseCurve();

        bool IsEqualTo(IGeCurve curve, bool ignorePointOrder);

        bool ContainsPoint(AaPoint3d pt);
        IEnumerable<AaGeCurveOverlapResult> FindOverlapResults(IEnumerable<IGeCurve> listCurves);
        
        /// <summary>
        /// Find overlap results between this simple curve and another.  Results includes description and any overlapping curve portion.
        /// </summary>
        /// <remarks>
        /// The results also contain this curve and the other, unaltered.
        /// </remarks>
        AaGeCurveOverlapResult FindOverlap(IGeCurve other);

    }
}