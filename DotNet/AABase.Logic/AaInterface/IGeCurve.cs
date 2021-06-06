using System;
using System.Collections.Generic;

namespace AABase.Logic
{
    /// <summary>
    /// Basic geometry curve.  A straight curve is a straight line.
    /// </summary>
    /// <remarks>
    /// For arcs, if looking down from plane's normal vector, arcs are always counter-clockwise.
    /// </remarks>
    public interface IGeCurve : IComparable<IGeCurve>
    {
        /// <summary>
        /// Is the curve an arc.  If not, it is a line.
        /// </summary>
        bool IsArc { get; }

        /// <summary>
        /// Start point of the curve.
        /// </summary>
        AaPoint3d StartPoint { get; }
        
        /// <summary>
        /// End point of the curve.
        /// </summary>
        AaPoint3d EndPoint { get; }

        /// <summary>
        /// Bulge factor between start and end points, as defined by Autocad definitions.  Value is 0 for lines.
        /// </summary>
        double Bulge { get; }

        /// <summary>
        /// Returns a curve representation that ensures the start point is "before" the end point.
        /// </summary>
        /// <remarks>
        /// This may be useful for readability in logs.
        /// </remarks>
        IGeCurve GetCurveOrdered();

        /// <summary>
        /// Returns the curve where start and end points are reversed.
        /// </summary>
        IGeCurve AsReverseCurve();

        /// <summary>
        /// Finds whether this and other curve is considered equal, considering fuzz tolereances.
        /// </summary>
        /// <param name="otherCurve">Other curve.</param>
        /// <param name="ignorePointOrder">If true, consider these equal if the other's reverse curve is equal</param>
        bool IsEqualTo(IGeCurve otherCurve, bool ignorePointOrder);

        /// <summary>
        /// Return the overlap results from testing this curve with a list of curves.
        /// </summary>
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