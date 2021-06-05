using System;
using System.Collections.Generic;

namespace AABase.Logic
{
    public interface IGeCurve : IComparable<IGeCurve> //, IEquatable<AaGeCurve>
    {
        bool IsArc { get; }

        AaPoint3d StartPoint { get; }
        AaPoint3d EndPoint { get; }
        
        /// <summary>
        /// The center point of curve.
        /// </summary>
        AaPoint3d Center { get; }
        /// <summary>
        /// The radius of curve.
        /// </summary>
        double Radius { get; }
         /// <summary>
        /// The start angle, in radians, (on unit circle) of the curve within the curve's plane
        /// </summary>
        /// <remarks>
        /// Arc is assumed to go counter-clockwise when viewed from it's plane's normal toward origin.
        /// </remarks>
        double StartAngle { get; }
        /// <summary>
        /// The end angle, in radians, (on unit circle) of the curve within the curve's plane.
        /// </summary>
        /// <remarks>
        /// Arc is assumed to go counter-clockwise when viewed from it's plane's normal toward origin.
        /// </remarks>
        double EndAngle { get; }
        
        AaPoint3d PlaneNormal { get; }

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