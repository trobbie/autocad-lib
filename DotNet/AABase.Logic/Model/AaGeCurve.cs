using System;
using System.Collections.Generic;
using System.Linq;
using OverlapResultSummary = AABase.Logic.AaGeCurveOverlapResult.SummaryType;

namespace AABase.Logic
{
    public abstract class AaGeCurve : IGeCurve
    {
        protected static IEnumerable<string> _supportedDxfNamesForCurveConversions = new List<string> { "LINE", "LWPOLYLINE", "ARC", "CIRCLE" };

        protected AaGeCurve(AaPoint3d pt1, AaPoint3d pt2)
        {
        }
        protected AaGeCurve(AaPoint3d center, double radius, double startAngle, double endAngle, AaPoint3d planeNormal)
        {
        }
        public static IGeCurve Create(AaPoint3d pt1, AaPoint3d pt2)
        {
            return new AaGeLine(pt1, pt2);
        }
        public static IGeCurve Create(AaPoint3d center, double radius, double startAngle, double endAngle, AaPoint3d planeNormal)
        {
            return new AaGeArc(center, radius, startAngle, endAngle, planeNormal);
        }

        public abstract bool IsArc { get; }
        public abstract AaPoint3d StartPoint { get; }
        public abstract AaPoint3d EndPoint { get; }
        public abstract double Bulge { get; }

        private class ValuesEqualityComparer : IEqualityComparer<IGeCurve>
        {
            public bool Equals(IGeCurve x, IGeCurve y)
            {
                return x.IsEqualTo(y, false);
            }
            public int GetHashCode(IGeCurve curve)
            {
                return curve.GetHashCode();
            }
        }
        public static IEqualityComparer<IGeCurve> EqualValuesComparer = new ValuesEqualityComparer();

        private class ValuesIgnoreOrderEqualityComparer : IEqualityComparer<IGeCurve>
        {
            public bool Equals(IGeCurve x, IGeCurve y)
            {
                return x.IsEqualTo(y, true);
            }
            public int GetHashCode(IGeCurve curve)
            {
                return curve.GetHashCode();
            }
        }
        public static IEqualityComparer<IGeCurve> EqualValuesIgnoreOrderComparer = new ValuesIgnoreOrderEqualityComparer();

        public virtual bool IsEqualTo(IGeCurve curve, bool ignorePointOrder)
        {
            if (curve is null) return false;
            if (!IsArc.Equals(curve.IsArc)) return false;
            return true;
        }

        public abstract int CompareTo(IGeCurve other);
        
        public abstract IGeCurve GetCurveOrdered();

        public abstract IGeCurve AsReverseCurve();

        protected abstract bool OnSameInfiniteCurve(IGeCurve other);

        /// <summary>
        /// Return all overlap results between this simple curve and list of curves.
        /// </summary>
        /// <remarks>
        /// Each result includes this curve, the other curve (unaltered), description and any overlapping curve portions.
        /// </remarks>
        public IEnumerable<AaGeCurveOverlapResult> FindOverlapResults(IEnumerable<IGeCurve> listCurves)
        {
            List<AaGeCurveOverlapResult> overlappingCurves = new List<AaGeCurveOverlapResult>();
            AaGeCurveOverlapResult result;
            foreach (AaGeCurve curve in listCurves)
            {
                if (curve == this) continue;
                // if overlaps this curve, then find the overlapping curve region and add to overlappingCurves
                result = this.FindOverlap(curve);
                if (!result.Summary.Equals(OverlapResultSummary.NoOverlap))
                {
                    overlappingCurves.Add(result);
                }                    
            }
            return overlappingCurves;
        }

        public virtual AaGeCurveOverlapResult FindOverlap(IGeCurve other)
        {
            AaGeCurveOverlapResult result = new AaGeCurveOverlapResult(this, other);
            if ((other is null) || !IsArc.Equals(other.IsArc)) return result.AssignResult(OverlapResultSummary.NoOverlap, null);

            if (this.IsEqualTo(other, true)) return result.AssignResult(OverlapResultSummary.Equals, this);
            if (!OnSameInfiniteCurve(other)) return result.AssignResult(OverlapResultSummary.NoOverlap, null);
            
            return result;
        }

        public static bool SupportsCurveConversions(string dxfName) 
        {
            return _supportedDxfNamesForCurveConversions.Contains(dxfName);
        }

        public static string EnumerableToString(IEnumerable<IGeCurve> curves)
        {
            return curves.Select(i => i.ToString())
                         .Aggregate("",(i,j) => i + "," + j);
        }

    }
}