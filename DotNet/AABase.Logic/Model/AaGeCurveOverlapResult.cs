using System;
using System.Collections.Generic;
using OverlapResultSummary = AABase.Logic.AaGeCurve.OverlapResultSummary;

namespace AABase.Logic
{
    public class AaGeCurveOverlapResult
    {
        public AaGeCurve ThisCurve { get; set; }
        public AaGeCurve OtherCurve { get; set; }
        public OverlapResultSummary Summary { get; set; }
        public AaGeCurve OverlapRegion { get; set; }

        public static IEqualityComparer<AaGeCurveOverlapResult> SameParentCurvesEqualityComparer { get; } = new _SameParentCurvesEqualityComparer();
        // Assumes thisCurve and otherCurve cannot be null
        public AaGeCurveOverlapResult(AaGeCurve thisCurve, AaGeCurve otherCurve)
        {
            ThisCurve =  thisCurve;
            OtherCurve = otherCurve;
        }

        public AaGeCurveOverlapResult AssignResult(OverlapResultSummary summary, AaGeCurve overlapRegion)
        {
            Summary = summary;
            OverlapRegion = overlapRegion;
            return this;
        } 
        
        private class _SameParentCurvesEqualityComparer : IEqualityComparer<AaGeCurveOverlapResult>
        {
            public bool Equals(AaGeCurveOverlapResult x, AaGeCurveOverlapResult y)
            {
                if (Object.ReferenceEquals(x, y)) return true;
                if (Object.ReferenceEquals(x, null) || Object.ReferenceEquals(y, null)) return false;

                // Note: if AaGeCurveOverlapResult is instantiated, then ThisCurve and OtherCurve assumed never null
                return ((x.ThisCurve == y.ThisCurve) && (x.OtherCurve == y.OtherCurve)
                        ||  (x.ThisCurve == y.OtherCurve) && (x.OtherCurve == y.ThisCurve));

            }
            public int GetHashCode(AaGeCurveOverlapResult x)
            {
                if (Object.ReferenceEquals(x, null)) return 0;

               // Note: if AaGeCurveOverlapResult is instantiated, then ThisCurve and OtherCurve assumed never null
                return (x.ThisCurve.GetHashCode() ^ x.OtherCurve.GetHashCode());
            }
        }
    }
}