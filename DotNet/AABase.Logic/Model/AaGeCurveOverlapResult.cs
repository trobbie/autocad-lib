using OverlapResultSummary = AABase.Logic.AaGeCurve.OverlapResultSummary;

namespace AABase.Logic
{
    public class AaGeCurveOverlapResult
    {
        public AaGeCurve ThisCurve { get; set; }
        public AaGeCurve OtherCurve { get; set; }
        public OverlapResultSummary Summary { get; set; }
        public AaGeCurve OverlapRegion { get; set; }

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
        
    }
}