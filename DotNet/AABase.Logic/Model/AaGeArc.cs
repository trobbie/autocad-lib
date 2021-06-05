
using System;
using OverlapResultSummary = AABase.Logic.AaGeCurveOverlapResult.SummaryType;

namespace AABase.Logic
{
    internal class AaGeArc : AaGeCurve
    {
        internal AaGeArc(AaPoint3d center, double radius, double startAngle, double endAngle, AaPoint3d planeNormal)
            : base(center, radius, startAngle, endAngle, planeNormal)
        {
            Center = center;
            Radius = radius;
            StartAngle = (planeNormal.Z < 0) ? endAngle : startAngle;
            EndAngle = (planeNormal.Z < 0) ? startAngle + 2 * Math.PI : endAngle;
            // since flipping angles if negative-Z normal, also adjust the new plane normal
            PlaneNormal = (planeNormal.Z >= 0) ? planeNormal : new AaPoint3d(-planeNormal.X, -planeNormal.Y, -planeNormal.Z);
        }

        public override bool IsArc { get { return true; } }

        public override double Slope { get { 
            return Double.NaN;
        }}

        public override double Yintercept { get { 
            return Double.NaN;
        }}

        public override AaPoint3d StartPoint { get {
            return Center + new AaPoint3d(Radius * Math.Cos(StartAngle),
                                          Radius * Math.Sin(StartAngle),
                                          0);
        }}

        public override AaPoint3d EndPoint { get {
            return Center + new AaPoint3d(Radius * Math.Cos(EndAngle),
                                          Radius * Math.Sin(EndAngle),
                                          0);
        }}

        public override double Bulge { get {
            // Bulge factor is the tangent of one fourth the included angle for an arc segment, made negative if arc goes clockwise
            return Math.Tan((EndAngle - StartAngle)/4);
        }}

        public override string ToString()
        {
            return $"[{base.GetHashCode()}:Arc:{Center.ToString()},{Radius.ToString()},{StartAngle.ToString()},{EndAngle.ToString()}]";
        }

        public override bool IsEqualTo(IGeCurve curve, bool ignorePointOrder)
        {
            if (!base.IsEqualTo(curve, ignorePointOrder)) return false;

            return (Center.Equals(curve.Center)
                    && Radius.Equals(curve.Radius)
                    && (StartAngle.Equals(curve.StartAngle) && EndAngle.Equals(curve.EndAngle))
                    || (ignorePointOrder && StartAngle.Equals(curve.EndAngle) && EndAngle.Equals(curve.StartAngle)));
        }

        public override int CompareTo(IGeCurve other)
        {
            // If other is not a valid object reference, this instance is greater.
            if (other == null) return 1;
            // if one is line and other is arc, put line first
            if (IsArc != other.IsArc) return other.IsArc ? 1 : 0;

            IGeCurve orderedCurve1 = this.GetCurveOrdered();
            IGeCurve orderedCurve2 = other.GetCurveOrdered();

            int result = orderedCurve1.Center.CompareTo(orderedCurve2.Center);
            if (result != 0) return result;
            result = orderedCurve1.Radius.CompareTo(orderedCurve1.Radius);
            if (result != 0) return result;
            result = orderedCurve1.StartAngle.CompareTo(orderedCurve1.StartAngle);
            if (result != 0) return result;
            result = orderedCurve1.EndAngle.CompareTo(orderedCurve1.EndAngle);
            return result;
            
        }

        public override AaGeArc GetCurveOrdered() {
            // arc points are already ordered since flipping Start and End produces a different arc
            return this;  
        }

        public override AaGeArc AsReverseCurve()
        {
            return new AaGeArc(Center, Radius, EndAngle, StartAngle, PlaneNormal);
        }

        protected override bool OnSameInfiniteCurve(IGeCurve other)
        {
            return this.Center.Equals(other.Center) && this.Radius.Equals(other.Radius) && this.PlaneNormal.Equals(other.PlaneNormal);
        }

        public override bool ContainsPoint(AaPoint3d pt) { 
            // TODO: calculate for arcs
            return false;
        }

        /// <summary>
        /// Find overlap results between this simple curve and another.  Results includes description and any overlapping curve portion.
        /// </summary>
        /// <remarks>
        /// The results also contain this curve and the other, unaltered.
        /// </remarks>
        public override AaGeCurveOverlapResult FindOverlap(IGeCurve other)
        {
            AaGeCurveOverlapResult result = base.FindOverlap(other);

            if (!result.Equals(OverlapResultSummary.NotAccessed)) return result;
                        
            IGeCurve thisOrdered = this.GetCurveOrdered();
            IGeCurve otherOrdered = other.GetCurveOrdered();

            // arcs' center, radius, and normal all the same, but arcs are not equal
            // see overlap between StartAngle and EndAngle
            OverlapResultSummary summary = OverlapResultSummary.NoOverlap;
            if (StartAngle <= other.StartAngle)
            {
                if (EndAngle >= other.EndAngle)
                    summary = OverlapResultSummary.ContainsOther;
                else
                    summary = OverlapResultSummary.EndOverlapsOtherEnd;
            }
            else
            {
                if (other.EndAngle >= EndAngle)
                    summary = OverlapResultSummary.ContainedByOther;
                else
                    summary = OverlapResultSummary.EndOverlapsOtherEnd;
            }
            if (summary.Equals(OverlapResultSummary.NoOverlap))
                return result.AssignResult(OverlapResultSummary.NoOverlap, null);
            else
                return result.AssignResult(summary, 
                    AaGeCurve.Create(Center, Radius, Math.Max(StartAngle,other.StartAngle), Math.Min(EndAngle, other.EndAngle), PlaneNormal));

            return result.AssignResult(OverlapResultSummary.NoOverlap, null);
        }

    }
}