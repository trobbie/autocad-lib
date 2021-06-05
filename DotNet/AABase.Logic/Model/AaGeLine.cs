using System;
using OverlapResultSummary = AABase.Logic.AaGeCurveOverlapResult.SummaryType;

namespace AABase.Logic
{
    internal class AaGeLine : AaGeCurve
    {
        protected AaPoint3d _pt1;
        protected AaPoint3d _pt2;

        internal AaGeLine(AaPoint3d pt1, AaPoint3d pt2) : base(pt1, pt2)
        {
            Center = null;
            Radius = 0;
            StartAngle = 0;
            EndAngle = 0;
            PlaneNormal = null;

            _pt1 = pt1;
            _pt2 = pt2;
        }

        public override bool IsArc { get { return false; } }

        public override double Slope { get { 
            return (_pt2.X - _pt1.X).IsEqualTo(0) ?
                    Double.PositiveInfinity :
                    (_pt2.Y - _pt1.Y) / (_pt2.X - _pt1.X); 
        }}

        public override double Yintercept { get { 
            return (Slope == Double.PositiveInfinity) ?
                Double.NaN :
                (_pt2.Y - Slope * _pt2.X); 
        }}

        public override AaPoint3d StartPoint { get {
            return _pt1;
        }}

        public override AaPoint3d EndPoint { get {
            return _pt2;
        }}

        public override double Bulge { get {
            // Bulge factor is the tangent of one fourth the included angle for an arc segment, made negative if arc goes clockwise
            return 0;
        }}

        public override string ToString()
        {
            return $"[{base.GetHashCode()}:Line:{_pt1.ToString()},{_pt2.ToString()}]";
        }

        public override bool IsEqualTo(IGeCurve curve, bool ignorePointOrder)
        {
            if (!base.IsEqualTo(curve, ignorePointOrder)) return false;

            return (StartPoint.Equals(curve.StartPoint) && EndPoint.Equals(curve.EndPoint))
                    || (ignorePointOrder && _pt1.Equals(curve.EndPoint) && _pt2.Equals(curve.StartPoint));
        }

        public override int CompareTo(IGeCurve other)
        {
            // If other is not a valid object reference, this instance is greater.
            if (other == null) return 1;
            // if one is line and other is arc, put line first
            if (IsArc != other.IsArc) return other.IsArc ? 1 : 0;
            

            IGeCurve orderedCurve1 = this.GetCurveOrdered();
            IGeCurve orderedCurve2 = other.GetCurveOrdered();

            int result = orderedCurve1.StartPoint.CompareTo(orderedCurve2.StartPoint);
            if (result != 0) return result;
            return orderedCurve1.EndPoint.CompareTo(orderedCurve1.EndPoint);
        }

        public override IGeCurve GetCurveOrdered() {
            // an ordered line ensures that pt1's X is less than pt2's X, and if equal that the Y value is less
            if(_pt1.X.IsEqualTo(_pt2.X) || _pt1.X > _pt2.X)
            {
                if (_pt1.Y.IsEqualTo(_pt2.Y) || _pt1.Y < _pt2.Y)
                {
                    return this;
                } 
                else
                {
                    return AaGeCurve.Create(_pt2, _pt1);
                }
            } else
            {
                return this;
            }
        }
        public override IGeCurve AsReverseCurve()
        {
            return AaGeCurve.Create(EndPoint, StartPoint);
        }

        protected override bool OnSameInfiniteCurve(IGeCurve other)
        {
            if (Slope == Double.PositiveInfinity) // then vertical line
            {
                return (other.Slope == Double.PositiveInfinity)
                       && StartPoint.X.IsEqualTo(other.StartPoint.X);
            } 
            else
            {
                return Slope.IsEqualTo(other.Slope)
                       && Yintercept.IsEqualTo(other.Yintercept);
            }
        }

        public override bool ContainsPoint(AaPoint3d pt) { 
            // TODO: refactor code shared with AaLine
            if (Slope == Double.PositiveInfinity)
            {
                if (!pt.X.IsEqualTo(StartPoint.X)) return false;
                if (StartPoint.Y < EndPoint.Y)
                {
                    if ((pt.Y > StartPoint.Y) && (pt.Y < EndPoint.Y)) return true;
                }
                else
                {
                    if ((pt.Y > EndPoint.Y) && (pt.Y < StartPoint.Y)) return true;
                }
            }
            else if (pt.Y.IsEqualTo(Slope*pt.X + Yintercept))
            {
                // it is on "infinite" line, so now check if within domain
                if (pt.X.IsEqualTo(StartPoint.X) || pt.X.IsEqualTo(EndPoint.X)) return true;
                if (StartPoint.X < EndPoint.X)
                {
                    if ((pt.X > StartPoint.X) && (pt.X < EndPoint.X)) return true;
                }
                else
                {
                    if ((pt.X > EndPoint.X) && (pt.X < StartPoint.X)) return true;
                }
            }
            return false;
        }

        
        public override AaGeCurveOverlapResult FindOverlap(IGeCurve other)
        {
            AaGeCurveOverlapResult result = base.FindOverlap(other);

            if (!result.Summary.Equals(OverlapResultSummary.NotAccessed)) return result;
                        
            IGeCurve thisOrdered = this.GetCurveOrdered();
            IGeCurve otherOrdered = other.GetCurveOrdered();

            // lines' start/end points are now ordered; guarantee: start.x <= end.x, and if start.x=end.x, then start.y <= end.y
            double thisStartValue, thisEndValue, otherStartValue, otherEndValue;
            if (thisOrdered.Slope == Double.PositiveInfinity) // then they're vertical lines
            {
                thisStartValue = thisOrdered.StartPoint.Y;
                thisEndValue = thisOrdered.EndPoint.Y;
                otherStartValue = otherOrdered.StartPoint.Y;
                otherEndValue = otherOrdered.EndPoint.Y;
            }
            else
            {
                thisStartValue = thisOrdered.StartPoint.X;
                thisEndValue = thisOrdered.EndPoint.X;
                otherStartValue = otherOrdered.StartPoint.X;
                otherEndValue = otherOrdered.EndPoint.X;
            }

            if (thisStartValue.IsEqualTo(otherStartValue))
            {
                if (thisOrdered.ContainsPoint(otherOrdered.EndPoint))
                    return result.AssignResult(OverlapResultSummary.ContainsOther, other);
                else
                    return result.AssignResult(OverlapResultSummary.ContainedByOther, this);
            }
            if (thisStartValue < otherStartValue)
            {
                if (thisOrdered.ContainsPoint(otherOrdered.StartPoint))
                {
                    if (thisEndValue.IsEqualTo(otherStartValue))
                        return result.AssignResult(OverlapResultSummary.NoOverlap, null);
                    else if (thisOrdered.ContainsPoint(otherOrdered.EndPoint))
                        return result.AssignResult(OverlapResultSummary.ContainsOther, other);
                    else
                        return result.AssignResult(OverlapResultSummary.EndOverlapsOtherEnd, AaGeCurve.Create(otherOrdered.StartPoint, thisOrdered.EndPoint));
                }
                else
                {
                    return result.AssignResult(OverlapResultSummary.NoOverlap, null);
                }
            }
            // knowns: curves not equal,  this.start > other.start
            //if (thisStartValue > otherStartValue)
            {
                if (otherOrdered.ContainsPoint(thisOrdered.StartPoint))
                {
                    if (otherEndValue.IsEqualTo(thisStartValue))
                        return result.AssignResult(OverlapResultSummary.NoOverlap, null);
                    else if (otherOrdered.ContainsPoint(thisOrdered.EndPoint))
                        return result.AssignResult(OverlapResultSummary.ContainedByOther, this);
                    else
                        return result.AssignResult(OverlapResultSummary.EndOverlapsOtherEnd, AaGeCurve.Create(thisOrdered.StartPoint, otherOrdered.EndPoint));
                }
            } 

            return result.AssignResult(OverlapResultSummary.NoOverlap, null);
        }

    }
}