using System;
using System.Collections.Generic;

namespace AABase.Logic
{
    /// <summary>
    /// Basic geometry curve.  A straight curve is a straight line.
    /// </summary>
    /// <remarks>
    /// This assumes curve is on XY-plane, i.e. curve's plane normal is (0,0,1) or (0,0,-1).
    /// A (0,0,1) plane normal vector would produce a counter-clockwise arc.
    /// A (0,0,-1) plane normal vector would produce a clockwise arc.
    /// </remarks>
    public class AaGeCurve : IComparable<AaGeCurve> //, IEquatable<AaGeCurve>
    {
        /// <summary>
        /// Is the curve an arc.  If not, it is a line.
        /// </summary>
        /// <value></value>
        public bool IsArc { get; }

        private readonly AaPoint3d _pt1;
        private readonly AaPoint3d _pt2;

        /// <summary>
        /// The center point of curve.
        /// </summary>
        public AaPoint3d Center { get; }

        /// <summary>
        /// The radius of curve.
        /// </summary>
        public double Radius { get; }
        
        /// <summary>
        /// The start angle, in radians, (on unit circle) of the curve within the curve's plane
        /// </summary>
        /// <remarks>
        /// Arc is assumed to go counter-clockwise when viewed from it's plane's normal toward origin.
        /// </remarks>
        public double StartAngle { get; }
        /// <summary>
        /// The end angle, in radians, (on unit circle) of the curve within the curve's plane.
        /// </summary>
        /// <remarks>
        /// Arc is assumed to go counter-clockwise when viewed from it's plane's normal toward origin.
        /// </remarks>
        public double EndAngle { get; }

        /// <summary>
        /// The normal vector of the plane containing the curve
        /// </summary>
        public AaPoint3d PlaneNormal;

        public enum OverlapResultSummary
        {
            NoOverlap = 0,
            Equals,
            ContainsOther,
            ContainedByOther,
            EndOverlapsOtherEnd
        }

        /// <summary>
        /// The slope of the curve, if a simple line. If an arc, return NaN
        /// </summary>
        public double Slope { get { 
            if (IsArc) return Double.NaN;
            return (EndPoint.X - StartPoint.X).IsEqualTo(0) ?
                    Double.PositiveInfinity :
                    (EndPoint.Y - StartPoint.Y) / (EndPoint.X - StartPoint.X); 
        } }
        /// <summary>
        /// The y-intercept of the curve, if a simple line. If an arc, return NaN
        /// </summary>
        public double Yintercept { get { 
            if (IsArc) return Double.NaN;
            return (Slope == Double.PositiveInfinity) ?
                Double.NaN :
                (EndPoint.Y - Slope * EndPoint.X); 
        } }
        
        public AaGeCurve(AaPoint3d pt1, AaPoint3d pt2)
        {
            IsArc = false;
            Center = null;
            Radius = 0;
            StartAngle = 0;
            EndAngle = 0;
            PlaneNormal = null;
            _pt1 = pt1;
            _pt2 = pt2;
        }
        
        public AaGeCurve(AaPoint3d center, double radius, double startAngle, double endAngle, AaPoint3d planeNormal)
        {
            IsArc = true;
            Center = center;
            Radius = radius;
            StartAngle = (planeNormal.Z < 0) ? endAngle : startAngle;
            EndAngle = (planeNormal.Z < 0) ? startAngle : endAngle;
            // since flipping angles if negative-Z normal, also adjust the new plane normal
            PlaneNormal = (planeNormal.Z >= 0) ? planeNormal : new AaPoint3d(-planeNormal.X, -planeNormal.Y, -planeNormal.Z);
            _pt1 = null;
            _pt2 = null;
        }

        public AaPoint3d StartPoint { get {
            return !IsArc ? _pt1 : 
                Center + new AaPoint3d(Radius * Math.Cos(StartAngle),
                                          Radius * Math.Sin(StartAngle),
                                          0);
        }}

        public AaPoint3d EndPoint { get {
            return !IsArc ? _pt2 : 
                Center + new AaPoint3d(Radius * Math.Cos(EndAngle),
                                          Radius * Math.Sin(EndAngle),
                                          0);
        }}

        public AaGeCurve GetCurveOrdered() {
            if (IsArc)
            {
                // arc points are already ordered since flipping Start and End produces a different arc
                return this;  
            }
            else
            {
                // an ordered line ensures that pt1's X is less than pt2's X, and if equal that the Y value is less
                if(_pt1.X.IsEqualTo(_pt2.X) || _pt1.X > _pt2.X)
                {
                    if (_pt1.Y.IsEqualTo(_pt2.Y) || _pt1.Y < _pt2.Y)
                    {
                        return this;
                    } 
                    else
                    {
                        return new AaGeCurve(_pt2, _pt1);
                    }
                } else
                {
                    return this;
                }
            }
        }

        /*
        public override bool Equals(object obj) => Equals(obj as AaGeCurve);
        public override int GetHashCode()
        {
            return IsArc ? Center.GetHashCode() ^ Radius.GetHashCode() ^ StartAngle.GetHashCode() ^ EndAngle.GetHashCode()
                         : _pt1.GetHashCode() ^ _pt2.GetHashCode();
        }
        */
        public bool IsEqualTo(AaGeCurve curve)
        {
            return !(curve is null )
                && IsArc.Equals(curve.IsArc)
                && (IsArc || _pt1.Equals(curve._pt1))
                && (IsArc || _pt2.Equals(curve._pt2))
                && (!IsArc || Center.Equals(curve.Center))
                && (!IsArc || Radius.Equals(curve.Radius))
                && (!IsArc || StartAngle.Equals(curve.StartAngle))
                && (!IsArc || EndAngle.Equals(curve.EndAngle));
        }

        public int CompareTo(AaGeCurve other)
        {
            // If other is not a valid object reference, this instance is greater.
            if (other == null) return 1;
            AaGeCurve orderedCurve1 = this.GetCurveOrdered();
            AaGeCurve orderedCurve2 = other.GetCurveOrdered();
            // if one is line and other is arc, put line first
            if (IsArc != other.IsArc) return other.IsArc ? 1 : 0;
            if (IsArc)
            {
                int result = orderedCurve1.Center.CompareTo(orderedCurve2.Center);
                if (result != 0) return result;
                result = orderedCurve1.Radius.CompareTo(orderedCurve1.Radius);
                if (result != 0) return result;
                result = orderedCurve1.StartAngle.CompareTo(orderedCurve1.StartAngle);
                if (result != 0) return result;
                result = orderedCurve1.EndAngle.CompareTo(orderedCurve1.EndAngle);
                return result;
            } else
            {
                int result = orderedCurve1.StartPoint.CompareTo(orderedCurve2.StartPoint);
                 if (result != 0) return result;
                 return orderedCurve1.EndPoint.CompareTo(orderedCurve1.EndPoint);
            }
        }
        
        private bool OnSameInfiniteCurve(AaGeCurve other)
        {
            if (IsArc)
            {
                // TODO: implement for arcs
                return false;
            } 
            else if (Slope == Double.PositiveInfinity) // then vertical line
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

        public bool ContainsPoint(AaPoint3d pt) { 

            if (IsArc)
            {
                // TODO: calculate for arcs
                return false;
            }
            else
            {
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
        }

        public IEnumerable<AaGeCurveOverlapResult> FindOverlappingCurves(IEnumerable<AaGeCurve> listCurves)
        {
            List<AaGeCurveOverlapResult> overlappingCurves = new List<AaGeCurveOverlapResult>();
            AaGeCurveOverlapResult result;
            foreach (AaGeCurve curve in listCurves)
            {
                if (curve == this) continue;
                // if overlaps this curve, then find the overlapping curve region and add to overlappingCurves
                // TODO: find overlapping area
                result = this.Overlaps(curve);
                if (!result.Summary.Equals(OverlapResultSummary.NoOverlap))
                {
                    overlappingCurves.Add(result);
                }                    
            }
            return overlappingCurves;
        }

        public AaGeCurveOverlapResult Overlaps(AaGeCurve other)
        {
            AaGeCurveOverlapResult result = new AaGeCurveOverlapResult(this, other);
            if ((other is null) || !IsArc.Equals(other.IsArc)) return result.AssignResult(OverlapResultSummary.NoOverlap, null);

            if (this.IsEqualTo(other)) return result.AssignResult(OverlapResultSummary.Equals, this);

            if (IsArc)
            {
                // overlapping arcs must have same center, radius, and plane normal
                if (!this.Center.Equals(other.Center)) return result.AssignResult(OverlapResultSummary.NoOverlap, null);
                if (!this.Radius.Equals(other.Radius)) return result.AssignResult(OverlapResultSummary.NoOverlap, null);
                if (!this.PlaneNormal.Equals(other.PlaneNormal)) return result.AssignResult(OverlapResultSummary.NoOverlap, null);
                
            }
            else // is simple line
            {
                if (!OnSameInfiniteCurve(other)) return result.AssignResult(OverlapResultSummary.NoOverlap, null);
                
                AaGeCurve thisOrdered = this.GetCurveOrdered();
                AaGeCurve otherOrdered = other.GetCurveOrdered();
                // curves' start/end points are now ordered; guarantee: start.x <= end.x, and if start.x=end.x, then start.y <= end.y

                // Note: at this moment, curves are guaranteed not equal
                if (thisOrdered.Slope == Double.PositiveInfinity) // then they're vertical lines
                {
                    if (thisOrdered.StartPoint.Y.IsEqualTo(otherOrdered.StartPoint.Y))
                    {
                        if (thisOrdered.ContainsPoint(otherOrdered.EndPoint))
                            return result.AssignResult(OverlapResultSummary.ContainsOther, null);
                        else
                            return result.AssignResult(OverlapResultSummary.ContainedByOther, null);
                    }
                    if (thisOrdered.StartPoint.Y < otherOrdered.StartPoint.Y)
                    {
                        if (thisOrdered.ContainsPoint(otherOrdered.StartPoint))
                        {
                            if (thisOrdered.EndPoint.Y.IsEqualTo(otherOrdered.StartPoint.Y))
                                return result.AssignResult(OverlapResultSummary.NoOverlap, null);
                            else if (thisOrdered.ContainsPoint(otherOrdered.EndPoint))
                                return result.AssignResult(OverlapResultSummary.ContainsOther, null);
                            else
                                return result.AssignResult(OverlapResultSummary.EndOverlapsOtherEnd, null);
                        }
                        else
                        {
                            return result.AssignResult(OverlapResultSummary.NoOverlap, null);
                        }
                    }
                    // knowns: curves not equal,  this.start > other.start
                    if (otherOrdered.ContainsPoint(thisOrdered.StartPoint))
                    {
                        if (otherOrdered.EndPoint.Y.IsEqualTo(thisOrdered.StartPoint.Y))
                            return result.AssignResult(OverlapResultSummary.NoOverlap, null);
                        else if (otherOrdered.ContainsPoint(thisOrdered.EndPoint))
                            return result.AssignResult(OverlapResultSummary.ContainedByOther, null);
                        else
                            return result.AssignResult(OverlapResultSummary.EndOverlapsOtherEnd, null);
                    }
                    return result.AssignResult(OverlapResultSummary.NoOverlap, null);
                }
                if (thisOrdered.StartPoint.X.IsEqualTo(otherOrdered.StartPoint.X))
                {
                    if (thisOrdered.ContainsPoint(otherOrdered.EndPoint))
                        return result.AssignResult(OverlapResultSummary.ContainsOther, null);
                    else
                        return result.AssignResult(OverlapResultSummary.ContainedByOther, null);
                }
                if (thisOrdered.StartPoint.X < otherOrdered.StartPoint.X)
                {
                    if (thisOrdered.ContainsPoint(otherOrdered.StartPoint))
                    {
                        if (thisOrdered.EndPoint.X.IsEqualTo(otherOrdered.StartPoint.X))
                            return result.AssignResult(OverlapResultSummary.NoOverlap, null);
                        else if (thisOrdered.ContainsPoint(otherOrdered.EndPoint))
                            return result.AssignResult(OverlapResultSummary.ContainsOther, null);
                        else
                            return result.AssignResult(OverlapResultSummary.EndOverlapsOtherEnd, null);
                    }
                    else
                    {
                        return result.AssignResult(OverlapResultSummary.NoOverlap, null);
                    }
                }
                // knowns: curves not equal,  this.start > other.start
                //if (thisOrdered.StartPoint.X > otherOrdered.StartPoint.X)
                {
                    if (otherOrdered.ContainsPoint(thisOrdered.StartPoint))
                    {
                        if (otherOrdered.EndPoint.X.IsEqualTo(thisOrdered.StartPoint.X))
                            return result.AssignResult(OverlapResultSummary.NoOverlap, null);
                        else if (otherOrdered.ContainsPoint(thisOrdered.EndPoint))
                            return result.AssignResult(OverlapResultSummary.ContainedByOther, null);
                        else
                            return result.AssignResult(OverlapResultSummary.EndOverlapsOtherEnd, null);
                    }
                }
            }    


            return result.AssignResult(OverlapResultSummary.NoOverlap, null);
        }

        public override string ToString()
        {
            return IsArc
                ? $"[{base.GetHashCode()}:Arc:{Center.ToString()},{Radius.ToString()},{StartAngle.ToString()},{EndAngle.ToString()}]"
                : $"[{base.GetHashCode()}:Line:{_pt1.ToString()},{_pt2.ToString()}]";
        }

    }
}