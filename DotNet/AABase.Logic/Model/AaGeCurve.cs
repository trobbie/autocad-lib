using System;
using System.Collections.Generic;
using System.Linq;

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
        private static IEnumerable<string> _supportedDxfNamesForCurveConversions = new List<string> { "LINE", "LWPOLYLINE", "ARC", "CIRCLE" };

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
        
        protected AaGeCurve(AaPoint3d pt1, AaPoint3d pt2)
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
        
        protected AaGeCurve(AaPoint3d center, double radius, double startAngle, double endAngle, AaPoint3d planeNormal)
        {
            IsArc = true;
            Center = center;
            Radius = radius;
            StartAngle = (planeNormal.Z < 0) ? endAngle : startAngle;
            EndAngle = (planeNormal.Z < 0) ? startAngle + 2 * Math.PI : endAngle;
            // since flipping angles if negative-Z normal, also adjust the new plane normal
            PlaneNormal = (planeNormal.Z >= 0) ? planeNormal : new AaPoint3d(-planeNormal.X, -planeNormal.Y, -planeNormal.Z);
            _pt1 = null;
            _pt2 = null;
        }
        public static AaGeCurve Create(AaPoint3d pt1, AaPoint3d pt2)
        {
            return AaGeCurve.Create(pt1, pt2);
        }
        public static AaGeCurve Create(AaPoint3d center, double radius, double startAngle, double endAngle, AaPoint3d planeNormal)
        {
            return AaGeCurve.Create(center, radius, startAngle, endAngle, planeNormal);
        }

        private class ValuesEqualityComparer : IEqualityComparer<AaGeCurve>
        {
            public bool Equals(AaGeCurve x, AaGeCurve y)
            {
                return x.IsEqualTo(y, false);
            }
            public int GetHashCode(AaGeCurve curve)
            {
                return curve.GetHashCode();
            }
        }
        public static IEqualityComparer<AaGeCurve> EqualValuesComparer = new ValuesEqualityComparer();

        private class ValuesIgnoreOrderEqualityComparer : IEqualityComparer<AaGeCurve>
        {
            public bool Equals(AaGeCurve x, AaGeCurve y)
            {
                return x.IsEqualTo(y, true);
            }
            public int GetHashCode(AaGeCurve curve)
            {
                return curve.GetHashCode();
            }
        }
        public static IEqualityComparer<AaGeCurve> EqualValuesIgnoreOrderComparer = new ValuesIgnoreOrderEqualityComparer();

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

        public double Bulge { get {
            // Bulge factor is the tangent of one fourth the included angle for an arc segment, made negative if arc goes clockwise
            if (!IsArc) return 0;
            return Math.Tan((EndAngle - StartAngle)/4);
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
                        return AaGeCurve.Create(_pt2, _pt1);
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
        public bool IsEqualTo(AaGeCurve curve, bool ignorePointOrder)
        {
            if (curve is null) return false;
            if (!IsArc.Equals(curve.IsArc)) return false;
            if (IsArc)
            {
                return Center.Equals(curve.Center)
                    && Radius.Equals(curve.Radius)
                    && StartAngle.Equals(curve.StartAngle)
                    && EndAngle.Equals(curve.EndAngle)
                    && (!ignorePointOrder || StartAngle.Equals(curve.EndAngle))
                    && (!ignorePointOrder || EndAngle.Equals(curve.StartAngle));;
            }
            else
            {
                return _pt1.Equals(curve._pt1)
                    && _pt2.Equals(curve._pt2)
                    && (!ignorePointOrder || _pt1.Equals(curve._pt2))
                    && (!ignorePointOrder || _pt2.Equals(curve._pt1));
            }
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
        
        public AaGeCurve AsReverseCurve()
        {
            if (IsArc)
                return AaGeCurve.Create(Center, Radius, EndAngle, StartAngle, PlaneNormal);
            else
                return AaGeCurve.Create(EndPoint, StartPoint);
        }

        private bool OnSameInfiniteCurve(AaGeCurve other)
        {
            if (IsArc)
            {
                return this.Center.Equals(other.Center) && this.Radius.Equals(other.Radius) && this.PlaneNormal.Equals(other.PlaneNormal);
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
        }

        /// <summary>
        /// Return all overlap results between this simple curve and list of curves.
        /// </summary>
        /// <remarks>
        /// Each result includes this curve, the other curve (unaltered), description and any overlapping curve portions.
        /// </remarks>
        public IEnumerable<AaGeCurveOverlapResult> FindOverlapResults(IEnumerable<AaGeCurve> listCurves)
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

        /// <summary>
        /// Find overlap results between this simple curve and another.  Results includes description and any overlapping curve portion.
        /// </summary>
        /// <remarks>
        /// The results also contain this curve and the other, unaltered.
        /// </remarks>
        public AaGeCurveOverlapResult FindOverlap(AaGeCurve other)
        {
            AaGeCurveOverlapResult result = new AaGeCurveOverlapResult(this, other);
            if ((other is null) || !IsArc.Equals(other.IsArc)) return result.AssignResult(OverlapResultSummary.NoOverlap, null);

            if (this.IsEqualTo(other, true)) return result.AssignResult(OverlapResultSummary.Equals, this);
            if (!OnSameInfiniteCurve(other)) return result.AssignResult(OverlapResultSummary.NoOverlap, null);
            
            AaGeCurve thisOrdered = this.GetCurveOrdered();
            AaGeCurve otherOrdered = other.GetCurveOrdered();

            if (IsArc)
            {
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
            }
            else // is simple line
            {
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
            }    


            return result.AssignResult(OverlapResultSummary.NoOverlap, null);
        }

        public static bool SupportsCurveConversions(string dxfName) 
        {
            return _supportedDxfNamesForCurveConversions.Contains(dxfName);
        }

        public override string ToString()
        {
            return IsArc
                ? $"[{base.GetHashCode()}:Arc:{Center.ToString()},{Radius.ToString()},{StartAngle.ToString()},{EndAngle.ToString()}]"
                : $"[{base.GetHashCode()}:Line:{_pt1.ToString()},{_pt2.ToString()}]";
        }

        public static string EnumerableToString(IEnumerable<AaGeCurve> curves)
        {
            return curves.Select(i => i.ToString())
                         .Aggregate("",(i,j) => i + "," + j);
        }

    }
}